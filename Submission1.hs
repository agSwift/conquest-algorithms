
{-#  OPTIONS_GHC -Wall  #-}
{-#  OPTIONS_GHC -Wno-unused-matches  #-}
{-#  OPTIONS_GHC -Wno-name-shadowing  #-}
{-#  OPTIONS_GHC -Wno-incomplete-patterns  #-}

{-#  LANGUAGE DeriveGeneric  #-}
{-#  LANGUAGE FlexibleInstances  #-}
{-#  LANGUAGE ScopedTypeVariables  #-}
{-#  LANGUAGE FunctionalDependencies  #-}
{-#  LANGUAGE GeneralizedNewtypeDeriving  #-}
{-#  LANGUAGE StandaloneDeriving  #-}
{-#  LANGUAGE InstanceSigs  #-}
{-#  LANGUAGE UndecidableInstances  #-}
{-#  LANGUAGE TypeApplications  #-}

module Submission1 where

import Prelude hiding (maximum)

import Data.Maybe (fromJust)

import Control.DeepSeq
import Data.Coerce (coerce)

import Data.Array
import Data.List (unfoldr, maximumBy, nub, sortBy, (\\))
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Binary as B
import           GHC.Generics

data Player = Player1 | Player2

data Planet = Planet Owner Ships Growth
newtype Ships = Ships Int
newtype Growth = Growth Int

data Owner = Neutral | Owned Player

newtype PlanetId = PlanetId Int

type Planets = Map PlanetId Planet

data Wormhole = Wormhole Source Target Turns

newtype Source = Source PlanetId
newtype Target = Target PlanetId
newtype Turns  = Turns Int

newtype WormholeId = WormholeId Int

type Wormholes = Map WormholeId Wormhole

data Fleet = Fleet Player Ships WormholeId Turns

type Fleets = [Fleet]

data GameState = GameState Planets Wormholes Fleets

data Order = Order WormholeId Ships

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fib' :: Int -> Integer
fib' n = table ! n
  where
    table :: Array Int Integer
    table = tabulate (0, n) mfib

    mfib 0 = 0
    mfib 1 = 1
    mfib n = table ! (n-1) + table ! (n-2)

tabulate :: Ix i => (i,i) -> (i -> a) -> Array i a
tabulate (u,v) f = array (u,v) [ (i, f i) | i <- range (u, v)]

example1 :: GameState
example1 = GameState planets wormholes fleets where
  planets = M.fromList
    [ (PlanetId 0, Planet (Owned Player1) (Ships 300) (Growth 0))
    , (PlanetId 1, Planet Neutral         (Ships 200) (Growth 50))
    , (PlanetId 2, Planet Neutral         (Ships 150) (Growth 10))
    , (PlanetId 3, Planet Neutral         (Ships 30)  (Growth 5))
    , (PlanetId 4, Planet Neutral         (Ships 100) (Growth 20))
    ]
  wormholes = M.fromList
    [ (WormholeId 0, Wormhole homePlanet (Target 1) (Turns 1))
    , (WormholeId 1, Wormhole homePlanet (Target 2) (Turns 1))
    , (WormholeId 2, Wormhole homePlanet (Target 3) (Turns 1))
    , (WormholeId 3, Wormhole homePlanet (Target 4) (Turns 1))
    ] where homePlanet = Source 0
  fleets = []

targetPlanets :: GameState -> Source -> [(PlanetId, Ships, Growth)]
targetPlanets st s
  = map (planetDetails . target) (M.elems (wormholesFrom s st))
  where
    planetDetails :: PlanetId -> (PlanetId, Ships, Growth)
    planetDetails pId = (pId, ships, growth)
      where Planet _ ships growth = lookupPlanet pId st

shipsOnPlanet :: GameState -> PlanetId -> Ships
shipsOnPlanet st pId = ships
  where Planet _ ships _ = lookupPlanet pId st

lookupPlanet :: PlanetId -> GameState -> Planet
lookupPlanet pId (GameState ps _ _) = fromJust (M.lookup pId ps)

wormholesFrom :: Source -> GameState -> Wormholes
wormholesFrom pId (GameState _ ws _)
  = M.filter (\(Wormhole s _ _) -> s == pId) ws

wormholesTo :: Target -> GameState -> Wormholes
wormholesTo pId (GameState _ ws _)
  = M.filter (\(Wormhole _ t _) -> t == pId) ws

knapsack :: forall name weight value. (Ord weight, Num weight, Ord value, Num value) =>
  [(name, weight, value)] -> weight -> value
knapsack wvs c = maximum 0 [ v + knapsack wvs (c - w) | (_,w,v) <- wvs , w <= c ]

maximum :: Ord a => a -> [a] -> a
maximum x xs = foldr max x xs

knapsack' :: forall name weight value .
  (Ix weight, Num weight, Ord value, Num value) =>
  [(name, weight, value)] -> weight -> value
knapsack' wvs c = table ! c
  where
    table :: Array weight value
    table = tabulate (0,c) mknapsack

    mknapsack :: weight -> value
    mknapsack c = maximum 0 [ v + table ! (c - w) | (_,w,v) <- wvs , w <= c ]

knapsack''
  :: forall name weight value
  . (Ix weight, Num weight, Ord value, Num value)
  => [(name, weight, value)] -> weight -> (value, [name])
knapsack'' wvs c = table ! c
  where
    table :: Array weight (value, [name])
    table = tabulate (0,c) mknapsack

    mknapsack :: weight -> (value, [name])
    mknapsack c
       = maximumBy(\i j -> compare (fst i) (fst j)) list
       where
         list = (0, []) : [(val + v, n : ns)
          | (n,w,v) <- wvs , w <= c , let (val, ns) = table ! (c - w)]

bknapsack
  :: forall name weight value
  . (Ord weight, Num weight, Ord value, Num value)
  => [(name, weight, value)] -> weight -> (value, [name])
bknapsack [] _ = (0, [])
bknapsack ((n, w, v) : wvs) c
  | w > c     = (nV, nNs)
  | otherwise = maxBy fst (incV + v, n : incNs) (nV, nNs)
  where
    (incV, incNs) = bknapsack wvs (c - w)
    (nV, nNs)     = bknapsack wvs c

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy f x y = case compare (f x) (f y) of
    GT -> x
    _  -> y

bknapsack' :: forall name weight value .
  (Ord weight, Num weight, Ord value, Num value) =>
  [(name, weight, value)] -> Int ->
  weight -> (value, [name])
bknapsack' [] _ _ = (0, [])
bknapsack' _ 0 _  = (0, [])
bknapsack' wvs idx c
  | w > c     = (nV, nNs)
  | otherwise = maxBy fst (incV + v, n : incNs) (nV, nNs)
  where
    (n, w, v)     = wvs !! (idx - 1)
    (incV, incNs) = bknapsack' wvs (idx - 1) (c - w)
    (nV, nNs)     = bknapsack' wvs (idx - 1) c

bknapsack'' :: forall name weight value .
  (Ord name, Ix weight, Ord weight, Num weight,
    Ord value, Num value) =>
  [(name, weight, value)] -> weight -> (value, [name])
bknapsack'' wvs c = table ! (c, length wvs)
  where
    table :: Array (weight, Int) (value, [name])
    table = tabulate ((0, 0), (c, length wvs)) mbknapsack

    mbknapsack :: (weight, Int) -> (value, [name])
    mbknapsack (0, _) = (0, [])
    mbknapsack (_, 0) = (0, [])
    mbknapsack (c, idx)
      | w > c     = (nV, nNs)
      | otherwise = maxBy fst (incV + v, n : incNs) (nV, nNs)
      where
        (n, w, v)     = wvs !! (idx - 1)
        (incV, incNs) = table ! (c - w, idx - 1)
        (nV, nNs)     = table ! (c, idx - 1)



optimise :: GameState -> Source -> (Growth, [PlanetId])
optimise st s@(Source p)
  = bknapsack'' (targetPlanets st s) (shipsOnPlanet st p)

type Weight = Integer

class Eq v => Edge e v | e -> v where
  source :: e -> v
  target :: e -> v
  weight :: e -> Weight

instance Edge (String, String, Integer) String where
  source (s, _, _) = s
  target (_, t, _) = t
  weight (_, _, i) = i

instance Edge Wormhole PlanetId where
  source (Wormhole (Source s) _ _)    = s
  target (Wormhole _ (Target t) _)    = t
  weight (Wormhole _ _ (Turns turns)) = toInteger turns

instance Edge (WormholeId, Wormhole) PlanetId where
  source (_, w) = source w
  target (_, w) = target w
  weight (_, w) = weight w

data Path e = Path Weight [e]

pathFromEdge :: Edge e v => e -> Path e
pathFromEdge e = Path (weight e) [e]

extend :: Edge e v => Path e -> e -> Path e
extend (Path _ []) _ = error "extend: Empty path"
extend (Path d (e:es)) e'
  | target e == source e' = Path (d + weight e') (e':e:es)
  | otherwise = error "extend: Incompatible endpoints"

pathFromEdges :: Edge e v => [e] -> Path e
pathFromEdges (x : xs) = foldl extend (pathFromEdge x) xs
pathFromEdges [] = error "pathFromEdges: Empty list of edges"

instance Edge e v => Edge (Path e) v where
  source (Path _ es) = source (last es)
  target (Path _ es) = target (head es)
  weight (Path w _)  = w

class Edge e v => Graph g e v | g -> e where
  vertices  :: g -> [v]
  edges     :: g -> [e]
  edgesFrom :: g -> v -> [e]
  edgesTo   :: g -> v -> [e]
  velem     :: v -> g -> Bool
  eelem     :: e -> g -> Bool

instance (Eq e, Edge e v) => Graph [e] e v where
  vertices es = nub (map source es ++ map target es)
  edges es    = es
  edgesFrom es v = [ e | e <- es, v == source e ]
  edgesTo   es v = [ e | e <- es, v == target e ]
  velem v es = v `elem` vertices es
  eelem v es = v `elem` edges es

example2 :: [(String, String, Integer)]
example2 = [("s","t",10), ("s","y",5), ("t","x",1), ("t","y",2), ("y","t",3),
            ("y","x", 9), ("x","z",4), ("z","x",6), ("y","z",2), ("z","s",7)]

instance Graph GameState (WormholeId, Wormhole) PlanetId where
  vertices (GameState ps _ _) = M.keys ps
  edges    (GameState _ ws _) = M.assocs ws
  edgesTo   st pId = M.toList (wormholesTo (Target pId) st)
  edgesFrom st pId = M.toList (wormholesFrom (Source pId) st)
  velem pId      (GameState ps _ _) = M.member pId ps
  eelem (wId, _) (GameState _ ws _) = M.member wId ws

lt :: (a -> a -> Ordering) -> (a -> a -> Bool)
lt cmp x y = cmp x y == LT

gt :: (a -> a -> Ordering) -> (a -> a -> Bool)
gt cmp x y = cmp x y == GT

lte :: (a -> a -> Ordering) -> (a -> a -> Bool)
lte cmp x y = cmp x y /= GT

eq :: (a -> a -> Ordering) -> (a -> a -> Bool)
eq cmp x y = cmp x y == EQ

class PQueue pqueue where
  toPQueue   :: (a -> a -> Ordering) -> [a] -> pqueue a
  toPQueue cmp xs = foldr insert (empty cmp) xs

  fromPQueue :: pqueue a -> [a]
  fromPQueue = unfoldr unqueue
      where
        unqueue q
          | isEmpty q = Nothing
          | otherwise = Just (detach q)

  priority :: pqueue a -> (a -> a -> Ordering)

  empty :: (a -> a -> Ordering) -> pqueue a
  isEmpty :: pqueue a -> Bool

  insert :: a -> pqueue a -> pqueue a

  extract :: pqueue a -> a
  discard :: pqueue a -> pqueue a
  detach  :: pqueue a -> (a, pqueue a)
  detach q = (extract q, discard q)

data PList a = PList (a -> a -> Ordering) [a]

instance PQueue PList where

  toPQueue cmp xs = PList cmp (sortBy cmp xs)

  fromPQueue (PList _ xs) = xs

  empty cmp = PList cmp []

  isEmpty (PList _ xs) = null xs

  priority (PList cmp _) = cmp

  insert x (PList cmp []) = PList cmp [x]
  insert x ps@(PList cmp _)
    | x <= y    = cons x ps
    | otherwise = cons y (insert x ys)
    where (<=) = lte cmp
          (y, ys) = detach ps
          cons x (PList cmp xs) = PList cmp (x:xs)

  extract (PList _ (x:_)) = x

  discard (PList cmp (_:xs)) = PList cmp xs

cmpPath :: Path v -> Path v -> Ordering
cmpPath (Path d _) (Path d' _) = compare d d'

shortestPaths :: forall g e v. Graph g e v => g -> v -> [Path e]
shortestPaths g v = dijkstra g (vertices g \\ [v]) ps
 where
  ps :: PList (Path e)
  ps = toPQueue cmpPath (map pathFromEdge (edgesFrom g v))

example3 :: GameState
example3 = GameState planets wormholes fleets where
  planets = M.fromList
    [ (PlanetId 0, Planet (Owned Player1) (Ships 300) (Growth 0))
    , (PlanetId 1, Planet Neutral         (Ships 200) (Growth 50))
    , (PlanetId 2, Planet Neutral         (Ships 150) (Growth 10))
    , (PlanetId 3, Planet Neutral         (Ships 30)  (Growth 5))
    , (PlanetId 4, Planet Neutral         (Ships 100) (Growth 20))
    , (PlanetId 5, Planet Neutral         (Ships 100) (Growth 20))
    ]
  wormholes = M.fromList
    [ (WormholeId 0, Wormhole homePlanet (Target 1) (Turns 1))
    , (WormholeId 1, Wormhole homePlanet (Target 2) (Turns 2))
    , (WormholeId 2, Wormhole homePlanet (Target 3) (Turns 3))
    , (WormholeId 3, Wormhole homePlanet (Target 4) (Turns 4))
    , (WormholeId 4, Wormhole (Source 4) (Target 5) (Turns 1))
    , (WormholeId 5, Wormhole (Source 2) (Target 5) (Turns 1))
    ] where homePlanet = Source 0
  fleets = []

dijkstra :: forall g e v pqueue.
  (Graph g e v, PQueue pqueue) =>
  g -> [v] -> pqueue (Path e) -> [Path e]
dijkstra _ [] _ = []
dijkstra g us ps
  | isEmpty ps  = []
  | t `elem` us =
      let us' :: [v]
          us' = us \\ [t]
          ps'' :: pqueue (Path e)
          ps'' = toPQueue cmpPath
                 (foldr (updatePath . extend p)
                         (fromPQueue ps') (edgesFrom g t))
      in p : dijkstra g us' ps''
  | otherwise  = dijkstra g us ps'
  where
    (p, ps') = detach ps
    t = target p
    updatePath :: Path e -> [Path e] -> [Path e]
    updatePath path [] = [path]
    updatePath path pss@(p : ps)
      | target path == target p && weight path < weight p = path : ps
      | target path == target p && weight path > weight p = pss
      | otherwise = p : updatePath path ps

data Heap a = Heap (a -> a -> Ordering) (Tree a)
data Tree a = Nil | Node Int (Tree a) a (Tree a)

rankTree :: Tree a -> Int
rankTree Nil            = 0
rankTree (Node h l x r) = h

rankHeap :: Heap a -> Int
rankHeap (Heap _ t) = rankTree t

node :: Tree a -> a -> Tree a -> Tree a
node l x r
  | hl < hr   = Node (hl + 1) r x l
  | otherwise = Node (hr + 1) l x r
 where
  hl = rankTree l
  hr = rankTree r

mergeHeap :: Heap a -> Heap a -> Heap a
mergeHeap (Heap cmp l) (Heap _ r) = Heap cmp (mergeTree cmp l r)

mergeTree :: (a -> a -> Ordering) -> Tree a -> Tree a -> Tree a
mergeTree _ Nil r = r
mergeTree _ l Nil = l
mergeTree cmp l@(Node _ l1 x1 r1) r@(Node _ l2 x2 r2)
  | lte cmp x1 x2  = node (mergeTree cmp r1 r) x1 l1
  | otherwise = node (mergeTree cmp l r2) x2 l2

instance PQueue Heap where
  priority :: Heap a -> (a -> a -> Ordering)
  priority (Heap cmp t) = cmp

  empty :: (a -> a -> Ordering) -> Heap a
  empty p = Heap p Nil

  isEmpty :: Heap a -> Bool
  isEmpty (Heap _ Nil) = True
  isEmpty (Heap _ _)   = False

  insert :: a -> Heap a -> Heap a
  insert a h@(Heap cmp _) = mergeHeap h (Heap cmp (Node 1 Nil a Nil))

  extract :: Heap a -> a
  extract (Heap _ (Node _ _ a _)) = a

  discard :: Heap a -> Heap a
  discard (Heap cmp Nil) = Heap cmp Nil
  discard (Heap cmp (Node _ l _ r)) = Heap cmp (mergeTree cmp l r)

shortestPaths' :: forall g e v . Graph g e v => g -> v -> [Path e]
shortestPaths' g v = dijkstra g (vertices g \\ [v]) ps
 where
  ps :: Heap (Path e)
  ps = foldr insert (empty cmpPath) (map pathFromEdge (edgesFrom g v))

newtype AdjList e v = AdjList [(v, [e])]

instance (Eq e, Edge e v) => Graph (AdjList e v) e v where
  vertices (AdjList ves)    = nub (map fst ves)
  edges (AdjList ves)       = nub (concatMap snd ves)
  edgesFrom (AdjList ves) s = [e | e <- edges (AdjList ves), s == source e]
  edgesTo (AdjList ves) t   = [e | e <- edges (AdjList ves), t == target e]
  velem v (AdjList ves)     = v `elem` vertices (AdjList ves)
  eelem e (AdjList ves)     = e `elem` edges (AdjList ves)

conflictZones :: GameState -> PlanetId -> PlanetId
  -> ([PlanetId], [PlanetId], [PlanetId])
conflictZones st p q = searchConflictZones sortedPathsP ([], [], []) sortedPathsQ
  where
    shortestPathsP = shortestPaths' st p
    shortestPathsQ = shortestPaths' st q
    sortedPathsP   = sortBy compareTarget shortestPathsP
    sortedPathsQ   = sortBy compareTarget shortestPathsQ

compareTarget :: (Ord a, Edge e a) => Path e -> Path e -> Ordering
compareTarget t t' = compare (target t) (target t')

searchConflictZones :: [Path (WormholeId, Wormhole)] -> ([PlanetId], [PlanetId], [PlanetId])
  -> [Path (WormholeId, Wormhole)] -> ([PlanetId], [PlanetId], [PlanetId])
searchConflictZones [] (ps, qs, pqs) []     = (ps, qs, pqs)
searchConflictZones [] (ps, qs, pqs) qPaths = (ps, qs ++ map target qPaths, pqs)
searchConflictZones pPaths (ps, qs, pqs) [] = (ps ++ map target pPaths, qs, pqs)
searchConflictZones (pPath : pPaths) (ps, qs, pqs) (qPath : qPaths)
  | pPathId > qPathId            = searchConflictZones (pPath : pPaths) (ps, qPathId : qs, pqs) qPaths
  | pPathId < qPathId            = searchConflictZones pPaths (pPathId : ps, qs, pqs) (qPath : qPaths)
  | weight pPath < weight qPath  = searchConflictZones pPaths (pPathId : ps, qs, pqs) qPaths
  | weight pPath == weight qPath = searchConflictZones pPaths (ps, qs, pPathId : pqs) qPaths
  | otherwise                    = searchConflictZones pPaths (ps, qPathId : qs, pqs) qPaths
  where
    pPathId = target pPath
    qPathId = target qPath


deriving instance Eq Player
deriving instance Show Player
deriving instance Read Player
deriving instance Generic Player
instance B.Binary Player
deriving instance Eq Owner
deriving instance Show Owner
deriving instance Read Owner
deriving instance Generic Owner
instance B.Binary Owner
deriving instance Show Planet
deriving instance Read Planet
deriving instance Generic Planet
instance B.Binary Planet
deriving instance Show Fleet
deriving instance Read Fleet
deriving instance Generic Fleet
instance B.Binary Fleet

deriving instance Show Wormhole
deriving instance Read Wormhole
deriving instance Generic Wormhole
instance B.Binary Wormhole

deriving instance Show Order
deriving instance Read Order
deriving instance Generic Order
instance B.Binary Order
deriving instance Show GameState
deriving instance Read GameState
deriving instance Generic GameState
instance B.Binary GameState

deriving instance Ord PlanetId
deriving instance Eq PlanetId
deriving instance Num PlanetId
deriving instance B.Binary PlanetId
instance Show PlanetId where
  show (PlanetId x) = show x
instance Read PlanetId where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ord Turns
deriving instance Eq Turns
deriving instance Num Turns
deriving instance B.Binary Turns
instance Show Turns where
  show (Turns x) = show x
instance Read Turns where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ord Source
deriving instance Eq Source
deriving instance B.Binary Source
instance Show Source where
  show (Source x) = show x
instance Read Source where
  readsPrec = coerce (readsPrec @Int)

deriving instance Num Growth
deriving instance Ord Growth
deriving instance Eq Growth
deriving instance B.Binary Growth
instance Show Growth where
  show (Growth x) = show x
instance Read Growth where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ix Ships
deriving instance Num Ships
deriving instance Ord Ships
deriving instance Eq Ships
deriving instance B.Binary Ships
instance Show Ships where
  show (Ships x) = show x
instance Read Ships where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ord Target
deriving instance Eq Target
deriving instance B.Binary Target
instance Show Target where
  show (Target x) = show x
instance Read Target where
  readsPrec = coerce (readsPrec @Int)

deriving instance Eq WormholeId
deriving instance Ord WormholeId
deriving instance B.Binary WormholeId
instance Show WormholeId where
  show (WormholeId x) = show x
instance Read WormholeId where
  readsPrec = coerce (readsPrec @Int)

deriving instance Read e => Read (Path e)
deriving instance Show e => Show (Path e)
instance Show a => Show (PList a) where
  show (PList _ xs) = show xs

deriving instance Generic PlanetId
deriving instance Generic WormholeId
deriving instance Generic Ships
deriving instance Generic (Path a)

instance NFData PlanetId
instance NFData Order
instance NFData WormholeId
instance NFData Ships
instance NFData a => NFData (Path a)

deriving instance Eq a => Eq (Tree a)
instance Eq a => Eq (Heap a) where
  (Heap _ h1) == (Heap _ h2) = h1 == h2
deriving instance Generic (Heap a)
instance NFData a => NFData (Heap a)
deriving instance Generic (Tree a)
instance NFData a => NFData (Tree a)

deriving instance (Show a, Show b) => Show (AdjList a b)
