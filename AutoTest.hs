{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module AutoTest where

import Submission1 hiding (maximum)
import Test (test, testFromFile, runTests, Test (..))

import Data.Map (fromList)
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Coerce
import Control.Exception
import GHC.Generics
import Data.Type.Bool
import Data.List
import Data.Function
import Data.Ord (comparing, Down (..))

import Control.Monad
import Control.DeepSeq

import System.IO

main :: IO ()
main = runTests $ do
  testFromFile "Knapsack' tests" (uncurry (knapsack' @String @Int @Int)) (==) "tests/knapsackTests.txt"
  testFromFile "Knapsack'' tests" (uncurry (knapsack'' @String @Int @Int)) ((==) `on` fst) "tests/knapsackTests2.txt"
  testFromFile "Bounded knapsack tests" (uncurry (bknapsack @String @Int @Int)) ((==) `on` fst) "tests/bknapsackTests.txt"
  testFromFile "Bounded knapsack' tests" (uncurry3 (bknapsack' @String @Int @Int)) ((==) `on` fst) "tests/bknapsackTests2.txt"
  testFromFile "Bounded knapsack'' tests" (uncurry (bknapsack'' @String @Int @Int)) ((==) `on` fst) "tests/bknapsackTests3.txt"
  testFromFile "Dijkstra tests" (uncurry (shortestPaths @[(String, String, Integer)])) ((==) `on` sort . map pathToTuple) "tests/dijkstra.txt"
  test "insert tests" (fromPQueue @Heap . uncurry toPQueue) (==) insertTests
  test "insert rank tests" (rankHeap . uncurry toPQueue) (==) insertRankTests
  testFromFile "Dijkstra (heap) tests" (uncurry (shortestPaths' @[(String, String, Integer)])) ((==) `on` sort . map pathToTuple) "tests/dijkstra.txt"
  testFromFile "Conflict zones tests" (uncurry3 conflictZones) ((==) `on` (\(a, b, c) -> (sort a, sort b, sort c))) "tests/conflictZonesTests.txt"
  test "AdjList vertices tests" vertices ((==) `on` sort) [adjList :=> ["a", "b", "c", "d"]]
  test "AdjList edges tests" edges ((==) `on` sort) [adjList :=> [("a","b",10),("a","c",20),("b","a",5),("b","d",8),("d","b",3),("d","a",4)]]
  test "AdjList edgesFrom tests" (uncurry edgesFrom) ((==) `on` sort) [(adjList, "a") :=> [("a","b",10),("a","c",20)], (adjList, "b") :=> [("b","a",5),("b","d",8)]]
  test "AdjList edgesTo tests" (uncurry edgesTo) ((==) `on` sort) [(adjList, "a") :=> [("b","a",5),("d","a",4)], (adjList, "b") :=> [("a","b",10),("d","b",3)]]
  test "AdjList velem tests" (uncurry velem) (==) [("x", adjList) :=> False, ("a", adjList) :=> True]
  test "AdjList eelem tests" (uncurry eelem) (==) [(("a", "b", 3), adjList) :=> False, (("b", "d", 8), adjList) :=> True]

uncurry3 :: (a->b->c -> d) -> (a,b,c) -> d
uncurry3 f ~(a, b, c) = f a b c

pathToTuple :: Path e -> (Weight, [e])
pathToTuple (Path w es) = (w, es)

instance Show (String -> String -> Ordering) where
  show _ = "<<function>>"

insertTests :: [Test ((String -> String -> Ordering), [String]) [String]]
insertTests
  = [ (compare, xs) :=> xs
    , (compare, (reverse xs)) :=> xs
    , (comparing Down, xs) :=> reverse xs
    , (comparing Down, (reverse xs)) :=> reverse xs
    ]
  where xs = ["a", "b", "b", "c", "d", "e"]

insertRankTests :: [Test ((String -> String -> Ordering), [String]) Int]
insertRankTests
  = [ (compare, xs) :=> 1
    , (compare, (reverse xs)) :=> 2
    , (comparing Down, xs) :=> 2
    , (comparing Down, (reverse xs)) :=> 1
    ]
  where xs = ["a", "b", "b", "c", "d", "e"]

adjList :: AdjList (String, String, Integer) String
adjList
  = AdjList
      [ ("a", [("a", "b", 10), ("a", "c", 20)])
      , ("b", [("b", "a", 5), ("b", "d", 8)])
      , ("c", [])
      , ("d", [("d", "b", 3), ("d", "a", 4)])
      ]
