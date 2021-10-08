.PHONY: check
check: Submission1.hs
	ghc $<  -fno-code

test_results_info.json: AutoTest.hs .FORCE
	runghc AutoTest.hs > test_results_info.json

.PHONY: test
test: AutoTest.hs .FORCE
	runghc AutoTest.hs -v > /dev/null

.PHONY: clean
clean:
	@true

.PHONY: .FORCE
.FORCE:
