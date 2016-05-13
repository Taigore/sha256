default:
	ghc Main.hs -outputdir obj -o main

run:
	./main

.PHONY: default
