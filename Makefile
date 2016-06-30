2048: 2048.hs Game.hs Algorithm.hs
	ghc 2048.hs

test: 2048
	ruby test.rb

clean:
	rm 2048 *.hi *.o
