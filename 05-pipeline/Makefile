CPP_FLAGS=-std=gnu++0x

tf-05-haskell:	tf-05.hs
	ghc -O2 -Wall -o $@ $<

tf-06: tf-06.cpp
	$(CXX) $(CPP_FLAGS) -o $@ $<

.PHONY: clean
clean:
	rm tf-06
