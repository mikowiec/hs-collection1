
HOME=/kunder/z/zarquon
GHC=/home/peter/apps/ghc/bin/ghc
FP=../../src/fp
HSFLAGS=-i$(FP) 

cgi_src_files = $(wildcard *.cgi.hs)

cgi_files = $(patsubst %cgi.hs, %cgi, $(cgi_src_files))

all: $(cgi_files)

%.cgi: %.cgi.hs
	HOME=$(HOME) $(GHC) $(HSFLAGS) --make $< -o $@

depend:
	for hs in $(cgi_src_files); do $(GHC) $(HSFLAGS) -M $$hs; done




# DO NOT DELETE: Beginning of Haskell dependencies
../../src/fp/Misc/Misc.o : ../../src/fp/Misc/Misc.hs
./RecompilePage.o : ./RecompilePage.hs
./RecompilePage.o : ../../src/fp/Misc/Misc.hi
./DynSite.o : ./DynSite.hs
./DynSite.o : ./RecompilePage.hi
./DynSite.o : ../../src/fp/Misc/Misc.hi
view.cgi.o : view.cgi.hs
view.cgi.o : ./RecompilePage.hi
view.cgi.o : ./DynSite.hi
view.cgi.o : ../../src/fp/Misc/Misc.hi
# DO NOT DELETE: End of Haskell dependencies
