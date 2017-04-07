GHC= ghc
HFLAGS = --make
SCRS = Configure.hs Dataparalelo.hs Parser.hs Parsing.lhs Scraper.hs 
MAIN = Main.hs



all:
	$(GHC) $(HFLAGS) $(MAIN) -odir obj -hidir bin


.PHONY: clean

clean:
	$(RM) *.o *hi





