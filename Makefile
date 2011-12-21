.NOTPARALLEL:

GHC = ghc -O -funfolding-use-threshold=1000

all: NotParsecBrackets AttoParsecBrackets ParsecBrackets NoMessagesBrackets IgnoreLabelsBrackets

IgnoreLabelsBrackets: GHC += -DIGNORE_LABELS

%Brackets: *.hs
	ln -sf $*.hs Prim.hs
	ln -sf Run$*.hs Run.hs
	$(GHC) -fforce-recomp --make Brackets -o $@
	$(GHC) -fforce-recomp -ddump-simpl -c Brackets.hs > $@.hcr

clean:
	rm -f *Brackets *.hcr *.o *.hi Prim.hs Run.hs
