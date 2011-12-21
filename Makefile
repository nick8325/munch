.NOTPARALLEL:

GHC = ghc -O -funfolding-use-threshold=1000
VARIANTS = NotParsec AttoParsec Parsec NoMessages IgnoreLabels
BENCHMARKS = Brackets

all: $(foreach v, $(VARIANTS), $(foreach b, $(BENCHMARKS), $v$b $(if $(HCR), $v$b.hcr)))

IgnoreLabelsBrackets: GHC += -DIGNORE_LABELS

define TEMPLATE
%$b $(if $(HCR), %$b.hcr):: *.hs Makefile
	ln -sf $$*.hs Prim.hs
	ln -sf Run$$*.hs Run.hs
	$(GHC) -fforce-recomp --make $b -o $$@
ifdef HCR
	$(GHC) -fforce-recomp -ddump-simpl -c $b.hs > $$@.hcr
endif
endef

$(foreach b, $(BENCHMARKS), $(eval $(TEMPLATE)))


clean:
	rm -f *Brackets *.hcr *.o *.hi Prim.hs Run.hs
