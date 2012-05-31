.NOTPARALLEL:

GHC = ghc -O -funfolding-creation-threshold=1000 -funfolding-use-threshold=1000 -fllvm -rtsopts
VARIANTS = Munch AttoParsec Parsec NoMessages IgnoreLabels Try More TryMore Errors Commit Commit2
BENCHMARKS = Brackets RFC2616 Arith Prolog

all: $(foreach v, $(VARIANTS), $(foreach b, $(BENCHMARKS), $v$b $(if $(HCR), $v$b.hcr)))

run: $(foreach v, $(VARIANTS), $(foreach b, $(BENCHMARKS), $v$b.run))
runmy: $(foreach v, $(filter-out Parsec AttoParsec, $(VARIANTS)), $(foreach b, $(BENCHMARKS), $v$b.run))

%.run: %
	./$* | grep '^benchmarking\|^collecting\|^mean:'

IgnoreLabelsBrackets: GHC += -DIGNORE_LABELS

define TEMPLATE
%$b: $b.hs *.hs Makefile
	ln -sf $$*.hs Prim.hs
	ln -sf Run$$*.hs Run.hs
	$(GHC) -fforce-recomp --make $b -o $$@
ifdef HCR
	$(GHC) -fforce-recomp -ddump-simpl -c $b.hs > $$@.hcr -dsuppress-module-prefixes -dsuppress-uniques -dsuppress-coercions
endif
ifdef STG
	$(GHC) -fforce-recomp -ddump-stg -c $b.hs > $$@.stg -dsuppress-module-prefixes -dsuppress-uniques -dsuppress-coercions
endif
ifdef CMM
	$(GHC) -fforce-recomp -ddump-opt-cmm -c $b.hs > $$@.cmm -dsuppress-module-prefixes -dsuppress-uniques -dsuppress-coercions
endif
endef

$(foreach b, $(BENCHMARKS), $(eval $(TEMPLATE)))


clean:
	rm -f *Brackets *Arith *RFC2616 *.hcr *.o *.hi Prim.hs Run.hs
