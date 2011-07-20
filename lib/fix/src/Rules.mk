# PUSH
sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

# Local rules and targets
SRCS_$(d)       := $(wildcard $(d)/*.erl)
XML_$(d)	:= $(wildcard $(d)/../priv/*.xml)
GEN_$(d)        := $(XML_$(d):$(d)/../priv/%.xml=$(d)/%.erl)
GEN_TGTS_$(d)	:= $(GEN_$(d):$(d)/%.erl=$(d)/../ebin/%.beam) \
			$(d)/../include/fix_records.hrl
TGTS_$(d)	:= $(SRCS_$(d):$(d)/%.erl=$(d)/../ebin/%.beam)
DEPS_$(d)	:= $(TGTS_$(d):%.beam=%.d)

$(d)/../ebin/%.beam: $(d)/%.erl
	$(COMP)

$(d)/../ebin/%.d: $(d)/%.erl
	@set -e; rm -f $@; \
	grep "^-include(" $< | \
	sed 's,^-include("\(.*\)")\..*,$(@D)/$(*F).beam : $(<D)/\1,' > $@
	@set -e; \
	grep "^-include_lib(" $< | \
	sed 's,^-include_lib("\(.*\)")\..*,$(@D)/$(*F).beam : \1,' >> $@

.SECONDEXPANSION:
$(GEN_$(d)): $(d)/../priv/$$(basename $$(@F)).xml \
	  $(d)/../ebin/fix_generate.beam $(d)/fix_generate.aux \
	  $(d)/../include/fix_records.hrl
	$(ERL) -pa $(@D)/../ebin -noshell -run fix_generate parser $(<) \
	  -s erlang halt > $(@)

$(d)/../include/fix_records.hrl: $(XML_$(d)) \
	  $(d)/../ebin/fix_generate.beam
	$(ERL) -pa $(@D)/../ebin -noshell -run fix_generate \
	  hrl $(^) -s erlang halt > $(@)

TGT_TEST_$(d)	:= $(d)/fix_test

.PHONY: $(d)/fix_test
$(d)/fix_test: 
	$(ERL) -pa $(@D)/../ebin -noshell -run fix_test run \
	  $(@D)/../priv/test/ -s erlang halt

TGT_TEST	:= $(TGT_TEST) $(TGT_TEST_$(d))

TGT_BIN		:= $(TGT_BIN) $(TGTS_$(d)) $(GEN_TGTS_$(d))
CLEAN		:= $(CLEAN) $(TGTS_$(d)) $(DEPS_$(d)) $(GEN_$(d)) $(GEN_TGTS_$(d))

$(TGTS_$(d)): $(d)/Rules.mk
$(GEN_TGTS_$(d)): $(d)/Rules.mk

# POP
-include	$(DEPS_$(d))

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
