# PUSH               
sp              := $(sp).x
dirstack_$(sp)  := $(d)
d               := $(dir)

# Local rules and targets
SRCS_$(d)       := $(wildcard $(d)/*.xml)
TGTS_$(d)       := $(SRCS_$(d):$(d)/%.xml=$(d)/../ebin/%.beam)

.SECONDEXPANSION:
$(TGTS_$(d)): $(d)/$$(basename $$(@F)).xml $(d)/../ebin/fix_generate.erl
	$(ERL) -pa $(@D) -noshell -run fix_generate generate $(<) \
	  -s erlang halt > $(@D)/../src/$(basename $(@F)).erl
	$(ERLC) $(ERLC_FLAGS) -o $(@D) $(@D)/../src/$(basename $(@F)).erl

TGT_BIN		:= $(TGT_BIN) $(TGTS_$(d))
CLEAN		:= $(CLEAN) $(TGTS_$(d)) $(DEPS_$(d))

$(TGTS_$(dir)): $(d)/Rules.mk

# POP
-include        $(DEPS_$(d))

d               := $(dirstack_$(sp))
sp              := $(basename $(sp))
