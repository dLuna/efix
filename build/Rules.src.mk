# PUSH
sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

# Local rules and targets
SRCS_$(d)       := $(wildcard $(d)/*.erl)
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

TGT_BIN		:= $(TGT_BIN) $(TGTS_$(d))
CLEAN		:= $(CLEAN) $(TGTS_$(d)) $(DEPS_$(d))

$(TGTS_$(d)):	build/Rules.src.mk

# POP
-include	$(DEPS_$(d))

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
