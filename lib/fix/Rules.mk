# PUSH
sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

# Subdirectories
dir		:= $(d)/src
include		build/Rules.src.mk
dir		:= $(d)/priv
include		$(dir)/Rules.mk

$(TGTS_$(dir)):	$(d)/Rules.mk

# POP
-include	$(DEPS_$(d))

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
