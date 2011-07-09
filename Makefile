# Flags
export LANG=C
export LC_ALL=C
ERLC_FLAGS+=-W +debug_info

# The all target
.PHONY: all
all: targets build/config.status

# The test target
.PHONY: test
test: all run_tests

# Autoconf & configure
CONFIG_GEN_FILES = $(shell cat build/_gen_files 2>/dev/null)
CONFIG_IN_FILES = $(CONFIG_GEN_FILES:%=%.in)

$(CONFIG_GEN_FILES): build/_tstamp_h

build/_tstamp_h: $(CONFIG_IN_FILES) build/_gen_files
	cd build && ./config.status

build/_gen_files: build/configure.in

build/configure: build/configure.in
	cd build && autoconf

build/path.mk:  build/path.mk.in build/config.status

build/config.status: build/configure
	cd build && ./configure

CONFCLEAN := $(CONFCLEAN) $(CONFIG_GEN_FILES) build/configure \
	build/config.status build/config.log build/_gen_files \
	build/_tstamp_h
CONFDIRCLEAN := build/autom4te.cache/

# Paths and compile command

include build/path.mk

COMP = $(ERLC) $(ERLC_FLAGS) -o $(@D) $(<)

# Subdirectories
## CHANGE HERE WHEN ADDING NEW LIBS
dir := lib/fix
include $(dir)/Rules.mk

# General directory independent rules
INCLUDE_FILES:=$(wildcard */*/src/*.hrl) $(wildcard */*/include/*.hrl)
.INTERMEDIATE: $(INCLUDE_FILES)
.SECONDEXPANSION:

ERL_INCLUDE_FILES:=$(shell find $(ERL_DIR)/lib -name "*.hrl" | \
	sed "s,$(ERL_DIR)/lib/,," | \
	sed "s,^\(.*\)-[^/]*/\(.*\)/\(.*\),\1/\2/\3,")
.INTERMEDIATE: $(ERL_INCLUDE_FILES)
.SECONDEXPANSION:
$(ERL_INCLUDE_FILES): $$(notdir $$@)
vpath %.hrl $(wildcard $(ERL_DIR)/lib/*/*)

# The variables TGT_* and CLEAN are added to by rules in Rules.*.mk
.PHONY: run_tests
run_tests: $(TGT_TEST)

.PHONY: targets
targets: $(TGT_BIN) $(TGT_SBIN) $(TGT_ETC) $(TGT_LIB)

.PHONY: clean
clean:
	rm -f $(CLEAN)

## FIXME: find out the standard GNU name for this target.
.PHONY: conf_clean
conf_clean:
	rm -f $(CONFCLEAN)
	rm -r $(CONFDIRCLEAN)

.PHONY: real_clean
real_clean: clean conf_clean
