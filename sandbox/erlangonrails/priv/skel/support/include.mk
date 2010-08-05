# erlang on rails - include.mk

ERL := erl
ERLC := $(ERL)c

INCLUDE_DIRS := ../include $(wildcard ../deps/*/include)
EBIN_DIRS := $(wildcard ../deps/*/ebin)

# erlc flags:
# <1> -W 
#     Same as -W1. Default.
# <2> -I directory 
#     Instructs the compiler to search for include files in the specified directory when
#     encountering an -include or -include_dir directive.
# <3> -pa directory
#     Appends directory to the front of the code path in the invoked Erlang emulator. 
# <4> -o directory
#     The directory where the compiler should place the output files. If not specified, 
#     output files will be placed in the current working directory.
ERLC_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %)

EBIN_DIR := ../ebin
EMULATOR := beam

ERL_SOURCES := $(wildcard *.erl)
MODULES = $(ERL_SOURCES:%.erl=%)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ../include/*.hrl)

ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
APP_FILES := $(wildcard *.app)

# Notes: the *.app and *.beam files under ../ebin
EBIN_FILES = $(ERL_OBJECTS) $(APP_FILES:%.app=../ebin/%.app)

# copy *.app to ../ebin
../ebin/%.app: %.app
	cp $< $@

# compile *.erl, the output directory is ../ebin
$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<
