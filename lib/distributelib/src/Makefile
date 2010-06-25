ERL := erl
ERLC := $(ERL)c

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
ERLC_FLAGS := -W 

EBIN_DIR := ../ebin
EMULATOR := beam

ERL_SOURCES := $(wildcard *.erl)
ERL_MODULES := $(ERL_SOURCES:%.erl=%)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ../../include/*.hrl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
APP_FILES := $(wildcard *.app)

EBIN_FILES := $(ERL_OBJECTS) $(APP_FILES:%.app=$(EBIN_DIR)/%.app)

$(EBIN_DIR)/%.app: %.app
	cp $< $@

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

all: $(EBIN_FILES)

clean:
	rm -rfv $(EBIN_FILES)
	rm -rfv *~