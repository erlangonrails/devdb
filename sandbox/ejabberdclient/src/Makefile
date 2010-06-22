INCLUDE_DIRS := ../include ../deps/exmpp-0.9.3/include
EBIN_DIRS := ../ebin ../deps/exmpp-0.9.3/ebin

EBIN_DIR := ../ebin

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


ERL_SOURCES := $(wildcard *.erl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.beam)
APP_FILES := $(wildcard *.app)
APP_OBJECTS := $(APP_FILES:%.app=$(EBIN_DIR)/%.app)


# Notes: the *.app and *.beam files under ../ebin
EBIN_FILES = $(ERL_OBJECTS) $(APP_OBJECTS) 

all: $(EBIN_FILES)

clean:
	rm -rfv *~

# copy *.app to ../ebin
$(EBIN_DIR)/%.app: %.app
	cp $< $@

# compile *.erl, the output directory is ../ebin
$(EBIN_DIR)/%.beam: %.erl
	erlc $(ERLC_FLAGS) -o $(EBIN_DIR) $<

