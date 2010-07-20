INCLUDE_DIRS := ../include 
EBIN_DIRS := ../ebin

EBIN_DIR := ../ebin
ETC_DIR := ../etc

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
CFG_FILES := $(wildcard *.cfg)
CFG_OBJECTS := $(CFG_FILES:%.cfg=$(ETC_DIR)/%.cfg)


# Notes: the *.app and *.beam files under ../ebin
EBIN_FILES = $(ERL_OBJECTS) $(APP_OBJECTS) $(CFG_OBJECTS)

all: $(EBIN_FILES)
	(cd log;$(MAKE))
	(cd dynamic;$(MAKE))
	(cd misc;$(MAKE))
	(cd tcp_server;$(MAKE))
	(cd core;$(MAKE))

clean:
	(cd log;$(MAKE) clean)
	(cd dynamic;$(MAKE) clean)
	(cd misc;$(MAKE) clean)
	(cd tcp_server;$(MAKE) clean)
	(cd core;$(MAKE) clean)
	rm -rfv *~

# copy *.app to ../ebin
$(EBIN_DIR)/%.app: %.app
	cp $< $@

# copy *.cfg to ../etc
$(ETC_DIR)/%.cfg: %.cfg
	cp $< $@

# compile *.erl, the output directory is ../ebin
$(EBIN_DIR)/%.beam: %.erl
	erlc $(ERLC_FLAGS) -o $(EBIN_DIR) $<

