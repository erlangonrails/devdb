ERL := erl
ERLC := $(ERL)c
INCLUDE_DIRS := ../../include $(wildcard ../../deps/*/include)
EBIN_DIRS := $(wildcard ../../deps/*/ebin)

ERLC_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %)

EBIN_DIR := ../../ebin
EMULATOR := beam

ERL_SOURCES := $(wildcard *.erl)
MODULES = $(ERL_SOURCES:%.erl=%)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ../../include/*.hrl)

ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))

all: $(ERL_OBJECTS)

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

clean:
	rm -rfv $(ERL_OBJECTS)