ERL := erl
ERLC := $(ERL)c

ERLC_FLAGS := -W 

EBIN_DIR := ../../../ebin
EMULATOR := beam

ERL_SOURCES := $(wildcard *.erl)
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


