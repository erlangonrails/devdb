# GNU make, please

MODULES= bal_sup balance bal_proxy tcp_proxy

TARGET_FILES= $(MODULES:%=../ebin/%.$(EMULATOR))

EBIN = ../ebin
ESRC = .
EMULATOR = beam
ERLC = erlc
ERL = erl
ERL_FLAGS = 
ERL_COMPILE_FLAGS = +debug_info

# Explicit targets
all: $(TARGET_FILES) balance.boot balance.script

balance.boot balance.script: ../ebin/balance.app balance.rel \
                $(TARGET_FILES)
	$(ERLC) -pa ../ebin balance.rel

clean:
	-rm -f $(TARGET_FILES)
	-rm -f core *.core ktrace.out*
	-rm -f balance.boot balance.script

$(EBIN)/%.$(EMULATOR): $(ESRC)/%.erl
	$(ERLC) -b$(EMULATOR) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

