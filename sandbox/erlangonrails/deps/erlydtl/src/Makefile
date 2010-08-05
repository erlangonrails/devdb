
ERLC=erlc
ERLC_FLAGS= 

INCLUDES = $(wildcard *.hrl)
SOURCES = $(wildcard *.erl)
BEAMS = $(SOURCES:%.erl=../../../ebin/%.beam)
APP = $(wildcard *.app)
CFG = $(wildcard *.cfg)
TARGET_APP = $(APP:%=../../../ebin/%)
TARGET_CFG = $(CFG:%=../../../etc/%)

all: $(BEAMS) $(TARGET_APP) $(TARGET_CFG) ../../../ebin/erlydtl_parser.beam

../../../ebin/erlydtl_parser.beam: erlydtl_parser.erl
	$(ERLC)	-W $(ERLC_FLAGS) -o ../../../ebin $<

# erlc also support "Yecc source code", It generates an .erl file. 
erlydtl_parser.erl: erlydtl_parser.yrl
	$(ERLC) -o . $<

../../../ebin/%.beam: %.erl $(INCLUDES)
	$(ERLC)	-W $(ERLC_FLAGS) -o ../../../ebin $<

../../../ebin/%.app: %.app
	cp $< ../../../ebin

../../../etc/%.cfg: %.cfg
	cp $< ../../../etc

clean:
	rm -f *.beam *~ ../../../ebin/*.beam ../../../ebin/*.app  ../../../etc/*.cfg
