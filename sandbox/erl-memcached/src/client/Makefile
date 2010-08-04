
ERLC=erlc
ERLC_FLAGS=

INCLUDES = $(wildcard *.hrl)
SOURCES = $(wildcard *.erl)
BEAMS = $(SOURCES:%.erl=../../ebin/%.beam)
APP = $(wildcard *.app)
CFG = $(wildcard *.cfg)
TARGET_APP = $(APP:%=../../ebin/%)
TARGET_CFG = $(CFG:%=../../ebin/%)

all: $(BEAMS) $(TARGET_APP) $(TARGET_CFG)

# fix the make warning: Warning: behaviour gen_server2 undefined
../../ebin/ememcached_client.beam: ../../ebin/gen_server2.beam 

../../ebin/%.beam: %.erl $(INCLUDES)
	$(ERLC)	-W $(ERLC_FLAGS) -pa ../../ebin -o ../../ebin $<

../../ebin/%.app: %.app
	cp $< ../../ebin

../../ebin/%.cfg: %.cfg
	cp $< ../../etc

clean:
	rm -f *.beam *~ ../../ebin/*.beam ../../ebin/*.app  ../../etc/*.cfg
