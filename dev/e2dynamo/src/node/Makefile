include ../../support/include.mk

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

test:
	$(ERL) -noshell -pa ../../ebin -s e2d_test -s init stop
