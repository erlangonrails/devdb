all: 
	(cd deps/exmpp-0.9.3;$(MAKE))
	(cd src;$(MAKE))
run:
	erl -pa ./ebin -pa ./deps/exmpp-0.9.3/ebin

clean:
	(cd deps/exmpp-0.9.3;$(MAKE) clean)
	(cd src;$(MAKE) clean)
	rm -rfv ./ebin/*
	rm -rfv *~
