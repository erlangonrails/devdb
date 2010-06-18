all:
	(cd src/node; $(MAKE))
	(cd src/mochiweb; $(MAKE))
	(cd src/client; $(MAKE))
	(cd src/manager; $(MAKE))

test: all
	(cd src/node;$(MAKE) test)

clean:
	(cd src/node;$(MAKE) clean)
	(cd src/mochiweb;$(MAKE) clean)
	(cd src/client;$(MAKE) clean)
	(cd src/manager;$(MAKE) clean)
