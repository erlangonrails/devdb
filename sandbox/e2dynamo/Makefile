all:
	(cd deps/mochiweb/src; $(MAKE))
	(cd src/node; $(MAKE))
	(cd src/manager; $(MAKE))
	(cd src/client; $(MAKE))
	(cd src/clientlib; $(MAKE))

clean:
	(cd deps/mochiweb/src; $(MAKE) clean)
	(cd src/node; $(MAKE) clean)
	(cd src/manager; $(MAKE) clean)
	(cd src/client; $(MAKE) clean)
	(cd src/clientlib; $(MAKE) clean)
	rm -rfv conf/*~
	rm -rfv doc/*~
	rm -rfv ebin/*~
	rm -rfv include/*~
	rm -rfv scripts/*~
	rm -rfv support/*~
	rm -rfv *~