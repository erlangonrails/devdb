all:
	(cd deps/mochiweb/src; $(MAKE))
	(cd src/node; $(MAKE))

clean:
	(cd deps/mochiweb/src; $(MAKE) clean)
	(cd src/node; $(MAKE) clean)
	rm -rfv conf/*~
	rm -rfv doc/*~
	rm -rfv ebin/*~
	rm -rfv include/*~
	rm -rfv scripts/*~
	rm -rfv support/*~
	rm -rfv *~