all:
	(cd src;$(MAKE))
	(cd src/controllers;$(MAKE))
	(cd src/helpers;$(MAKE))
	(cd deps/erlydtl/src;$(MAKE))
	(cd deps/mochiweb/src;$(MAKE))
	(cd deps/erlangonrails/src;$(MAKE))
	(mv deps/erlangonrails/ebin/*.beam ./ebin)

clean:
	(cd src;$(MAKE) clean)
	(cd src/controllers;$(MAKE) clean)
	(cd src/helpers;$(MAKE) clean)
	(cd deps/erlydtl/src;$(MAKE) clean)
	(cd deps/mochiweb/src;$(MAKE) clean)
	(cd deps/erlangonrails/src;$(MAKE) clean)
	rm -rfv ./doc/*~
	rm -rfv ./include/*~
	rm -rfv ./scripts/*~
	rm -rfv ./src/*~
	rm -rfv ./support/*~
	rm -rfv *~
