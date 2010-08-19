all:
	(cd src;$(MAKE))
	(cd src/controllers;$(MAKE))
	(cd src/helpers;$(MAKE))
	(cd src/models;$(MAKE))
	(cd src/client_api;$(MAKE))
	(cd src/crontab_task;$(MAKE))
	(cd src/xmppclient;$(MAKE))
	(cd deps/erlydtl/src;$(MAKE))
	(cd deps/mochiweb/src;$(MAKE))
	(cd deps/exmpp-0.9.3;$(MAKE))
	(cd deps/erlangonrails/src;$(MAKE))
	(cp deps/erlangonrails/ebin/* ./ebin)

clean:
	(cd src;$(MAKE) clean)
	(cd src/controllers;$(MAKE) clean)
	(cd src/helpers;$(MAKE) clean)
	(cd src/client_api;$(MAKE) clean)
	(cd src/crontab_task;$(MAKE) clean)
	(cd src/xmppclient;$(MAKE) clean)
	(cd deps/erlydtl/src;$(MAKE) clean)
	(cd deps/mochiweb/src;$(MAKE) clean)
	(cd deps/erlangonrails/src;$(MAKE) clean)
	(cd deps/exmpp-0.9.3;$(MAKE) clean)
	rm -rfv ./priv/lib/.libs/*
	rm -rfv ./priv/lib/*
	rm -rfv ./ebin/*
	rm -rfv ./doc/*~
	rm -rfv ./scripts/*~
	rm -rfv ./src/*~
	rm -rfv ./support/*~
	rm -rfv ./database/*
	rm -rfv ./log/*
	rm -rfv ./etc/*
	rm -rfv ./data/*
	rm -rfv *~
