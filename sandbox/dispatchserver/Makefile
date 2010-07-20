all:
	(cd src;$(MAKE))

buildtest:
	(cd test;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	(cd test;$(MAKE) clean)
	rm -rfv ./ebin/*
	rm -rfv ./etc/*
	rm -rfv ./log/*
	rm -rfv ./include/*~
	rm -rfv ./script/*~
	rm -rfv ./doc/*~
	rm -rfv *~