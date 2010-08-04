

all:
	cd src && $(MAKE)
	cd src/client && $(MAKE)

clean:
	rm -f *~
	rm -f ./log/*
	rm -f ./database/*
	cd src && $(MAKE) clean
	cd src/client && $(MAKE) clean
