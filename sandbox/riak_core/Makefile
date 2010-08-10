all:
	cd src/deps && $(MAKE)
	cd src && $(MAKE)

clean:
	rm -f *~
	cd src/deps && $(MAKE) clean
	cd src && $(MAKE) clean
