include ../support/include.mk

CFG = $(wildcard *.cfg)
TARGET_CFG = $(CFG:%=../etc/%)

all: $(EBIN_FILES) $(TARGET_CFG)

../etc/%.cfg: %.cfg
	cp $< ../etc

clean:
	rm -rfv $(EBIN_FILES)
	rm -rfv $(TARGET_CFG)
	rm -rfv *~
