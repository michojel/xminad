HEIGHT=20
OUT=$(addsuffix .xpm, $(basename $(filter-out bar-base.svg, $(wildcard *.svg))))
DESTDIR=$(HOME)/.local/share/xminad
DESTS=$(addprefix $(DESTDIR)/, $(OUT))

.PHONY: all clean install datadir

all: $(OUT)

install: $(DESTS)

datadir: $(DESTDIR)

$(DESTDIR):
	-mkdir $(DESTDIR)

$(DESTDIR)/%: datadir %
	install -m 0644 $* $@ 

%.xpm: %.svg
	convert -resize x$(HEIGHT) -background none $? $@

clean:
	rm *.xpm
