HEIGHT=20
OUT=$(addsuffix .xpm, $(basename $(filter-out bar-base.svg, $(wildcard *.svg))))
DESTDIR=$(HOME)/.local/share/xminad
DESTS=$(addprefix $(DESTDIR)/, $(OUT))

.PHONY: all clean install

all: $(OUT)

install: $(DESTS)

$(DESTDIR):
	mkdir $(HOME)/.local/share/xminad

$(DESTDIR)/%: $(DESTDIR) %
	install -m 0644 $* $@ 

%.xpm: %.svg
	convert -resize x$(HEIGHT) -background none $? $@

clean:
	rm *.xpm
