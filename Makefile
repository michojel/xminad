################################################################################
ARCH             := $(shell uname -m)
OS               := $(shell uname -s | tr '[A-Z]' '[a-z]')
TARGET           := $(HOME)/.xmonad/xmonad-$(ARCH)-$(OS)
SRC              := $(shell find . -type f -name '*.hs')
CABAL_BIN        ?= $(shell which cabal)
SANDBOX          := cabal.sandbox.config
XMINAD           := dist/build/xminad/xminad
CABAL_FLAGS      := --enable-optimization=2
CABAL_ADD_SOURCE ?=
DO_CHECK         ?= YES
XMONAD           ?= $(shell which xmonad)
DISPLAY          ?= :0

################################################################################
.PHONY: all build install restart clean realclean check pic

################################################################################
all: build

################################################################################
install: $(TARGET)
	sed 's!{{DATA_DIR}}!$(HOME)/.local/share/xminad!' config/xmobar.config > $(HOME)/.xmobarrc
	make -C pic install

################################################################################
build: $(XMINAD)

################################################################################
pic:
	make -C pic

################################################################################
restart: install
	export DISPLAY=$(DISPLAY)
	$(XMONAD) --restart

################################################################################
clean:
	rm -rf dist $(XMINAD) $(CHECK) $(SANDBOX)
	make -C pic clean

################################################################################
realclean:
	rm -rf .cabal-sandbox

################################################################################
check: build
	$(CHECK)
	cabal check

################################################################################
ifeq ($(DO_CHECK),YES)
  CHECK = dist/build/checkrc/checkrc
else
  CHECK = :
endif

################################################################################
build: $(XMINAD)

################################################################################
$(XMINAD): $(SRC) $(SANDBOX)
	ghc -V
	$(CABAL_BIN) build
	$(CHECK)

################################################################################
$(SANDBOX):
	$(CABAL_BIN) sandbox init
	$(if $(CABAL_ADD_SOURCE),$(CABAL_BIN) sandbox add-source $(CABAL_ADD_SOURCE),)
	$(CABAL_BIN) install --only-dependencies $(CABAL_FLAGS)
	$(CABAL_BIN) configure $(CABAL_FLAGS)
	touch $@

################################################################################
$(TARGET): $(XMINAD)
	mkdir -p $(dir $@)
	if [ -r $@ ]; then mv $@ $@.prev; fi
	cp -p $? $@
	cd $(dir $@) && ln -nfs $(notdir $@) xminad
