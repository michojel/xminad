################################################################################
PREFIX              ?= $(HOME)/.local
BIN_DIR             := $(PREFIX)/bin
DATA_DIR            := $(PREFIX)/share/xminad
ARCH                := $(shell uname -m)
OS                  := $(shell uname -s | tr '[A-Z]' '[a-z]')
TARGET              := $(BIN_DIR)/xminad
SRC                 := $(shell find . -type f -name '*.hs')
CABAL_BIN           ?= $(shell which cabal)
SANDBOX             := cabal.sandbox.config
CONFIG              := cabal.config
XMINAD              := dist/build/xminad/xminad
CABAL_FLAGS         := --enable-optimization=2
CABAL_DIR_FLAGS     := --prefix=$(PREFIX) --bindir=$(BIN_DIR) --datadir=$(DATA_DIR) 
CABAL_ADD_SOURCE    ?=
DO_CHECK            ?= YES
DISPLAY             ?= :0

################################################################################
.PHONY: all build install restart clean realclean check pic configure

################################################################################
all: build

################################################################################
install: $(TARGET)
	# TODO: install the xmobarrc under the PREFIX and configure xminad with
	# the correct path
	sed 's!{{DATA_DIR}}!$(DATA_DIR)!' config/xmobar.config \
	    > $(HOME)/.xmobarrc
	sed 's!{{BIN_DIR}}!$(BIN_DIR)!' config/xminad.desktop \
	    > $(PREFIX)/share/applications/xminad.desktop
	make -C pic install


configure: $(SANDBOX)
	$(CABAL_BIN) configure $(CABAL_FLAGS) $(CABAL_DIR_FLAGS)
	echo "install-dirs" > $(CONFIG)
	grep -E '^\s*(bindir|datadir|install-dirs)\>' $(SANDBOX) >$(CONFIG)
	sed -i 's,\(bindir\s*:\s*\).*,\1$(BIN_DIR),'   $(CONFIG)
	sed -i 's,\(datadir\s*:\s*\).*,\1$(DATA_DIR),' $(CONFIG)

################################################################################
build: $(XMINAD)

################################################################################
pic:
	make -C pic

################################################################################
restart: install
	export DISPLAY="$(DISPLAY)"
	$(TARGET) --restart

################################################################################
clean:
	rm -rf dist $(XMINAD) $(CHECK) $(SANDBOX) $(CONFIG)
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
$(XMINAD): $(SRC) $(CONFIG)
	ghc -V
	$(CABAL_BIN) build
	$(CHECK)

################################################################################
$(SANDBOX):
	$(CABAL_BIN) sandbox init
	$(if $(CABAL_ADD_SOURCE),$(CABAL_BIN) sandbox add-source $(CABAL_ADD_SOURCE),)
	$(CABAL_BIN) install --only-dependencies $(CABAL_FLAGS)
	touch $@

################################################################################
$(CONFIG):
	make -C . configure

################################################################################
$(TARGET): $(XMINAD)
	if [[ ! -e $(dir $@) ]]; then mkdir -p $(dir $@); fi
	if [[ -r $@ ]]; then mv $@ $@.prev; fi
	cp -p $? $@
	if [[ $(notdir $(TARGET)) != xminad ]]; then \
	    cd $(dir $@) && ln -nfs $(notdir $@) xminad; \
	fi
