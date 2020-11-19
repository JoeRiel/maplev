# Makefile - for the maplev distribution
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

PKG := maplev
VERSION := 3.0.2

CLOUD-ID := 5655772713713664
CLOUD-DESCRIPTION := An Emacs mode for Maple developers
CLOUD-GROUP := Maple Emacs:5683998438195200
CLOUD-VERSION := 1
# PKG-DATE := $(shell date '+%B %Y')

PKG-VER  := $(PKG)-$(VERSION)
PKG-DIR := /tmp/$(PKG-VER)

TAR-FILE := $(PKG-VER).tar

$(info PKG-DIR=$(PKG-DIR))

$(PKG-DIR): lisp/*.el doc/maplev.info
	$(RM) -r $@
	mkdir $@
	$(CP) $^ $@

$(TAR-FILE): $(PKG-DIR) $(EL-FILES)
	tar --verbose --create --file $(TAR-FILE) --directory=/tmp $(PKG-VER)

# Activate selected make sections
BOOK  := true
CLOUD := true
book-files := $(TAR-FILE) doc/maplev.html doc/maplev.pdf
book-map := , "bin.APPLE_UNIVERSAL_OSX/pmaple" = "pmaple/bin.APPLE_UNIVERSAL_OSX/pmaple"\
            , "bin.X86_64_LINUX/pmaple" = "pmaple/bin.X86_64_LINUX/pmaple"\
            , "bin.X86_64_WINDOWS/pmaple.exe" = "pmaple/bin.X86_64_WINDOWS/pmaple.exe"

book: $(PKG-DIR) $(TAR-FILE)

include MapleLisp.mk
