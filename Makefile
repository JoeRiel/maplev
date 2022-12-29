# Makefile - for the maplev distribution
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

include version.mk

PKG := maplev
EXTRA_ELFLAGS := --eval "(add-to-list (quote load-path) (expand-file-name \".emacs.d/elpa/button-lock-1.0.2\" \"$(HOME)\"))"

# Activate selected make sections

BOOK  := true
CLOUD := true

BOOK-FILES := doc/${PKG}.html doc/${PKG}.pdf
BOOK-MAP := , "bin.X86_64_LINUX/pmaple"        = "pmaple/bin.X86_64_LINUX/pmaple"\
            , "bin.X86_64_WINDOWS/pmaple.exe"  = "pmaple/bin.X86_64_WINDOWS/pmaple.exe"
#	    , "bin.APPLE_UNIVERSAL_OSX/pmaple" = "pmaple/bin.APPLE_UNIVERSAL_OSX/pmaple"

INSTALLER := true

MLA-DEPENDS := $(wildcard maple/Install/*)

include MapleLisp.mk

# {{{ prebuilt

help: $(call print-separator)

.PHONY: prebuilt

prebuilt-zip := $(PKG)-built.zip

help: $(call print-help,prebuilt,Build $(prebuilt-zip))

prebuilt: $(prebuilt-zip)

doc-files := $(addprefix doc/$(PKG).,html info pdf)

$(prebuilt-zip): $(prebuilt-files)
	$(RM) $@
	zip $@ $(doc-files)
	[ -d lib ] || mkdir lib
	cp $(PKG).mla $(PKG).help lib
	zip $@ lib/*
	cd pmaple ; zip ../$@ bin*/*

# }}}



