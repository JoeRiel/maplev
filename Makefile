# Makefile - for the maplev distribution
#
# Maintainer: Joe Riel <joer@san.rr.com>

# include version.mk

PKG := maplev

VERSION := 3.2.0

CLOUD-ID := 4677254699810816
CLOUD-DESCRIPTION := An Emacs mode for Maple developers
CLOUD-GROUP := packages
CLOUD-VERSION := 7
CLOUD-TITLE := An Emacs-based debugger for Maple

# Activate selected make sections

BOOK  := true
CLOUD := true
USE-MAPLE := true

BOOK-FILES := doc/${PKG}.html doc/${PKG}.pdf
BOOK-FILES += $(wildcard maple/src/*.mm)
BOOK-FILES += $(wildcard maple/src/*.mpl)
BOOK-FILES += $(wildcard maple/include/*)

BOOK-MAP := , "bin.X86_64_LINUX/pmaple"        = "pmaple/bin.X86_64_LINUX/pmaple"\
            , "bin.X86_64_WINDOWS/pmaple.exe"  = "pmaple/bin.X86_64_WINDOWS/pmaple.exe"
#            , "bin.APPLE_UNIVERSAL_OSX/pmaple" = "pmaple/bin.APPLE_UNIVERSAL_OSX/pmaple"

BOOK-EXECUTABLES := bin.X86_64_LINUX/pmaple

MLA-DEPENDS := $(wildcard maple/Install/*)

LINEINFO_RELPATH := true

PKG-EXTRA := README

LISP-DIR := $(HOME)/.emacs.d/elpa/maplev-$(VERSION)

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



