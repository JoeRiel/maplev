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

# Activate selected make sections
BOOK  := true
CLOUD := true

include MapleLisp.mk

