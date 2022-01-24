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
BOOK-MAP := , "bin.APPLE_UNIVERSAL_OSX/pmaple" = "pmaple/bin.APPLE_UNIVERSAL_OSX/pmaple"\
            , "bin.X86_64_LINUX/pmaple" = "pmaple/bin.X86_64_LINUX/pmaple"\
            , "bin.X86_64_WINDOWS/pmaple.exe" = "pmaple/bin.X86_64_WINDOWS/pmaple.exe"

include MapleLisp.mk

