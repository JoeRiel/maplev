# Makefile - for the pmaple subproject

SHELL := /bin/bash

UNAME := $(shell uname)

include help-system.mak

# {{{ Directories

LISP-DIR := $(HOME)/.emacs.d/maple
BIN-DIR := $(LISP-DIR)/bin

# }}}

# {{{ noweb

.PHONY: pdf

pdf: $(call print-help,pdf,	Create pmaple.pdf)
pdf: pmaple.pdf

%.c: %.nw
	notangle -R$@ $< > $@

%.tex: %.nw
	noweave \
	-filter stripmodeline \
	-filter 'inlinecomments commentre="/[*]" commentshow="/*"' \
	-delay \
	-autodefs c \
	-index \
	$< > $@

%.pdf: %.tex %.aux
	pdflatex $<

%.aux: %.tex
	pdflatex $<

# }}}
# {{{ pmaple

.PHONY: linux mac windows

CC 	:= gcc
MINGW   := x86_64-w64-mingw32-gcc
RM 	:= rm -rf
MAPLE 	:= maple
SYSTYPE := $(shell ./maple.system.type)
LIB     := $(MAPLE_ROOT)/internal/$(SYSTYPE)
LIBS 	:= -L $(LIB)
INCLUDE := -I $(MAPLE_ROOT)/extern/include

ifeq ($(UNAME),Linux)
LIB-PATH-VAR := LD_LIBRARY_PATH
endif
ifeq ($(UNAME),Darwin)
LIB-PATH-VAR := DYLD_LIBRARY_PATH
endif

ENV     := $(LIB-PATH-VAR)=$(shell ./getlibpath)
CFLAGS  := -g -D_GNU_SOURCE -pedantic-errors

linux:   $(SYSTYPE)/pmaple
mac:     $(SYSTYPE)/pmaple
windows: bin.X86_64_WINDOWS/pmaple.exe

linux: $(call print-help,linux,	Create pmaple binary)
mac:   $(call print-help,mac,	Create pmaple binary)
windows: $(call print-help,windows,	Create pmaple.exe binary)

$(SYSTYPE)/pmaple: pmaple.c getdelim.c
	mkdir -p $(SYSTYPE)
	@echo "compiling and linking $< ..."
	$(ENV) $(CC) $< -o $@ $(INCLUDE) $(CFLAGS) $(LIBS) -l maplec

bin.X86_64_WINDOWS/pmaple.exe: pmaple.c getdelim.c $(MAPLEC-DLL) 
	@echo "compiling and linking $< ..."
	$(ENV) $(MINGW) $< -o $@ $(INCLUDE) $(CFLAGS) -L . -lmaplec

# }}}
# {{{ install

.PHONY: install

install: $(call print-help,install,	Install the binaries and executables into $(BIN-DIR))

install: pmaple
	install -D --target-directory=$(BIN-DIR) $+

# }}}
