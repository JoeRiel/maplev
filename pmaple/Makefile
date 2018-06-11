# Makefile - for the pmaple subproject

SHELL := /bin/bash

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

.PHONY: linux windows

CC 	:= gcc
MINGW   := x86_64-w64-mingw32-gcc
RM 	:= rm -rf
MAPLE 	:= maple
SYSTYPE := $(shell ./maple.system.type)
LIB     := $(MAPLE_ROOT)/internal/$(SYSTYPE)
LIBS 	:= -L $(LIB)
INCLUDE := -I $(MAPLE_ROOT)/extern/include
#ENV     := LD_LIBRARY_PATH=$(LIB) MAPLE=$(MAPLE)
ENV     := LD_LIBRARY_PATH=$(shell ./getlibpath)

$(info LIBS=$(LIBS))

#CFLAGS  := -m32
CFLAGS  := -g -D_GNU_SOURCE -std=c11 -pedantic-errors

linux: pmaple
windows: pmaple.exe

linux: $(call print-help,linux,	Create pmaple binary)
windows: $(call print-help,windows,	Create pmaple.exe binary)

pmaple: pmaple.c getdelim.c
	@echo "compiling and linking $< ..."
	$(ENV) $(CC) $< -o $@ $(INCLUDE) $(CFLAGS) $(LIBS) -l maplec

pmaple.exe: pmaple.c getdelim.c $(MAPLEC-DLL) 
	@echo "compiling and linking $< ..."
	$(ENV) $(MINGW) $< -o $@ $(INCLUDE) $(CFLAGS) -L . -lmaplec

# }}}
# {{{ install

.PHONY: install

install: $(call print-help,install,	Install the binaries and executables into $(BIN-DIR))

install: pmaple
	install -D --target-directory=$(BIN-DIR) $+

# }}}
