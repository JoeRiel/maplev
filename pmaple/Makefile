# Makefile - for the pmaple subproject

SHELL := /bin/bash

UNAME := $(shell uname)

MAPLE := /opt/maple2020

include help-system.mk

CC 	:= gcc
MINGW   := x86_64-w64-mingw32-gcc
RM 	:= rm -rf
SYSTYPE := $(shell $(MAPLE)/bin/maple.system.type)
LIB     := $(MAPLE)/$(SYSTYPE)
LIBS 	:= -L $(LIB)
INCLUDE := -I $(MAPLE)/extern/include

# $(info MAPLE=$(MAPLE))
# $(info SYSTYPE=$(SYSTYPE))
# $(info LIB=$(LIB))
# $(info INCLUD=$(INCLUDE))

# {{{ Directories

# LISP-DIR := $(HOME)/.emacs.d/maple
# BIN-DIR := $(LISP-DIR)/bin
BIN-DIR := $(HOME)/maple/toolbox/maplev/$(SYSTYPE)

# }}}

# {{{ noweb

.PHONY: pdf clean

help: $(call print-help,clean,	Delete extracted noweb files)
clean:
	$(RM) pmaple.{aux,c,log,out,pdf,tex}

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


ifeq ($(UNAME),Linux)
LIB-PATH-VAR := LD_LIBRARY_PATH
endif
ifeq ($(UNAME),Darwin)
LIB-PATH-VAR := DYLD_LIBRARY_PATH
endif

ENV     := $(LIB-PATH-VAR)=$(MAPLE)/$(SYSTYPE)
CFLAGS  := -g -D_GNU_SOURCE -pedantic-errors

# $(info ENV=$(ENV), INCLUDE=$(INCLUDE))

C-FILES := pmaple.c getdelim.c
H-FILES := getdelim.h

linux:   $(SYSTYPE)/pmaple
mac:     $(SYSTYPE)/pmaple
windows: bin.X86_64_WINDOWS/pmaple.exe

linux:  $(call print-help,linux,	Create pmaple binary)
mac:    $(call print-help,mac,	Create pmaple binary)
windows: $(call print-help,windows,	Create pmaple.exe binary)


$(SYSTYPE)/pmaple: $(C-FILES) $(H-FILES)
	mkdir -p $(SYSTYPE)
	@echo "compiling and linking $(C-FILES)"
	$(ENV) $(CC) $(C-FILES) -o $@ $(INCLUDE) $(CFLAGS) $(LIBS) -l maplec

bin.X86_64_WINDOWS/pmaple.exe: $(C-FILES) $(H-FILES) $(MAPLEC-DLL)
	mkdir -p $(dir $(@))
	@echo "compiling and linking $(C-FILES)"
	$(ENV) $(MINGW) $(C-FILES) -o $@ $(INCLUDE) $(CFLAGS) -L . -lmaplec


# }}}
# {{{ install

.PHONY: install

install: $(call print-help,install,	Install the binaries and executables into $(BIN-DIR))

install:
	@install --verbose -D --target-directory=$(BIN-DIR) $(SYSTYPE)/pmaple

# }}}
