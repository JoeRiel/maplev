# -*- mode: makefile-gmake -*-

help:
	@echo $(if $(need-help),,Type \'$(MAKE)$(dash-f) help\' to get help)
	@echo
	@echo 'To install, do'
	@echo '$$ sudo make install'
	@echo '$$ make install-maple'

need-help := $(filter help,$(MAKECMDGOALS))

define print-help
$(if $(need-help),$(warning $1 -- $2))
endef

define last-element
$(lastword $1)
endef

this-makefile   := $(call last-element,$(MAKEFILE_LIST))
other-makefiles := $(filter-out $(this-makefile),$(MAKEFILE_LIST))
parent-makefile := $(call last-element,$(other-makefiles))

dash-f := $(if $(filter-out Makefile makefile GNUmakefile,\
$(parent-makefile)), -f $(parent-makefile))

.PHONY: help 