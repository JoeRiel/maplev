base = maplev
installdir = ~/emacs

#emacs	= emacs --debug-init --no-site-file --no-init-file --eval '(setq debug-on-error t)'
#emacs	= emacs --debug-init --no-site-file --eval '(setq debug-on-error t)'
#emacs	= emacs --debug-init --no-init-file --eval '(setq debug-on-error t)'
emacs = emacs

# ELFLAGS	= --eval '(setq load-path (append (list "." "$(elibdir)" "$(lispdir)") load-path))'

ELFLAGS = 
ELC	= $(emacs) --batch $(ELFLAGS) --funcall=batch-byte-compile

elfile  = $(base).el
elcfile = $(elfile:.el=.elc)

installed_elfile  = $(addprefix $(installdir)/, $(elfile))
installed_elcfile = $(installed_elfile:.el=.elc)

default: elcfile installedfiles
install: installedfiles
elcfile: $(elcfile)
dist: $(base).zip

%.elc : %.el
	$(ELC) $<

$(installed_elfile): $(elfile)
	cp $< $@

$(installed_elcfile): $(elcfile)
	cp $< $@

installedfiles: $(installed_elfile) $(installed_elcfile)

$(base).zip: $(elfile) doc/$(base).texi
	zip $@ $?

.PHONY: elcfile installedfiles default install dist