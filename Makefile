base = maplev
installdir = ~/emacs
CP = cp

#emacs	= emacs --debug-init --no-site-file --no-init-file --eval '(setq debug-on-error t)'
#emacs	= emacs --debug-init --no-site-file --eval '(setq debug-on-error t)'
#emacs	= emacs --debug-init --no-site-file --eval '(setq debug-on-error t)'
emacs = emacs --no-site-file

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

.PHONY: p4put p4get
p4dir = /home/joe/work/MapleSoft/sandbox/groups/share/emacs/$(base)
p4put: $(base).el 
	(cd $(p4dir); p4 edit $?)
	$(CP) $? $(p4dir)

p4get: 
	$(CP) $(pfdir)/. .

.PHONY: elcfile installedfiles default install dist