
# extract the version number from maplev.el
version := $(shell head --lines=20 maplev.el | grep 'Version:' | sed 's/^[^0-9]*//' )

xemacs-dir := ~/elisp/xemacs21/maplev

default: get

# download maplev.el and ChangeLog from my website.
# wget will add a numbered suffix to uniquely identify the files
get:
	wget \
	http://www.k-online.com/~joer/maplev/maplev.el \
	http://www.k-online.com/~joer/maplev/ChangeLog


X:
	xemacs -nw -batch -f batch-byte-compile maplev.el 2> compilation.log.x



# copy elisp file to xemacs directory
xemacs: $(xemacs-dir)/maplev.el

$(xemacs-dir)/maplev.el: maplev.el
	cp $< $@

getxemacs:
	cp $(xemacs-dir)/maplev.el .

.PHONY: get xemacs default X
