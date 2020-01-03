BINS := $(shell find bin -mindepth 1 -maxdepth 1 -type f -printf "%f")

install: link build

build:
	@xmonad --recompile

link:
	@ln -sfv $(CURDIR) $(HOME)/.xmonad
	@ln -sfv $(CURDIR)/.xmobarrc $(HOME)/.xmobarrc
	@for f in $(BINS); do ln -sfv "$(CURDIR)/bin/$${f}" ~/bin; done

unlink:
	@unlink $(HOME)/.xmonad
	@unlink $(HOME)/.xmobarrc
	@for f in $(BINS); do unlink ~/bin/"$${f}"; done
