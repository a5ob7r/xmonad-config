install: link build

build:
	@xmonad --recompile

link:
	@ln -sfv $(CURDIR) $(HOME)/.xmonad
	@ln -sfv $(CURDIR)/.xmobarrc $(HOME)/.xmobarrc

unlink:
	@unlink $(HOME)/.xmonad
	@unlink $(HOME)/.xmobarrc
