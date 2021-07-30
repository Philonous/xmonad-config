sourcefiles:=$(shell find source -type f)

xmonad-x86_64-linux: xmonad
	ln -sfT xmonad xmonad-x86_64-linux

xmonad: $(sourcefiles) stack.yaml my-xmonad.cabal
	stack install --ghc-options -j --local-bin-path $(PWD)
