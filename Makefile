watch:
	nix run .#site-builder -- watch

clean:
	nix run .#site-builder -- clean

build:
	nix run .#site-builder -- build
