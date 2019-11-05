all: static-binaries


static-binaries:
	rm -rf dist
	mkdir -p dist/mac
	mkdir -p dist/linux

	# Mac
	stack build
	cp .stack-work/install/x86_64-osx/lts-12.16/8.4.4/bin/elm-proofread ./dist/mac/elm-proofread

	# Linux
	docker build -t bin .
	docker cp $$(docker create bin):/tmp/dist-newstyle/build/x86_64-linux/ghc-8.4.3/elm-proofread-0.2.0/x/elm-proofread/build/elm-proofread/elm-proofread ./dist/linux/
	chmod +x ./dist/linux/elm-proofread
	docker system prune -a

	# Tar files
	tar czf dist/mac-64bit.tar.gz -C dist/mac/ .
	tar czf dist/linux-64bit.tar.gz -C dist/linux/ .
