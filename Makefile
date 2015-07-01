
SRC=$(shell find src -name '*.hs')

FLAGS=--enable-tests

all: init test docs package

init: stack.yaml

stack.yaml:
	stack init --prefer-nightly

test:
	stack test

run:
	stack exec -- taygeta


# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# install:
# generate executable and put it into `/usr/local`
#
# deploy:
# prep and push

tags: ${SRC}
	codex update

lint:
	hlint src specs

clean:
	stack clean
	codex cache clean

distclean: clean

build:
	stack build

watch:
	ghcid "--command=stack ghci"

restart: distclean init build

rebuild: clean build

.PHONY: all init test run clean distclean build rebuild lint watch
