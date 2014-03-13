PACKAGE = pungi
EMACS = $(shell which emacs)
VERSION = $(shell cask version)
DIST_FILE = pungi.el
DIST_DIR = dist/${PACKAGE}-${VERSION}

clean:
	rm -rf dist/${PACKAGE}-${VERSION}

dist-clean:
	rm -rf dist

${PACKAGE}-${VERSION}: dist/${PACKAGE}-${VERSION}

dist/${PACKAGE}-${VERSION}:
	mkdir -p $@
	cp -v ${DIST_FILE} $@

install:
	${EMACS} --batch -nw -Q \
		--load package \
		--eval "(setq package-archives ())" \
		--eval "(add-to-list 'package-archives \
					'(\"marmalade\" . \"http://marmalade-repo.org/packages/\"))" \
		--eval "(add-to-list 'package-archives \
					'(\"melpa\" . \"http://melpa.milkbox.net/packages/\"))" \
		--eval '(list-packages)' \
		--eval '(package-install-file "${PWD}/dist/${PACKAGE}-${VERSION}/${DIST_FILE}")' \
		--eval "(when (require 'jedi nil :noerr) (jedi:install-server))"
