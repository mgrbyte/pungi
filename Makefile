PACKAGE = pungi
EMACS = $(shell which emacs)
VERSION = $(shell cask version)
DIST_FILES = pungi.el
DIST_DIR = dist/${PACKAGE}-${VERSION}
PACKAGE_DIR = $(cask package-directory)

clean:
	rm -rf dist/${PACKAGE}-${VERSION}

dist-clean:
	rm -rf dist

dist/${PACAKGE}-${VERSION}.tar: ${PACKAGE}-${VERSION}.tar

${PACKAGE}-${VERSION}.tar: ${PACKAGE}-${VERSION}
	tar --directory dist -cvf dist/$@ $<

dist/${PACKAGE}-${VERSION}.tar.gz: ${PACKAGE}-${VERSION}
	tar --directory dist -cvzf dist/$@ $<

${PACKAGE}-${VERSION}: dist/${PACKAGE}-${VERSION}

dist/${PACKAGE}-${VERSION}:
	mkdir -p $@
	cp -v ${DIST_FILES} $@

install:
	${EMACS} --batch -nw -Q \
		--load package \
		--eval "(add-to-list 'package-archives \
					'(\"marmalade\" . \"http://marmalade-repo.org/packages/\"))" \
		--eval "(add-to-list 'package-archives \
					'(\"melpa\" . \"http://melpa.milkbox.net/packages/\"))" \
		--eval "(add-to-list 'package-archives \
					'(\"${PACKAGE}\" . \"${PACKAGE_DIR}\") t)" \
		--eval '(list-packages)' \
		--eval '(package-install-file "${PWD}/dist/${PACKAGE}-${VERSION}.tar")' \
		--eval "(when (require 'jedi nil 'noerror) (jedi:install-server))"
