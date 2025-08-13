export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask emacs -batch -L . -L test \
	--eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile $$(cask files); \
	(ret=$$? ; cask clean-elc && exit $$ret)
.PHONY: test coverage
test:
	rm -rf coverage
	cask exec emacs --no-init-file --no-site-file -batch -L . -l buttercup -f buttercup-run-discover


tests: test

coverage: test
	genhtml -o coverage/ coverage/lcov.info

.PHONY: build-svelte-calendar
build-svelte-calendar:
	cd svelte-calendar && npm run build
	rm -rf calendar/static calendar/_app calendar/*.html calendar/*.js calendar/*.css
	cp -r svelte-calendar/dist/* calendar/
	cp svelte-calendar/public/index.html calendar/
	@echo "Static svelte calendar built and copied to calendar/ directory"
