INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: all
all:
	jbuilder build @all

.PHONY: install
install:
	jbuilder install $(INSTALL_ARGS)

.PHONY: uninstall
uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

.PHONY: reinstall
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

.PHONY: test
test:
	jbuilder runtest

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
	jbuilder build --workspace jbuild-workspace.dev

.PHONY: clean
clean:
	jbuilder clean
