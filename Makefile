INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	@jbuilder build @install

install:
	@jbuilder install $(INSTALL_ARGS)

uninstall:
	@jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

doc:
	@jbuilder build @doc

test:
	@jbuilder runtest --no-buffer

bench:
	@jbuilder build @runbench --no-buffer

clean:
	@jbuilder clean

.PHONY: default install uninstall reinstall doc test bench clean
