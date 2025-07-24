# Emacs configuration build system
EMACS := emacs
BATCH := $(EMACS) --batch --no-init-file
CONFIG_ORG := configuration.org
CONFIG_EL := configuration.el
CONFIG_ELC := configuration.elc
INIT_EL := init.el

# Extract packages from configuration.org dynamically (only actual use-package calls)
PACKAGES := $(shell grep 'use-package' configuration.org | grep -o 'use-package [a-zA-Z0-9-]*' | awk '{print $$2}' | sort -u | grep -v '^org$$' | grep -v '^such$$')

.PHONY: all clean install-packages compile test help auto-compile

all: install-packages compile

# Auto-compile only if configuration.org is newer than configuration.elc
auto-compile: $(CONFIG_ELC)

# Install all required packages (extracted dynamically from configuration.org)
install-packages:
	@echo "Found packages: $(PACKAGES)"
	@echo "Installing required packages..."
	@for pkg in $(PACKAGES); do \
		if grep -q ":load-path.*$$pkg" configuration.org; then \
			echo "Skipping $$pkg (local package)"; \
		else \
			echo "Installing $$pkg..."; \
			$(BATCH) --eval "(progn \
				(require 'package) \
				(setq package-archives \
					'((\"gnu elpa\" . \"https://elpa.gnu.org/packages/\") \
					  (\"melpa\" . \"https://melpa.org/packages/\") \
					  (\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))) \
				(package-initialize) \
				(unless package-archive-contents \
					(package-refresh-contents)) \
				(condition-case err \
					(unless (package-installed-p '$$pkg) \
						(package-install '$$pkg) \
						(message \"Installed $$pkg\")) \
				  (error (message \"Failed to install $$pkg: %s\" err))))" || true; \
		fi; \
	done
	@echo "Package installation completed"

# Tangle configuration.org to configuration.el
$(CONFIG_EL): $(CONFIG_ORG)
	@echo "Tangling configuration.org..."
	$(BATCH) --eval "(progn \
		(require 'org) \
		(org-babel-tangle-file \"$(CONFIG_ORG)\"))" && \
	$(BATCH) --eval "(progn \
		(let ((content (with-temp-buffer \
			(insert-file-contents \"$(CONFIG_EL)\") \
			(buffer-string)))) \
		(with-temp-file \"$(CONFIG_EL)\" \
			(insert \";;; -*- lexical-binding: t; -*-\\n\") \
			(insert content))))"

# Compile configuration.el to configuration.elc (depends on configuration.org)
$(CONFIG_ELC): $(CONFIG_ORG) install-packages
	@if [ ! -f $(CONFIG_EL) ] || [ $(CONFIG_ORG) -nt $(CONFIG_EL) ]; then \
		echo "Tangling configuration.org (source changed)..."; \
		$(BATCH) --eval "(progn \
			(require 'org) \
			(org-babel-tangle-file \"$(CONFIG_ORG)\"))"; \
		$(BATCH) --eval "(progn \
			(let ((content (with-temp-buffer \
				(insert-file-contents \"$(CONFIG_EL)\") \
				(buffer-string)))) \
			(with-temp-file \"$(CONFIG_EL)\" \
				(insert \";;; -*- lexical-binding: t; -*-\\n\") \
				(insert content))))"; \
	fi
	@echo "Compiling configuration.el..."
	$(BATCH) --eval "(progn \
		(require 'package) \
		(package-initialize) \
		(setq byte-compile-warnings '(not obsolete)) \
		(byte-compile-file \"$(CONFIG_EL)\"))"

# Compile everything
compile: $(CONFIG_ELC)
	@echo "Compilation complete!"

# Test startup time
test:
	@echo "Testing startup time..."
	@echo "Vanilla Emacs:"
	@time $(EMACS) --batch --eval "(kill-emacs)" 2>/dev/null
	@echo "With configuration:"
	@time $(EMACS) --batch --load $(INIT_EL) --eval "(kill-emacs)" 2>/dev/null

# Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	rm -f $(CONFIG_EL) $(CONFIG_ELC)
	rm -f *.elc

# Show help
help:
	@echo "Emacs Configuration Build System"
	@echo ""
	@echo "Available targets:"
	@echo "  all              - Install packages and compile configuration"
	@echo "  auto-compile     - Auto-recompile only if configuration.org changed"
	@echo "  install-packages - Install all required packages"
	@echo "  compile          - Force tangle and compile configuration"
	@echo "  test             - Test startup time"
	@echo "  clean            - Remove compiled files"
	@echo "  help             - Show this help"
	@echo ""
	@echo "Smart recompilation:"
	@echo "  - init.el automatically calls 'make auto-compile' when needed"
	@echo "  - Only recompiles if configuration.org is newer than configuration.elc"
	@echo ""
	@echo "Usage: make [target]"