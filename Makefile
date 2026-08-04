EMACS ?= emacs
ELISP_FILES := early-init.el init.el $(wildcard lisp/*.el)

.PHONY: check check-files check-startup

check: check-files check-startup

check-files:
	@for file in $(ELISP_FILES); do \
		$(EMACS) --batch -Q \
			--eval "(progn (require 'checkdoc) (find-file \"$(CURDIR)/$$file\") (check-parens) (checkdoc-current-buffer t))" \
			>/dev/null || exit 1; \
	done
	@echo "Elisp syntax and documentation checks passed."

check-startup:
	@$(EMACS) --batch -Q \
		--eval "(setq user-emacs-directory \"$(CURDIR)/\")" \
		--load early-init.el \
		--load init.el
	@echo "Clean batch startup passed."
