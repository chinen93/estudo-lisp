EMACS=emacs

.PHONY: test-batch

test-batch:
	${EMACS} -Q --batch -L . -l test/all.el \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"
