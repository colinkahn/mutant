.PHONY: test

ex-test:
	cd example && \
		clj -A:test -m cognitect.test-runner -d test

ex-mutant:
	cd example && \
		clj -A:test:mutant -m mutant.test-runner -t test -s src

test-old:
	clj -A:old-test -m cognitect.test-runner -d old-test

test:
	clj -A:test -m cognitect.test-runner -d test

test-watch:
	clj -A:test -m kaocha.runner --config-file tests/test.edn --watch

test-focus:
	clj -A:test -m kaocha.runner --config-file tests/test.edn --watch --focus-meta focus
