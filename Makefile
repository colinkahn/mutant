.PHONY: test

test-all: test ex-mutant

ex-test:
	cd example && \
		clj -Sforce -Srepro -A:test -m cognitect.test-runner -d test

ex-mutant:
	cd example && \
		clj -Sforce -Srepro -A:test:mutant -m mutant.test-runner -t test -s src

test-old:
	clj -Sforce -Srepro -A:old-test -m cognitect.test-runner -d old-test

test:
	clj -Sforce -Srepro -A:test -m cognitect.test-runner -d test

test-watch:
	clj -Sforce -Srepro -A:test -m kaocha.runner --config-file tests/test.edn --watch

test-focus:
	clj -Sforce -Srepro -A:test -m kaocha.runner --config-file tests/test.edn --watch --focus-meta focus
