.DELETE_ON_ERROR: # Remove artifacts on failure

SHELL = /bin/bash

CLOJARS_USERNAME ?= cch1
srcfiles = $(shell find src/ -type f -name '*.clj' -or -name '*.cljc' -or -name '*.cljs' -or -name '*.edn')
testfiles = $(shell find test/ -type f -name '*.clj' -or -name '*.cljc' -or -name '*.cljs' -or -name '*.edn')

target = ./target
jar-file = $(target)/zipii-$(VERSION).jar
pom-file = $(target)/zipii-$(VERSION).pom.xml

# This is the default target because it is the first real target in this Makefile
.PHONY: default # Same as "make jar"
default: jar

all: test lint jar

# https://github.com/git/git/blob/9b88fcef7dd6327cc3aba3927e56fef6f6c4d628/GIT-VERSION-GEN
# NB: since the following recipe name matches the following include, the recipe is *always* run and VERSION is always set. Thank you, make.
# NB: the FORCE dependency here is critical.
.PHONY: FORCE
.make.git-version-file: FORCE
	@$(SHELL) ./bin/vgit $@
-include .make.git-version-file
export VERSION

.PHONY: version # Report the git version used to tag artifacts
version:
	@echo $(VERSION)

.PHONY: assert-clean # Fail if the git repo is dirty (untracked files, modified files, or files are in the index)
assert-clean:
ifeq ($(DRO),true) # Dirty Repository Ok
	@echo "Skipping dirty repo check"
else
	@test -z "$$(git status --porcelain)"
endif

$(target)/:
	mkdir -p $@

.PHONY: test # Run the Clojure and ClojureScript test suites
test: .make.test-clj .make.test-cljs

.make.test-clj: deps.edn $(testfiles) $(srcfiles)
	clojure -M:test:project/test-clj
	touch .make.test-clj

.make.test-cljs: deps.edn $(testfiles) $(srcfiles)
	clojure -M:test:project/test-cljs
	touch .make.test-cljs

.PHONY: lint # Lint the source code
lint: .make.lint

.make.lint: $(srcfiles)
	clojure -M:lint/kondo ; test $$? -lt 3
	touch .make.lint

$(pom-file): deps.edn | $(target)/
	clojure -M:project/pom --force-version $(VERSION)
	mv pom.xml $(DESTDIR)$@

$(jar-file): deps.edn $(pom-file) $(shell find src/ -type f -or -name '*.clj' -name '*.cljc') | $(target)/
	clojure -X:project/jar :pom-file \"$(DESTDIR)$(pom-file)\" :jar \"$@\"

.PHONY: pom # Create the pom.xml file
pom: $(pom-file)

.PHONY: jar # Build the jar file
jar: assert-clean $(jar-file)

install: $(jar-file)
	clojure -X:deps mvn-install :jar \"$(jar-file)\"

.PHONY: deploy # Deploy the jar file to Clojars
deploy: $(jar-file)
	env CLOJARS_USERNAME=$(CLOJARS_USERNAME) CLOJARS_PASSWORD=$(CLOJARS_PASSWORD) clojure -M:project/clojars $(jar-file)

clean:
	rm -f pom.xml pom.xml.asc zipii.jar
	rm -rf target/*
	rm -rf cljs-test-runner-out
	rm -f .make.*

# Copied from: https://github.com/jeffsp/makefile_help/blob/master/Makefile
# Tab nonesense resolved with help from StackOverflow... need a literal instead of the \t escape on MacOS
help: # Generate list of targets with descriptions
	@grep '^.PHONY: .* #' Makefile | sed 's/\.PHONY: \(.*\) # \(.*\)/\1	\2/' | expand -t20
