NEU := $(shell command -v neu 2> /dev/null)
currentDir = $(shell pwd)

install:
ifndef NEU
	@echo "==> installing neutralino js"
	@npm install -g @neutralinojs/neu
endif
.PHONY: install

run:
	@echo "==> running insect"
	@neu run
.PHONY: run

build:
	@echo "==> building insect"
	@cp ./bin/neutralino.js ./resources/js/
	@neu build
.PHONY: build

git:
	@echo "==> adding git tracked files"
	@git add -u
	@git commit
	@echo "==> pushing to git remote"
	@git push origin
.PHONY: git

clean:
	@go clean --cache
	@go mod tidy
	@git clean -f
.PHONY: clean
