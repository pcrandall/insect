currentDir = $(shell pwd)

ifeq '$(findstring ;,$(PATH))' ';'
    detected_OS := Windows
else
    detected_OS := $(shell uname 2>/dev/null || echo Unknown)
endif

ifeq ($(detected_OS),Windows)
	cmd := ${currentDir}/scripts/cp_neutralino.bat
	NEU := $(where neu)
else
	cmd := ${currentDir}/scripts/cp_neutralino.sh
	NEU := $(shell command -v neu 2> /dev/null)
endif

install:
ifndef NEU
	@echo "$(NEU)"
	@echo "==> installing neutralino js"
	@npm install -g @neutralinojs/neu
endif
.PHONY: install

update:
	@echo "$(NEU)"
	@echo "==> getting latest insect.js from https://insect.sh/insect.js"
	@${currentDir}/scripts/backup.sh
	@wget -O app/insect.js https://insect.sh/insect.js 2>/dev/null
	@wget -O app/jquery.min.js https://insect.sh/jquery.min.js 2>/dev/null
	@wget -O app/jquery.mousewheel-min.js https://insect.sh/jquery.mousewheel-min.js 2>/dev/null
	@wget -O app/jquery.terminal.min.js https://insect.sh/jquery.terminal.min.js 2>/dev/null
	@wget -O app/keyboardevent-key-polyfill.js https://insect.sh/keyboardevent-key-polyfill.js 2>/dev/null
.PHONY: update

run:
	@echo "==> running insect"
	@yarn tauri dev
.PHONY: run

build:
	@echo "==> building insect"
	@yarn tauri build
.PHONY: build

release:
	@echo "==> making release"
	@${currentDir}/scripts/release.sh
.PHONY: release

git:
	@echo "==> adding git tracked files"
	@git add -u
	@git commit
	@echo "==> pushing to git remote"
	@git push origin
.PHONY: git

clean:
	@git clean -f
.PHONY: clean
