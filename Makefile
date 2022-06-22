currentDir = $(shell pwd)

# Put icon for your app at the root of the project and name it app-icon.png
icon:
	@echo "==> generating icons"
	@yarn tauri icon
.PHONY: icon

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
