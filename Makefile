all: format build 
folders: 
	elm make src/PhotoFolders.elm --output app.js --debug

dev: 
	elm make src/PhotoGroove.elm --output app.js --debug
format: 
	elm-format . --yes 
build: 
	elm make src/PhotoGroove.elm --output app.js
