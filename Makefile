all: format build 
dev: 
	http-server-spa .
folders: 
	elm make src/PhotoFolders.elm --output app.js --debug
main: 
	elm make src/Main.elm --output app.js --debug
photo_groove: 
	elm make src/PhotoGroove.elm --output app.js --debug
format: 
	elm-format . --yes 
build: 
	elm make src/PhotoGroove.elm --output app.js
