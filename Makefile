all: format build 
dev: 
	elm make src/PhotoGroove.elm --output app.js --debug
format: 
	elm-format . --yes 
build: 
	elm make src/PhotoGroove.elm --output app.js
