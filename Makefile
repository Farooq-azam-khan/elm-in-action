all: format main dev 
dev: 
	http-server-spa .
main: 
	elm make src/Main.elm --output app.js --debug
format: 
	elm-format . --yes 
