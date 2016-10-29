all:
	rm assets/js/MisterDojo.js
	elm-make src/MisterDojo.elm --output MisterDojo.js
	mv MisterDojo.js assets/js/
