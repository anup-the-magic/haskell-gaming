dev:
	cd client; npx elm-live -- src/Main.elm --debug

watch:
	cd client;\
	  while sleep 0.5;\
		do find src \
		| entr -crsd 'elm make --output=/dev/null **/*.elm && echo Press ctrl-c twice to exit!';\
	done;

serve:
	cd satisfactory-hs/data; npx http-server -p 8080 --cors
