
.PHONY: clean

clean:
	rm -rf build/*

banana: css
	cp -v static/index.html build/index.html
	cp -v static/banana.png build/banana.png
	elm make src/Banana.elm --output build/banana.js

css:
	sass static/style.sass build/style.css
