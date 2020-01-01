default: all

.PHONY: clean all static

clean:
	rm -rf build/*

all: static banana

static: 
	cp -v static/index.html build/index.html
	cp -v static/*.png build/
	cp -v static/*.ico build/
	cp -v static/site.webmanifest build/

banana: css
	elm make src/Banana.elm --output build/banana.js

css:
	sass static/style.sass build/style.css

publish: css
	cp -v static/banana.png build/banana.png
	elm make src/Banana.elm --optimize --output=build/banana.js
	uglifyjs build/banana.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=build/banana.min.js
	cp -v static/index.min.html build/index.html
