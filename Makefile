
.PHONY: clean

clean:
	rm -rf build/*

banana:
	cp -v static/index.html build/index.html
	elm make src/banana.elm --output build/banana.js