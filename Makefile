DOC_PATH := $(shell stack path --local-doc-root)/index.html

all:
	stack install || (stack setup && stack install)

run: all
	bin/graphics-exe $(PPM_NAME)

doc:
	stack exec which HsColour >/dev/null || stack install hscolour
	stack haddock
	@echo "\nOpen $(DOC_PATH) in your browser or run \`make doc-view\`\n"

doc-view: doc
	@echo "\nOpening $(DOC_PATH) in your browser...\n"
	@open $(DOC_PATH) 2>/dev/null || xdg-open $(DOC_PATH) 2>/dev/null

bench:
	stack bench --benchmark-arguments '-o benchmark.html'

clean:
	stack clean
	rm -f *.ppm *.png *.html *.prof bin/*
