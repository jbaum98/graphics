all:
	stack build || stack setup && stack build

run: all
	stack exec graphics-exe

convert: run
	convert pic.pbm pic.png

doc:
	stack exec which HsColour >/dev/null || stack install hscolour
	stack haddock

doc-view: doc
	DOC_PATH="$$(stack path --local-doc-root)/index.html"; echo "\nOpening $$DOC_PATH in your browser...\n";\
	open "$$DOC_PATH" 2>/dev/null || xdg-open "$$DOC_PATH" 2>/dev/null

clean:
	stack clean
	rm -f pic.pbm pic.png
