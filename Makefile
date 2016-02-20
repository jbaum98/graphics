PIC_NAME := pic
PPM_NAME := $(addsuffix .ppm, $(PIC_NAME))
PNG_NAME := $(addsuffix .png, $(PIC_NAME))

all:
	stack build || stack setup && stack build

run: all
	stack exec graphics-exe $(PPM_NAME)

convert: run
	convert $(PPM_NAME) $(PNG_NAME)

doc:
	stack exec which HsColour >/dev/null || stack install hscolour
	stack haddock

doc-view: doc
	DOC_PATH="$$(stack path --local-doc-root)/index.html"; echo "\nOpening $$DOC_PATH in your browser...\n";\
	open "$$DOC_PATH" 2>/dev/null || xdg-open "$$DOC_PATH" 2>/dev/null

clean:
	stack clean
	rm -f *.ppm *.png
