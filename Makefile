.PHONY: main run convert clean

all:
	stack build

run: all
	stack exec graphics-exe

convert: run
	convert pic.pbm pic.png

clean:
	rm -f pic.pbm pic.png
