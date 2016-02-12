.PHONY: run convert clean

main:
	ghc -O2 -ilib --make Main.hs -o main

run: pic.pbm
pic.pbm: main
	./main

convert: pic.png
pic.png: pic.pbm
	convert pic.pbm pic.png

clean:
	rm -f gen_pic *.hi *.o
