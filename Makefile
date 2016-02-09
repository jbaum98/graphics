.PHONY: gen_pic
gen_pic:
	ghc -O2 --make gen_pic.hs

run: gen_pic
	./gen_pic && convert pic.pbm pic.png

clean:
	rm -f gen_pic *.hi *.o
