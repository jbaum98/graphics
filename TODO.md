- [X] Remove State and symbol table from Parser
- [x] Implement interpreter, using symbol table
- [ ] Add error handling to interpreter
- [ ] Add error handling using error monad in parser
- [ ] Reimplement error handling in lexer using error monad, hopefully the same way
- [ ] Remove Comment from Expr
- [x] Colors and sizes
- [ ] Instead of Picture -> Picture, try generalizing the [D2Point] approach for easaier testing
- [x] Use rewrite rules to make solidPic black and white faster
- [ ] Use an MVar to store the last spawned process and wait for it before starting a new one
      That way you can do other stuff but you won't display multiple in a row
- [ ] Use par instead of forkIO to evaluate each picture in parallel

- [ ] add script
- [ ] add docs
