main.exe: solve.hs lex.hs parse.hs
	ghc solve.hs -o main.exe -O2 -outputdir out
lex.hs: lexer.x
	alex lexer.x -o lex.hs
	
parse.hs: lex.hs parse.y
	happy parse.y -o parse.hs

run: main.exe
	main.exe