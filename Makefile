examples: examples/simple_c_lexer

./examples/simple_c_lexer: examples/simple_c_lexer.c lca_lexer.h
	cc -o $@ $< -I .

.PHONY: examples
