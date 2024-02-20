#define LCA_LEXER_IMPLEMENTATION
#include "lca_lexer.h"

#include <stdio.h>

const char* source_text = "int main() {\n"
//"    fprintf(stderr, \"This is a number: %d\\n\", 69);\n"
"    fprintf(stderr, );\n"
"}\n";

int main() {
    lca_lexer lexer = {0};
    lca_lexer_init(&lexer, source_text, NULL);

    while (!lca_lexer_is_at_end(&lexer)) {
        lca_lexer_token token = lca_lexer_read_c_token(&lexer);
        fprintf(stderr, "[%s] %.*s\n", lca_lexer_c_token_kind_to_string(token.kind), (int)(token.source_end - token.source_begin), token.source_begin);
    }

    return 0;
}
