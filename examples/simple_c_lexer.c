#define NO_LCA
#define LCA_IMPLEMENTATION

#if !defined(NO_LCA)
#    include "lca.h"
#endif

#define LCA_LEXER_LANGUAGE_C
#include "lca_lexer.h"

#include <stdio.h>

const char* source_text =
    "int main() {\n"
    //"    fprintf(stderr, \"This is a number: %d\\n\", 69);\n"
    "    fprintf(stderr, );\n"
    "}\n";

int main() {
    lca_lexer lexer = {0};
    lca_lexer_init(&lexer, "<source>", -1, source_text, NULL);

    lca_lexer_cpp cpp = {0};
    lca_lexer_cpp_init(&cpp);

    while (!lca_lexer_is_at_end(&lexer)) {
        lca_lexer_c_token token = lca_lexer_read_c_token(&lexer, &cpp);
        fprintf(
            stderr,
            "[%s] %.*s\n",
            lca_lexer_c_token_kind_to_string(token.kind),
            (int)token.source_text.count,
            token.source_text.data
        );
    }

    return 0;
}
