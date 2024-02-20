#ifndef LCA_LEXER_H
#define LCA_LEXER_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct lca_lexer lca_lexer;
typedef struct lca_lexer_token lca_lexer_token;

typedef void* (*lca_lexer_allocator_function)(void* userdata, size_t nbytes);

/// The type of the lexer's character decoding functions.
typedef bool (*lca_lexer_character_decoder)(const char* data, int data_count, int* out_character, int* out_character_stride);

typedef void (*lca_lexer_error_handler)(const char* source_where_position, int source_character, const char* message);

struct lca_lexer {
    /// Pointer to the start of the source text to lex (its first character).
    const char* source_begin;
    /// Pointer to the end of the source text (one past its last character).
    const char* source_end;

    /// Pointer to allocator userdata. The configured allocator will receive this as its first argument.
    void* allocator_userdata;
    /// The allocator function used by this lexer for allocating string data.
    lca_lexer_allocator_function allocator_function;

    /// The character decoder function, used to read potentially-multibyte characters from the lexer.
    lca_lexer_character_decoder character_decoder;

    /// the current position of the lexer within the source text.
    /// this points to a character within the source text range (between its start and end pointers, inclusive).
    /// note that this may point at the end of the source text, beyond the semantically indexable source text.
    const char* source_current;
    /// The current character, as an integer.
    /// The lexer caches the current character in case decoding is necessary, to avoid duplicating that work.
    int current_character;
    /// The number of bytes that make up the current character.
    /// The lexer caches the current character stride in case decoding is necessary, to avoid duplicating that work.
    /// When the next character is requested, the lexer is advanced forward a number of bytes equal to this stride.
    int current_character_stride;
};

struct lca_lexer_token {
    int kind;

    const char* source_begin;
    const char* source_end;

    int64_t integer_value;
    double float_value;

    const char* string_value;
    size_t string_value_length;
};

typedef struct {
    char* string_buffer;
    size_t buffer_length;
    size_t allocated_count;
} lca_lexer_buffered_allocator_userdata;

/// Initialize the lexer with a given range of source text.
/// This default initialization routine will rely on libc's `malloc` to allocate necessary memory.
void lca_lexer_init(lca_lexer* lexer, const char* source_begin, const char* source_end);
void lca_lexer_init_buffered(lca_lexer* lexer, const char* source_begin, const char* source_end, lca_lexer_buffered_allocator_userdata* buffered_allocator_userdata);
// void lca_lexer_init_buffered(lca_lexer* lexer, const char* source_begin, const char* source_end, char* string_buffer, size_t string_buffer_size);
void lca_lexer_init_allocator(lca_lexer* lexer, const char* source_begin, const char* source_end, void* allocator_userdata, lca_lexer_allocator_function allocator_function);

int lca_lexer_next_character(lca_lexer* lexer);
int lca_lexer_peek_next_character(lca_lexer* lexer);
bool lca_lexer_is_at_end(lca_lexer* lexer);
void lca_lexer_skip_whitespace(lca_lexer* lexer);

/// Allocate memory with the lexer's configured allocator function.
void* lca_lexer_allocate(lca_lexer* lexer, size_t nbytes);

void* lca_lexer_buffered_allocator(void* userdata, size_t nbytes);

void lca_lexer_prime_next_character(lca_lexer* lexer);

/// Decodes the character pointed to by `lexer->source_current`, storing its value and byte count in
/// the output parameters `out_character` and `out_character_stride` respectively.
/// Returns true if the character was a valid UTF-8 encoded character, false otherwise.
bool lca_lexer_decode_utf8(const char* data, int data_count, int* out_character, int* out_character_stride);

const char* lca_lexer_c_token_kind_to_string(int c_token_kind);
void lca_lexer_skip_c_whitespace(lca_lexer* lexer);
lca_lexer_token lca_lexer_read_c_token(lca_lexer* lexer);
lca_lexer_token lca_lexer_read_c_token_no_keywords(lca_lexer* lexer);

enum {
    LCA_TOKEN_BANG = '!',
    LCA_TOKEN_EXCLAMATION = '!',

    LCA_TOKEN_HASH = '#',
    LCA_TOKEN_POUND = '#',
    LCA_TOKEN_NUMBER_SIGN = '#',

    LCA_TOKEN_DOLLAR = '$',
    LCA_TOKEN_DOLLAR_SIGN = '$',

    LCA_TOKEN_PERCENT = '%',
    LCA_TOKEN_AMPERSAND = '&',

    LCA_TOKEN_OPEN_PAREN = '(',
    LCA_TOKEN_CLOSE_PAREN = ')',

    LCA_TOKEN_STAR = '*',
    LCA_TOKEN_PLUS = '+',
    LCA_TOKEN_COMMA = ',',

    LCA_TOKEN_MINUS = '-',

    LCA_TOKEN_DOT = '.',

    LCA_TOKEN_SLASH = '/',
    LCA_TOKEN_FORWARD_SLASH = '/',

    LCA_TOKEN_COLON = ':',

    LCA_TOKEN_SEMI = ';',
    LCA_TOKEN_SEMI_COLON = ';',

    LCA_TOKEN_LESS = '<',
    LCA_TOKEN_LESS_THAN = '<',
    LCA_TOKEN_OPEN_ANGLE = '<',

    LCA_TOKEN_EQUAL = '=',

    LCA_TOKEN_GREATER = '>',
    LCA_TOKEN_GREATER_THAN = '>',
    LCA_TOKEN_CLOSE_ANGLE = '>',

    LCA_TOKEN_QUESTION = '?',
    LCA_TOKEN_QUESTION_MARK = '?',

    LCA_TOKEN_AT = '@',
    LCA_TOKEN_AT_SIGN = '@',

    LCA_TOKEN_OPEN_SQUARE = '[',
    LCA_TOKEN_BACK_SLASH = '\\',
    LCA_TOKEN_CLOSE_SQUARE = ']',

    LCA_TOKEN_CARET = '^',

    LCA_TOKEN_GRAVE = '`',

    LCA_TOKEN_OPEN_CURLY = '{',
    LCA_TOKEN_PIPE = '|',
    LCA_TOKEN_CLOSE_CURLY = '}',

    LCA_TOKEN_TILDE = '~',
};

enum {
    LCA_TOKEN_C_MODULO = '%',
    LCA_TOKEN_C_REMAINDER = '%',
    LCA_TOKEN_C_AND = '&',
    LCA_TOKEN_C_MULTIPLY = '*',
    LCA_TOKEN_C_TIMES = '*',
    LCA_TOKEN_C_ADD = '+',
    LCA_TOKEN_C_SUBTRACT = '-',
    LCA_TOKEN_C_NEGATE = '-',
    LCA_TOKEN_C_DIVIDE = '/',
    LCA_TOKEN_C_XOR = '^',
    LCA_TOKEN_C_OR = '|',
    LCA_TOKEN_C_COMPLEMENT = '~',
    LCA_TOKEN_C_BITWISE_NOT = '~',

    LCA_TOKEN_C_ENUM_MIN_VALUE__ = 1 << 28,

    LCA_TOKEN_C_KEYWORD_MIN_VALUE__,
    LCA_TOKEN_C_KEYWORD_AUTO,
    LCA_TOKEN_C_KEYWORD_BREAK,
    LCA_TOKEN_C_KEYWORD_CASE,
    LCA_TOKEN_C_KEYWORD_CHAR,
    LCA_TOKEN_C_KEYWORD_CONST,
    LCA_TOKEN_C_KEYWORD_CONTINUE,
    LCA_TOKEN_C_KEYWORD_DEFAULT,
    LCA_TOKEN_C_KEYWORD_DO,
    LCA_TOKEN_C_KEYWORD_DOUBLE,
    LCA_TOKEN_C_KEYWORD_ELSE,
    LCA_TOKEN_C_KEYWORD_ENUM,
    LCA_TOKEN_C_KEYWORD_EXTERN,
    LCA_TOKEN_C_KEYWORD_FLOAT,
    LCA_TOKEN_C_KEYWORD_FOR,
    LCA_TOKEN_C_KEYWORD_GOTO,
    LCA_TOKEN_C_KEYWORD_IF,
    LCA_TOKEN_C_KEYWORD_INT,
    LCA_TOKEN_C_KEYWORD_LONG,
    LCA_TOKEN_C_KEYWORD_REGISTER,
    LCA_TOKEN_C_KEYWORD_RETURN,
    LCA_TOKEN_C_KEYWORD_SHORT,
    LCA_TOKEN_C_KEYWORD_SIGNED,
    LCA_TOKEN_C_KEYWORD_SIZEOF,
    LCA_TOKEN_C_KEYWORD_STATIC,
    LCA_TOKEN_C_KEYWORD_STRUCT,
    LCA_TOKEN_C_KEYWORD_SWITCH,
    LCA_TOKEN_C_KEYWORD_TYPEDEF,
    LCA_TOKEN_C_KEYWORD_UNION,
    LCA_TOKEN_C_KEYWORD_UNSIGNED,
    LCA_TOKEN_C_KEYWORD_VOID,
    LCA_TOKEN_C_KEYWORD_VOLATILE,
    LCA_TOKEN_C_KEYWORD_WHILE,
    LCA_TOKEN_C_KEYWORD_END_VALUE__,
    LCA_TOKEN_C_KEYWORD_COUNT__ = LCA_TOKEN_C_KEYWORD_END_VALUE__ - LCA_TOKEN_C_KEYWORD_MIN_VALUE__,

    LCA_TOKEN_C_END_OF_FILE = LCA_TOKEN_C_KEYWORD_END_VALUE__,
    LCA_TOKEN_C_EOF = LCA_TOKEN_C_END_OF_FILE,

    LCA_TOKEN_C_IDENTIFIER,
    LCA_TOKEN_C_IDENT = LCA_TOKEN_C_IDENTIFIER,
    LCA_TOKEN_C_ID = LCA_TOKEN_C_IDENTIFIER,

    LCA_TOKEN_C_INTEGER_LITERAL,
    LCA_TOKEN_C_FLOAT_LITERAL,
    LCA_TOKEN_C_STRING_LITERAL,

    LCA_TOKEN_C_EQUAL_EQUAL,
    LCA_TOKEN_C_BANG_EQUAL,
    LCA_TOKEN_C_GREATER_EQUAL,
    LCA_TOKEN_C_LESS_EQUAL,

    LCA_TOKEN_C_AMPERSAND_AMPERSAND,
    LCA_TOKEN_C_AMP_AMP = LCA_TOKEN_C_AMPERSAND_AMPERSAND,
    LCA_TOKEN_C_AND_AND = LCA_TOKEN_C_AMPERSAND_AMPERSAND,

    LCA_TOKEN_C_PIPE_PIPE,
    LCA_TOKEN_C_OR_OR = LCA_TOKEN_C_PIPE_PIPE,

    LCA_TOKEN_C_PLUS_PLUS,
    LCA_TOKEN_C_INCREMENT = LCA_TOKEN_C_PLUS_PLUS,

    LCA_TOKEN_C_MINUS_MINUS,
    LCA_TOKEN_C_DECREMENT = LCA_TOKEN_C_MINUS_MINUS,

    LCA_TOKEN_C_LESS_LESS,
    LCA_TOKEN_C_SHIFT_LEFT = LCA_TOKEN_C_LESS_LESS,

    LCA_TOKEN_C_GREATER_GREATER,
    LCA_TOKEN_C_SHIFT_RIGHT = LCA_TOKEN_C_GREATER_GREATER,

    LCA_TOKEN_C_PLUS_EQUAL,
    LCA_TOKEN_C_ADD_EQUAL = LCA_TOKEN_C_PLUS_EQUAL,

    LCA_TOKEN_C_MINUS_EQUAL,
    LCA_TOKEN_C_SUBTRACT_EQUAL = LCA_TOKEN_C_MINUS_EQUAL,

    LCA_TOKEN_C_STAR_EQUAL,
    LCA_TOKEN_C_TIMES_EQUAL = LCA_TOKEN_C_STAR_EQUAL,
    LCA_TOKEN_C_MULTIPLY_EQUAL = LCA_TOKEN_C_STAR_EQUAL,

    LCA_TOKEN_C_SLASH_EQUAL,
    LCA_TOKEN_C_FOREWARD_SLASH_EQUAL = LCA_TOKEN_C_SLASH_EQUAL,
    LCA_TOKEN_C_DIVIDE_EQUAL = LCA_TOKEN_C_SLASH_EQUAL,

    LCA_TOKEN_C_PERCENT_EQUAL,
    LCA_TOKEN_C_MODULO_EQUAL = LCA_TOKEN_C_PERCENT_EQUAL,
    LCA_TOKEN_C_REMAINDER_EQUAL = LCA_TOKEN_C_PERCENT_EQUAL,

    LCA_TOKEN_C_LESS_LESS_EQUAL,
    LCA_TOKEN_C_SHIFT_LEFT_EQUAL = LCA_TOKEN_C_LESS_LESS_EQUAL,

    LCA_TOKEN_C_GREATER_GREATER_EQUAL,
    LCA_TOKEN_C_SHIFT_RIGHT_EQUAL = LCA_TOKEN_C_GREATER_GREATER_EQUAL,

    LCA_TOKEN_C_AMPERSAND_EQUAL,
    LCA_TOKEN_C_AMP_EQUAL = LCA_TOKEN_C_AMPERSAND_EQUAL,
    LCA_TOKEN_C_AND_EQUAL = LCA_TOKEN_C_AMPERSAND_EQUAL,

    LCA_TOKEN_C_PIPE_EQUAL,
    LCA_TOKEN_C_OR_EQUAL = LCA_TOKEN_C_PIPE_EQUAL,

    LCA_TOKEN_C_CARET_EQUAL,
    LCA_TOKEN_C_XOR_EQUAL = LCA_TOKEN_C_CARET_EQUAL,

    LCA_TOKEN_C_ENUM_END_VALUE__,
    LCA_TOKEN_C_ENUM_COUNT__ = LCA_TOKEN_C_ENUM_END_VALUE__ - LCA_TOKEN_C_ENUM_MIN_VALUE__,
};

#ifdef LCA_LEXER_IMPLEMENTATION

#    include <assert.h>
#    include <stdlib.h>
#    include <string.h>

void lca_lexer_init(lca_lexer* lexer, const char* source_begin, const char* source_end) {
    assert(lexer != NULL);
    memset(lexer, 0, sizeof *lexer);

    lexer->source_begin = source_begin;
    if (source_end == NULL)
        lexer->source_end = source_begin + strlen(source_begin);
    else lexer->source_end = source_end;
    assert(lexer->source_end >= lexer->source_begin && "lexer source range is inverted, or otherwise invalid");

    lexer->source_current = source_begin;

    lexer->character_decoder = lca_lexer_decode_utf8;
    lca_lexer_prime_next_character(lexer);
}

void lca_lexer_init_buffered(lca_lexer* lexer, const char* source_begin, const char* source_end, lca_lexer_buffered_allocator_userdata* buffered_allocator_userdata) {
    lca_lexer_init(lexer, source_begin, source_end);

    lexer->allocator_userdata = buffered_allocator_userdata;
    lexer->allocator_function = lca_lexer_buffered_allocator;
}

void lca_lexer_init_allocator(lca_lexer* lexer, const char* source_begin, const char* source_end, void* allocator_userdata, lca_lexer_allocator_function allocator_function) {
    lca_lexer_init(lexer, source_begin, source_end);

    lexer->allocator_userdata = allocator_userdata;
    lexer->allocator_function = allocator_function;
}

int lca_lexer_next_character(lca_lexer* lexer) {
    assert(lexer != NULL);

    assert(lexer->source_begin != NULL);
    assert(lexer->source_end != NULL);
    assert(lexer->source_current != NULL);

    assert(lexer->character_decoder != NULL);
    if (!lca_lexer_is_at_end(lexer)) {
        assert(lexer->current_character_stride != 0);
    }

    lexer->source_current += lexer->current_character_stride;

    if (lca_lexer_is_at_end(lexer)) {
        lexer->current_character = 0;
        lexer->current_character_stride = 0;
    } else {
        lca_lexer_prime_next_character(lexer);
    }

    return lexer->current_character;
}

int lca_lexer_peek_next_character(lca_lexer* lexer) {
    const char* source_current = lexer->source_current;
    if (source_current + lexer->current_character_stride >= lexer->source_end) {
        return 0;
    }

    int peek_character = 0;
    if (!lexer->character_decoder(lexer->source_current + lexer->current_character_stride, (int)(lexer->source_end - (lexer->source_current + lexer->current_character_stride)), &peek_character, NULL)) {
        return 0;
    }

    return peek_character;
}

bool lca_lexer_is_at_end(lca_lexer* lexer) {
    return lexer->source_current >= lexer->source_end;
}

static bool lca_lexer_is_whitespace_impl(int c) {
    return c == ' ' || c == '\r' || c == '\n' || c == '\t' || c == '\v';
}

void lca_lexer_skip_whitespace(lca_lexer* lexer) {
    while (!lca_lexer_is_at_end(lexer) && lca_lexer_is_whitespace_impl(lexer->current_character)) {
        lca_lexer_next_character(lexer);
    }
}

void* lca_lexer_allocate(lca_lexer* lexer, size_t nbytes) {
    assert(lexer != NULL);
    assert(lexer->allocator_function != NULL);

    void* result = lexer->allocator_function(lexer->allocator_userdata, nbytes);
    assert(result != NULL && "lexer allocator function returned NULL");

    return result;
}

void* lca_lexer_buffered_allocator(void* userdata, size_t nbytes) {
    assert(userdata != NULL);
    lca_lexer_buffered_allocator_userdata* buffered_userdata = userdata;

    assert(nbytes <= buffered_userdata->buffer_length - buffered_userdata->allocated_count && "Buffered allocator ran out of available memory.");

    void* result = buffered_userdata->string_buffer + buffered_userdata->allocated_count;
    memset(result, 0, nbytes);

    buffered_userdata->allocated_count += nbytes;
    return result;
}

void lca_lexer_prime_next_character(lca_lexer* lexer) {
    assert(lexer != NULL);
    assert(lexer->character_decoder != NULL);

    if (!lexer->character_decoder(lexer->source_current, (int)(lexer->source_end - lexer->source_current), &lexer->current_character, &lexer->current_character_stride)) {
        assert(false && "handle error when decoder fails");
    }
}

bool lca_lexer_decode_utf8(const char* data, int data_count, int* out_character, int* out_character_stride) {
    assert(data != NULL);
    assert(data_count > 0);

    int character = *data;

    if (out_character) *out_character = character;
    if (out_character_stride) *out_character_stride = 1;

    return true;
}

// C lexer

const char* lca_lexer_c_token_kind_to_string(int c_token_kind) {
    if (c_token_kind > 32 && c_token_kind < 128) {
        static char values[256] = {0};
        static bool values_initialized = false;

        if (!values_initialized) {
            values_initialized = true;
            for (int i = 32; i < 128; i++) {
                values[i * 2] = i;
            }
        }

        return &values[c_token_kind * 2];
    }

    switch (c_token_kind) {
        default: return "<unknown>";
        case LCA_TOKEN_C_KEYWORD_AUTO: return "auto";
        case LCA_TOKEN_C_KEYWORD_BREAK: return "break";
        case LCA_TOKEN_C_KEYWORD_CASE: return "case";
        case LCA_TOKEN_C_KEYWORD_CHAR: return "char";
        case LCA_TOKEN_C_KEYWORD_CONST: return "const";
        case LCA_TOKEN_C_KEYWORD_CONTINUE: return "continue";
        case LCA_TOKEN_C_KEYWORD_DEFAULT: return "default";
        case LCA_TOKEN_C_KEYWORD_DO: return "do";
        case LCA_TOKEN_C_KEYWORD_DOUBLE: return "double";
        case LCA_TOKEN_C_KEYWORD_ELSE: return "else";
        case LCA_TOKEN_C_KEYWORD_ENUM: return "enum";
        case LCA_TOKEN_C_KEYWORD_EXTERN: return "extern";
        case LCA_TOKEN_C_KEYWORD_FLOAT: return "float";
        case LCA_TOKEN_C_KEYWORD_FOR: return "for";
        case LCA_TOKEN_C_KEYWORD_GOTO: return "goto";
        case LCA_TOKEN_C_KEYWORD_IF: return "if";
        case LCA_TOKEN_C_KEYWORD_INT: return "int";
        case LCA_TOKEN_C_KEYWORD_LONG: return "long";
        case LCA_TOKEN_C_KEYWORD_REGISTER: return "register";
        case LCA_TOKEN_C_KEYWORD_RETURN: return "return";
        case LCA_TOKEN_C_KEYWORD_SHORT: return "short";
        case LCA_TOKEN_C_KEYWORD_SIGNED: return "signed";
        case LCA_TOKEN_C_KEYWORD_SIZEOF: return "sizeof";
        case LCA_TOKEN_C_KEYWORD_STATIC: return "static";
        case LCA_TOKEN_C_KEYWORD_STRUCT: return "struct";
        case LCA_TOKEN_C_KEYWORD_SWITCH: return "switch";
        case LCA_TOKEN_C_KEYWORD_TYPEDEF: return "typedef";
        case LCA_TOKEN_C_KEYWORD_UNION: return "union";
        case LCA_TOKEN_C_KEYWORD_UNSIGNED: return "unsigned";
        case LCA_TOKEN_C_KEYWORD_VOID: return "void";
        case LCA_TOKEN_C_KEYWORD_VOLATILE: return "volatile";
        case LCA_TOKEN_C_KEYWORD_WHILE: return "while";
        case LCA_TOKEN_C_END_OF_FILE: return "<eof>";
        case LCA_TOKEN_C_IDENTIFIER: return "<identifier>";
        case LCA_TOKEN_C_INTEGER_LITERAL: return "<integer_literal>";
        case LCA_TOKEN_C_FLOAT_LITERAL: return "<float_literal>";
        case LCA_TOKEN_C_STRING_LITERAL: return "<string_literal>";
        case LCA_TOKEN_C_EQUAL_EQUAL: return "==";
        case LCA_TOKEN_C_BANG_EQUAL: return "!=";
        case LCA_TOKEN_C_GREATER_EQUAL: return ">=";
        case LCA_TOKEN_C_LESS_EQUAL: return "<=>";
        case LCA_TOKEN_C_AMPERSAND_AMPERSAND: return "&&";
        case LCA_TOKEN_C_PIPE_PIPE: return "||";
        case LCA_TOKEN_C_PLUS_PLUS: return "++";
        case LCA_TOKEN_C_MINUS_MINUS: return "--";
        case LCA_TOKEN_C_LESS_LESS: return "<<";
        case LCA_TOKEN_C_GREATER_GREATER: return ">>";
        case LCA_TOKEN_C_PLUS_EQUAL: return "+=";
        case LCA_TOKEN_C_MINUS_EQUAL: return "-=";
        case LCA_TOKEN_C_STAR_EQUAL: return "*=";
        case LCA_TOKEN_C_SLASH_EQUAL: return "/=";
        case LCA_TOKEN_C_PERCENT_EQUAL: return "%=";
        case LCA_TOKEN_C_LESS_LESS_EQUAL: return "<<=";
        case LCA_TOKEN_C_GREATER_GREATER_EQUAL: return ">>=";
        case LCA_TOKEN_C_AMPERSAND_EQUAL: return "&=";
        case LCA_TOKEN_C_PIPE_EQUAL: return "|=";
        case LCA_TOKEN_C_CARET_EQUAL: return "^=";
    }
}

#    include <stdio.h>

void lca_lexer_skip_c_whitespace(lca_lexer* lexer) {
    while (!lca_lexer_is_at_end(lexer)) {
        if (lca_lexer_is_whitespace_impl(lexer->current_character)) {
            lca_lexer_next_character(lexer);
        } else if (lexer->current_character == '/' && lca_lexer_peek_next_character(lexer) == '/') {
            while (!lca_lexer_is_at_end(lexer) && lexer->current_character != '\n') {
                lca_lexer_next_character(lexer);
            }
        } else if (lexer->current_character == '/' && lca_lexer_peek_next_character(lexer) == '*') {
            lca_lexer_next_character(lexer);
            lca_lexer_next_character(lexer);

            int last_character = 0;
            bool is_finished = false;

            while (!lca_lexer_is_at_end(lexer) && !is_finished) {
                int current_character = lexer->current_character;
                lca_lexer_next_character(lexer);

                if (last_character == '*' && current_character == '/') {
                    is_finished = true;
                }
            }

            if (!is_finished) {
                assert(false && "report lexer error for unfinished C delimited comment");
            }
        } else {
            break;
        }
    }
}

lca_lexer_token lca_lexer_read_c_token(lca_lexer* lexer) {
    lca_lexer_token token = lca_lexer_read_c_token_no_keywords(lexer);
    return token;
}

static bool lca_lexer_character_is_c_identifier_part(int c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
}

lca_lexer_token lca_lexer_read_c_token_no_keywords(lca_lexer* lexer) {
    lca_lexer_skip_c_whitespace(lexer);

    lca_lexer_token token = {
        .source_begin = lexer->source_current,
    };

    if (lca_lexer_is_at_end(lexer)) {
        token.kind = LCA_TOKEN_C_EOF;
        return token;
    }

    int current_character = lexer->current_character;
    switch (current_character) {
        case '(':
        case ')':
        case '[':
        case ']':
        case '{':
        case '}':
        case ',':
        case ';':
        case ':': {
            lca_lexer_next_character(lexer);
            token.kind = current_character;
        } break;

        case '_':
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
        case 'g':
        case 'h':
        case 'i':
        case 'j':
        case 'k':
        case 'l':
        case 'm':
        case 'n':
        case 'o':
        case 'p':
        case 'q':
        case 'r':
        case 's':
        case 't':
        case 'u':
        case 'v':
        case 'w':
        case 'x':
        case 'y':
        case 'z':
        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
        case 'G':
        case 'H':
        case 'I':
        case 'J':
        case 'K':
        case 'L':
        case 'M':
        case 'N':
        case 'O':
        case 'P':
        case 'Q':
        case 'R':
        case 'S':
        case 'T':
        case 'U':
        case 'V':
        case 'W':
        case 'X':
        case 'Y':
        case 'Z': {
            while (!lca_lexer_is_at_end(lexer) && lca_lexer_character_is_c_identifier_part(lexer->current_character)) {
                lca_lexer_next_character(lexer);
            }

            token.kind = LCA_TOKEN_C_IDENTIFIER;
            token.string_value = token.source_begin;
            token.string_value_length = lexer->source_current - token.source_begin;
        } break;

        default: {
            assert(false && "unhandled character in C token read, report error");
        }
    }

    assert(token.kind != 0 && "C token read did not populate the token kind, oops");

    token.source_end = lexer->source_current;
    assert(token.source_end > token.source_begin && "C token read did not consume any characters, oops");

    return token;
}

#endif // LCA_LEXER_IMPLEMENTATION

#endif // !LCA_LEXER_H
