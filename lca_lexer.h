/*
This software is available under 2 licenses -- choose whichever you prefer.
------------------------------------------------------------------------------
ALTERNATIVE A - MIT License
Copyright (c) 2024 Local Atticus
Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
------------------------------------------------------------------------------
ALTERNATIVE B - Public Domain (www.unlicense.org)
This is free and unencumbered software released into the public domain.
Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
software, either in source code form or as a compiled binary, for any purpose,
commercial or non-commercial, and by any means.
In jurisdictions that recognize copyright laws, the author or authors of this
software dedicate any and all copyright interest in the software to the public
domain. We make this dedication for the benefit of the public at large and to
the detriment of our heirs and successors. We intend this dedication to be an
overt act of relinquishment in perpetuity of all present and future rights to
this software under copyright law.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/*
 */

#ifndef LCA_LEXER_H
#define LCA_LEXER_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#if !defined(LCA_INCLUDED)
#    include <assert.h>

#    define LCA_ASSERT(Condition, Message) assert((Condition) && Message)

#    if defined(LCA_LEXER_MALLOC) || defined(LCA_LEXER_REALLOC) || defined(LCA_LEXER_DEALLOC)
#        if !defined(LCA_LEXER_MALLOC) || !defined(LCA_LEXER_REALLOC) || !defined(LCA_LEXER_DEALLOC)
#            error "The LCA Lexer library requires all three of LCA_LEXER_MALLOC, LCA_LEXER_REALLOC and LCA_LEXER_DEALLOC to be defined if at least one is."
#        endif
#    else
#        define LCA_LEXER_MALLOC(N)     (malloc(N))
#        define LCA_LEXER_REALLOC(P, N) (realloc(P, N))
#        define LCA_LEXER_FREE(P)       (free(P))
#    endif

#    define lca_da(T)            T*
#    define lca_da_get_header(V) (((struct lca_da_header*)(V)) - 1)
#    define lca_da_count(V)      ((V) ? lca_da_get_header(V)->count : 0)
#    define lca_da_capacity(V)   ((V) ? lca_da_get_header(V)->capacity : 0)
#    define lca_da_push(V, E)                                                             \
        do {                                                                              \
            lca_da_maybe_expand((void**)&(V), (int64_t)sizeof *(V), lca_da_count(V) + 1); \
            (V)[lca_da_count(V)] = E;                                                     \
            lca_da_get_header(V)->count++;                                                \
        } while (0)
#    define lca_da_pop(V)                                                   \
        do {                                                                \
            if (lca_da_get_header(V)->count) lca_da_get_header(V)->count--; \
        } while (0)

void lca_da_maybe_expand(void** da_ref, int64_t element_size, int64_t required_count);

/// Header data for a light-weight implelentation of typed dynamic arrays.
typedef struct lca_da_header {
    int64_t capacity;
    int64_t count;
} lca_da_header;

typedef struct lca_string_view {
    const char* data;
    int64_t count;
} lca_string_view;
#endif // !defined(LCA_INCLUDED)

typedef struct lca_lexer lca_lexer;
typedef struct lca_lexer_token lca_lexer_token;

typedef void* (*lca_lexer_allocator_function)(void* userdata, size_t nbytes);

/// The type of the lexer's character decoding functions.
typedef bool (*lca_lexer_character_decoder)(const char* data, int data_count, int* out_character, int* out_character_stride);

typedef void (*lca_lexer_error_handler)(const char* source_where_position, int source_character, const char* message);

struct lca_lexer {
    /// The name of the source being lexed.
    lca_string_view name;

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

typedef struct lca_lexer_init_args {
    void* allocator_userdata;
    lca_lexer_allocator_function allocator_function;
    lca_lexer_character_decoder character_decoder;
} lca_lexer_init_args;

struct lca_lexer_token {
    int kind;
    lca_string_view source_text;

    int64_t integer_value;
    double real_value;
    lca_string_view string_value;
};

typedef struct {
    char* string_buffer;
    size_t buffer_length;
    size_t allocated_count;
} lca_lexer_buffered_allocator_userdata;

#ifdef LCA_LEXER_LANGUAGE_C

typedef struct lca_lexer_c_token {
    // all the same things as lca_lexer_token

    int kind;
    lca_string_view source_text;

    int64_t integer_value;
    double real_value;
    lca_string_view string_value;

    // but also preprocessor things

    bool is_macro_param;
    int macro_param_index;
    bool is_angle_string;
} lca_lexer_c_token;

typedef struct lca_lexer_c_macro_def {
    const char* name;
    size_t name_length;

    bool has_params;
    lca_da(lca_string_view) params;
    lca_da(lca_lexer_c_token) body;
} lca_lexer_c_macro_def;

typedef struct lca_lexer_c_macro_expansion {
    lca_lexer_c_macro_def* def;
    int64_t body_position;
    lca_da(lca_da(lca_lexer_c_token)) args;
    int64_t arg_index; // set to -1 when not expanding an argument
    int64_t arg_position;
} lca_lexer_c_macro_expansion;

typedef struct lca_lexer_cpp {
    /// True if the lexer is within the preprocessor, false otherwise.
    /// When the preprocessor is provided to the C lexer functions, some lexer features (like consuming newline characters)
    /// are conditional on whether or not the preprocess is active.
    /// When no preprocessor is given to the C lexer functions, it is assumed to be inactive.
    bool active;
    bool at_start_of_line;
    bool is_in_include;

    lca_da(lca_lexer_c_macro_expansion) macro_expansions;
} lca_lexer_cpp;

#endif // LCA_LEXER_LANGUAGE_C

/// Initialize the lexer with a given range of source text.
/// This default initialization routine will rely on libc's `malloc` to allocate necessary memory.
void lca_lexer_init(lca_lexer* lexer, const char* name, int64_t name_length, const char* source_begin, const char* source_end);
// void lca_lexer_init_all(lca_lexer* lexer, const char* name, int64_t name_length, const char* source_begin, const char* source_end, lca_lexer_init_args args);
void lca_lexer_init_buffered(lca_lexer* lexer, const char* name, int64_t name_length, const char* source_begin, const char* source_end, lca_lexer_buffered_allocator_userdata* buffered_allocator_userdata);
// void lca_lexer_init_buffered(lca_lexer* lexer, const char* source_begin, const char* source_end, char* string_buffer, size_t string_buffer_size);
void lca_lexer_init_allocator(lca_lexer* lexer, const char* name, int64_t name_length, const char* source_begin, const char* source_end, void* allocator_userdata, lca_lexer_allocator_function allocator_function);

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

#ifdef LCA_LEXER_LANGUAGE_C

const char* lca_lexer_c_token_kind_to_string(int c_token_kind);
void lca_lexer_skip_c_whitespace(lca_lexer* lexer, lca_lexer_cpp* cpp);
lca_lexer_c_token lca_lexer_read_c_token(lca_lexer* lexer, lca_lexer_cpp* cpp);
lca_lexer_c_token lca_lexer_read_c_token_no_keywords(lca_lexer* lexer, lca_lexer_cpp* cpp);

void lca_lexer_cpp_init(lca_lexer_cpp* cpp);
bool lca_lexer_cpp_is_active(lca_lexer_cpp* cpp);

#endif // LCA_LEXER_LANGUAGE_C

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

#ifdef LCA_LEXER_LANGUAGE_C

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

#endif // LCA_LEXER_LANGUAGE_C

#if defined(LCA_IMPLEMENTATION) || defined(LCA_LEXER_IMPLEMENTATION)

#    include <assert.h>
#    include <stdlib.h>
#    include <string.h>

#    if !defined(LCA_INCLUDED)

void lca_da_maybe_expand(void** da_ref, int64_t element_size, int64_t required_count) {
    if (required_count <= 0) return;

    struct lca_da_header* header = lca_da_get_header(*da_ref);
    if (!*da_ref) {
        int64_t initial_capacity = 32;
        void* new_data = LCA_LEXER_MALLOC((sizeof *header) + (size_t)(initial_capacity * element_size));
        header = new_data;

        header->capacity = initial_capacity;
        header->count = 0;
    } else if (required_count > header->capacity) {
        while (required_count > header->capacity)
            header->capacity *= 2;
        header = LCA_LEXER_REALLOC(header, (sizeof *header) + (size_t)(header->capacity * element_size));
    }

    *da_ref = (void*)(header + 1);
}

#    endif // !defined(LCA_INCLUDED)

void lca_lexer_init(lca_lexer* lexer, const char* name, int64_t name_length, const char* source_begin, const char* source_end) {
    assert(lexer != NULL);
    memset(lexer, 0, sizeof *lexer);

    if (name_length < 0) {
        name_length = strlen(name);
    }

    lexer->name = (lca_string_view){
        .data = name,
        .count = name_length,
    };

    lexer->source_begin = source_begin;
    if (source_end == NULL)
        lexer->source_end = source_begin + strlen(source_begin);
    else lexer->source_end = source_end;
    assert(lexer->source_end >= lexer->source_begin && "lexer source range is inverted, or otherwise invalid");

    lexer->source_current = source_begin;

    lexer->character_decoder = lca_lexer_decode_utf8;
    lca_lexer_prime_next_character(lexer);
}

void lca_lexer_init_buffered(lca_lexer* lexer, const char* name, int64_t name_length, const char* source_begin, const char* source_end, lca_lexer_buffered_allocator_userdata* buffered_allocator_userdata) {
    lca_lexer_init(lexer, name, name_length, source_begin, source_end);

    lexer->allocator_userdata = buffered_allocator_userdata;
    lexer->allocator_function = lca_lexer_buffered_allocator;
}

void lca_lexer_init_allocator(lca_lexer* lexer, const char* name, int64_t name_length, const char* source_begin, const char* source_end, void* allocator_userdata, lca_lexer_allocator_function allocator_function) {
    lca_lexer_init(lexer, name, name_length, source_begin, source_end);

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

int lca_lexer_peek_nth_character(lca_lexer* lexer, int n) {
    if (n == 0) return lexer->current_character;

    const char* source_current = lexer->source_current;
    int current_character_stride = lexer->current_character_stride;

    int peek_character = 0;

    for (int i = 0; i < n; i++) {
        if (source_current + current_character_stride >= lexer->source_end) {
            return 0;
        }

        const char* source_next = source_current + current_character_stride;
        if (!lexer->character_decoder(source_current + current_character_stride, (int)(lexer->source_end - (source_current + current_character_stride)), &peek_character, &current_character_stride)) {
            return 0;
        }

        source_current = source_next;
    }

    return peek_character;
}

int lca_lexer_peek_next_character(lca_lexer* lexer) {
    return lca_lexer_peek_nth_character(lexer, 1);
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

/// C Language Lexer

#    ifdef LCA_LEXER_LANGUAGE_C

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

static bool lca_lexer_c_skip_backslash_newline(lca_lexer* lexer, lca_lexer_cpp* cpp) {
    LCA_ASSERT(lexer != NULL, "lexer missing");

    if (!lca_lexer_is_at_end(lexer) && lexer->current_character == '\\' &&
        (lca_lexer_peek_next_character(lexer) == '\n' || (lca_lexer_peek_next_character(lexer) == '\r' && lca_lexer_peek_nth_character(lexer, 2) == '\n'))) {
        lca_lexer_next_character(lexer);
        LCA_ASSERT(!lca_lexer_is_at_end(lexer), "checked for multiple characters, but only one could be consumed");

        if (lexer->current_character == '\n') {
            lca_lexer_next_character(lexer);
            if (!lca_lexer_is_at_end(lexer) && lexer->current_character == '\r')
                lca_lexer_next_character(lexer);
        } else {
            LCA_ASSERT(lexer->current_character == '\r' && lca_lexer_peek_next_character(lexer) == '\n', "checked for \r\n, but didn't see it");
            lca_lexer_next_character(lexer);
            lca_lexer_next_character(lexer);
        }

        return true;
    }

    return false;
}

static void lca_lexer_c_next_character(lca_lexer* lexer, lca_lexer_cpp* cpp, bool allow_comments) {
    LCA_ASSERT(lexer != NULL, "lexer missing");

    lca_lexer_next_character(lexer);

    if (lexer->current_character == '\n') {
        if (cpp) cpp->at_start_of_line = true;
    } else if (!lca_lexer_is_whitespace_impl(lexer->current_character) && lexer->current_character != 0) {
        if (cpp) cpp->at_start_of_line = false;
    }

    while (lca_lexer_c_skip_backslash_newline(lexer, cpp)) {}
    if (lca_lexer_is_at_end(lexer)) return;

    if (allow_comments && lexer->current_character == '/') {
        if (lca_lexer_peek_next_character(lexer) == '/') {
            lca_lexer_next_character(lexer);
            lca_lexer_next_character(lexer);

            bool has_warned = false;

            while (!lca_lexer_is_at_end(lexer)) {
                if (lexer->current_character == '\\' && lca_lexer_c_skip_backslash_newline(lexer, cpp)) {
                    if (!has_warned) {
                        LCA_ASSERT(false, "issue a warning for multi-line // comment");
                    }

                    has_warned = true;
                    continue;
                }

                if (lexer->current_character == '\n') {
                    break;
                }

                lca_lexer_next_character(lexer);
            }

            LCA_ASSERT(lca_lexer_is_at_end(lexer) || lexer->current_character == '\n', "invalid place to end a // comment");
            lexer->current_character = ' ';
        } else if (lca_lexer_peek_next_character(lexer) == '*') {
            lca_lexer_next_character(lexer);
            lca_lexer_next_character(lexer);

            int last_character = 0;
            int current_character = 0;

            for (;;) {
                if (!lca_lexer_is_at_end(lexer) && lexer->current_character == '\\') {
                    lca_lexer_c_skip_backslash_newline(lexer, cpp);
                }

                if (lca_lexer_is_at_end(lexer)) {
                    LCA_ASSERT(false, "issue error for unfinished /* comment.");
                    break;
                }

                current_character = lexer->current_character;
                lca_lexer_next_character(lexer);

                if (current_character == '\n') {
                    if (cpp) cpp->at_start_of_line = true;
                }

                if (current_character == '/' && last_character == '*') {
                    break;
                }

                last_character = current_character;
            }

            LCA_ASSERT(lca_lexer_is_at_end(lexer) || (current_character == '/' && last_character == '*'), "invalid place to end a /* comment");
            lexer->current_character = ' ';
        }
    }
}

void lca_lexer_skip_c_whitespace(lca_lexer* lexer, lca_lexer_cpp* cpp) {
    LCA_ASSERT(lexer != NULL, "lexer missing");

    while (!lca_lexer_is_at_end(lexer) && lca_lexer_is_whitespace_impl(lexer->current_character)) {
        if (lca_lexer_cpp_is_active(cpp) && lexer->current_character == '\n')
            break;

        lca_lexer_c_next_character(lexer, cpp, true);
    }
}

static void lca_lexer_c_token_maybe_transform_keyword(lca_lexer_c_token* token) {
}

lca_lexer_c_token lca_lexer_read_c_token(lca_lexer* lexer, lca_lexer_cpp* cpp) {
    LCA_ASSERT(lexer != NULL, "lexer missing");

    if (cpp == NULL) {
        lca_lexer_c_token token = lca_lexer_read_c_token_no_keywords(lexer, cpp);
        if (token.kind == LCA_TOKEN_C_IDENTIFIER) {
            lca_lexer_c_token_maybe_transform_keyword(&token);
        }

        return token;
    }

    LCA_ASSERT(cpp != NULL, "somehow skipped the early check for CPP presence");

    if (lca_da_count(cpp->macro_expansions) > 0) {
        LCA_ASSERT(false, "get tokens from current macro expansion");
        //

        // we can't modify cpp->macro_expansions while we have this reference.
        lca_lexer_c_macro_expansion* current_macro_expansion = &cpp->macro_expansions[lca_da_count(cpp->macro_expansions) - 1];

        if (current_macro_expansion->arg_index >= 0) {
        } else {
            LCA_ASSERT(current_macro_expansion->def != NULL, "no macro def associated with this expansion");

            lca_da(lca_lexer_c_token) macro_body = current_macro_expansion->def->body;
            if (current_macro_expansion->body_position >= lca_da_count(macro_body)) {
                current_macro_expansion = NULL;
                lca_da_pop(cpp->macro_expansions);
                goto regular_lex_c_token;
            }

            int64_t body_position = current_macro_expansion->body_position;
        }

        return (lca_lexer_c_token){0};
    }

regular_lex_c_token:;
    lca_lexer_skip_c_whitespace(lexer, cpp);
    while (cpp->at_start_of_line && lexer->current_character == '#') {
        LCA_ASSERT(false, "parse preprocessor directives");
        //

        LCA_ASSERT(!lca_lexer_cpp_is_active(cpp), "parsing preprocessor directive left us in a preprocessing state");
        lca_lexer_skip_c_whitespace(lexer, cpp);
    }

    lca_lexer_c_token token = lca_lexer_read_c_token_no_keywords(lexer, cpp);
    if (token.kind == LCA_TOKEN_C_IDENTIFIER) {
        // TODO(local): lookup macros and spawn expansions and stuff
    not_a_macro:;
        lca_lexer_c_token_maybe_transform_keyword(&token);
    }

    return token;
}

static bool lca_lexer_character_is_c_identifier_part(int c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_';
}

lca_lexer_c_token lca_lexer_read_c_token_no_keywords(lca_lexer* lexer, lca_lexer_cpp* cpp) {
    lca_lexer_skip_c_whitespace(lexer, cpp);

    lca_lexer_c_token token = {
        .source_text.data = lexer->source_current,
    };

    if (lca_lexer_is_at_end(lexer)) {
        token.kind = LCA_TOKEN_C_EOF;
        return token;
    }

    int current_character = lexer->current_character;
    switch (current_character) {
        case '\n': {
            LCA_ASSERT(lca_lexer_cpp_is_active(cpp), "should only encounter the newline character in the main C token lexer switch if the preprocessor is active.");
            lca_lexer_c_next_character(lexer, cpp, true);
            token.kind = '\n';
            if (lexer->current_character != 0 && lexer->source_current < lexer->source_end) LCA_ASSERT(cpp->at_start_of_line, "after newline, should always be at start of line");
        } break;

            // clang-format off
        case '(': case ')':
        case '[': case ']':
        case '{': case '}':
        case ',': case ';': case ':': {
            // clang-format on
            lca_lexer_c_next_character(lexer, cpp, true);
            token.kind = current_character;
        } break;

            // clang-format off
        case '_':
        case 'a': case 'b': case 'c': case 'd': case 'e':
        case 'f': case 'g': case 'h': case 'i': case 'j':
        case 'k': case 'l': case 'm': case 'n': case 'o':
        case 'p': case 'q': case 'r': case 's': case 't':
        case 'u': case 'v': case 'w': case 'x': case 'y':
        case 'z':
        case 'A': case 'B': case 'C': case 'D': case 'E':
        case 'F': case 'G': case 'H': case 'I': case 'J':
        case 'K': case 'L': case 'M': case 'N': case 'O':
        case 'P': case 'Q': case 'R': case 'S': case 'T':
        case 'U': case 'V': case 'W': case 'X': case 'Y':
        case 'Z': {
            // clang-format on
            while (!lca_lexer_is_at_end(lexer) && lca_lexer_character_is_c_identifier_part(lexer->current_character)) {
                lca_lexer_c_next_character(lexer, cpp, true);
            }

            token.kind = LCA_TOKEN_C_IDENTIFIER;
            token.string_value = (lca_string_view){
                .data = token.source_text.data,
                .count = lexer->source_current - token.source_text.data
            };
        } break;

        default: {
            LCA_ASSERT(false, "unhandled character in C token read, report error");
        }
    }

    LCA_ASSERT(token.kind != 0, "C token read did not populate the token kind, oops");

    token.source_text.count = lexer->source_current - token.source_text.data;
    LCA_ASSERT(token.source_text.count > 0, "C token read did not consume any characters, oops");

    return token;
}

void lca_lexer_cpp_init(lca_lexer_cpp* cpp) {
    if (cpp == NULL) return;
    *cpp = (lca_lexer_cpp){0};
}

bool lca_lexer_cpp_is_active(lca_lexer_cpp* cpp) {
    return cpp == NULL ? false : cpp->active;
}

#    endif // LCA_LEXER_LANGUAGE_C

#endif // defined(LCA_IMPLEMENTATION) || defined(LCA_LEXER_IMPLEMENTATION)

#endif // !LCA_LEXER_H
