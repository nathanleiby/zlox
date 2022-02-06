const std = @import("std");
const print = std.debug.print;
// TODO: consider dep injecting this, so i can use a test one too
const allocator = std.heap.page_allocator;
const expect = std.testing.expect;

const DEBUG_PRINT_TOKENS = true;

pub const TokenType = enum {
// Single-character tokens.
LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,
// One or two character tokens.
BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,
// Literals.
IDENTIFIER, STRING, NUMBER,
// Keywords.
AND, CLASS, ELSE, FALSE, FOR, FUN, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE, ERROR, EOF };

pub const Token = struct {
    ttype: TokenType,
    start: usize, // []u8
    length: usize,
    line: usize,
};

// fn scanTokens() void {
//     var line: usize = 0;
//     while (true) {
//         const token: Token = scanToken();

//         if (DEBUG_PRINT_TOKENS) {
//             if (token.line != line) {
//                 print("line={d} ", .{token.line});
//                 //   print("%4d ", .{token.line});
//                 line = token.line;
//             } else {
//                 print("   | ", .{});
//             }

//             if (token.start + token.length < scanner.source.len) {
//                 print("ttype={d} '{s}'\n", .{ token.ttype, scanner.source[token.start .. token.start + token.length] });
//             } else {
//                 print("ttype={d}\n", .{token.ttype});
//             }
//         }

//         // TODO: we don't yet emit this?
//         if (token.ttype == TokenType.EOF) break;
//     }
// }

fn skipWhitespace() void {
    while (!isAtEnd()) {
        const c = peek();
        if (c == ' ' or c == '\r' or c == '\t') {
            _ = advance();
        } else if (c == '\n') {
            scanner.line += 1;
            _ = advance();
        } else if (c == '/') {
            if (peekNext() == '/') {
                // A comment goes until the end of the line.
                while (peek() != '\n' and !isAtEnd()) {
                    _ = advance();
                }
            }
            return;
        } else {
            return;
        }
    }
}

fn peek() u8 {
    return scanner.source[scanner.current];
}

fn peekNext() u8 {
    // TODO: Explore use of https://ziglang.org/documentation/master/#Sentinel-Terminated-Slices
    if (isAtEnd()) return 0; // can I handle 0s like \0 would work in C implementation?
    return scanner.source[scanner.current + 1];
}

pub fn scanToken() Token {
    skipWhitespace();
    scanner.start = scanner.current;

    if (isAtEnd()) return makeToken(TokenType.EOF);

    const c = advance();

    if (isDigit(c)) return number();
    if (isAlpha(c)) return identifier();

    if (c == '(') return makeToken(TokenType.LEFT_PAREN);
    if (c == ')') return makeToken(TokenType.RIGHT_PAREN);
    if (c == '{') return makeToken(TokenType.LEFT_BRACE);
    if (c == '}') return makeToken(TokenType.RIGHT_BRACE);
    if (c == ';') return makeToken(TokenType.SEMICOLON);
    if (c == ',') return makeToken(TokenType.COMMA);
    if (c == '.') return makeToken(TokenType.DOT);
    if (c == '-') return makeToken(TokenType.MINUS);
    if (c == '+') return makeToken(TokenType.PLUS);
    if (c == '/') return makeToken(TokenType.SLASH);
    if (c == '*') return makeToken(TokenType.STAR);
    if (c == '!') {
        const ttype = if (match('=')) TokenType.BANG_EQUAL else TokenType.BANG;
        return makeToken(ttype);
    }
    if (c == '=') {
        const ttype = if (match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL;
        return makeToken(ttype);
    }
    if (c == '<') {
        const ttype = if (match('=')) TokenType.LESS_EQUAL else TokenType.LESS;
        return makeToken(ttype);
    }
    if (c == '>') {
        const ttype = if (match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER;
        return makeToken(ttype);
    }
    if (c == '"') return string();

    var msg: []const u8 = "Unexpected character.";
    return errorToken(msg);
}

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlphaNumeric(c: u8) bool {
    return isAlpha(c) or isDigit(c);
}

fn identifier() Token {
    while (isAlphaNumeric(peek())) {
        _ = advance();
    }

    // const ttype = lookupIdentifier(scanner.source[scanner.start..scanner.current]);
    return makeToken(identifierType());
}

fn identifierType() TokenType {
    switch (scanner.source[scanner.start]) {
        'a' => {
            return checkKeyword(1, 2, "nd", TokenType.AND);
        },
        'c' => {
            return checkKeyword(1, 4, "lass", TokenType.CLASS);
        },
        'e' => {
            return checkKeyword(1, 3, "lse", TokenType.ELSE);
        },
        'f' => {
            if (scanner.current - scanner.start > 1) {
                switch (scanner.source[scanner.start + 1]) {
                    'a' => {
                        return checkKeyword(2, 3, "lse", TokenType.FALSE);
                    },
                    'o' => {
                        return checkKeyword(2, 1, "r", TokenType.FOR);
                    },
                    'u' => {
                        return checkKeyword(2, 1, "n", TokenType.FUN);
                    },
                    else => {},
                }
            }
        },
        'i' => {
            return checkKeyword(1, 1, "f", TokenType.IF);
        },
        'n' => {
            return checkKeyword(1, 2, "il", TokenType.NIL);
        },
        'o' => {
            return checkKeyword(1, 1, "r", TokenType.OR);
        },
        'p' => {
            return checkKeyword(1, 4, "rint", TokenType.PRINT);
        },
        'r' => {
            return checkKeyword(1, 5, "eturn", TokenType.RETURN);
        },
        's' => {
            return checkKeyword(1, 4, "uper", TokenType.SUPER);
        },
        't' => {
            if (scanner.current - scanner.start > 1) {
                switch (scanner.source[scanner.start + 1]) {
                    'h' => {
                        return checkKeyword(2, 2, "is", TokenType.FALSE);
                    },
                    'r' => {
                        return checkKeyword(2, 2, "ue", TokenType.TRUE);
                    },
                    else => {},
                }
            }
        },
        'v' => {
            return checkKeyword(1, 2, "ar", TokenType.VAR);
        },
        'w' => {
            return checkKeyword(1, 4, "hile", TokenType.WHILE);
        },
        else => {},
    }

    return TokenType.IDENTIFIER;
}

fn checkKeyword(offset: usize, length: usize, rest: []const u8, ttype: TokenType) TokenType {
    const first = scanner.start + offset;
    if (std.mem.eql(u8, scanner.source[first .. first + length], rest)) {
        return ttype;
    }

    return TokenType.IDENTIFIER;
}

fn number() Token {
    while (isDigit(peek())) {
        _ = advance();
    }

    // Look for a fractional part.
    if (peek() == '.' and isDigit(peekNext())) {
        // Consume the ".".
        _ = advance();

        while (isDigit(peek())) {
            _ = advance();
        }
    }

    return makeToken(TokenType.NUMBER);
}

fn string() Token {
    while (peek() != '"' and !isAtEnd()) {
        if (peek() == '\n') scanner.line += 1;
        _ = advance();
    }

    if (isAtEnd()) return errorToken("Unterminated string.");

    // The closing quote.
    _ = advance();

    return makeToken(TokenType.STRING);
}

fn isAtEnd() bool {
    return scanner.current >= scanner.source.len - 1;
}

fn match(expected: u8) bool {
    if (isAtEnd()) return false;
    if (scanner.source[scanner.current] != expected) return false;
    scanner.current += 1;
    return true;
}

fn advance() u8 {
    scanner.current += 1;
    return scanner.source[scanner.current - 1];
}

fn makeToken(tt: TokenType) Token {
    return Token{
        .ttype = tt,
        .start = scanner.start,
        .length = scanner.current - scanner.start,
        .line = scanner.line,
    };
}

fn errorToken(message: []const u8) Token {
    return Token{
        .ttype = TokenType.ERROR,
        // TODO: This doesn't point to the source code, so gets stored differently
        .start = @ptrToInt(&message),
        .length = message.len,
        .line = scanner.line,
    };
}

pub const Scanner = struct {
    source: []u8,
    start: usize,
    current: usize,
    line: usize,
};

// TODO: encapsulate behavior in a struct, instead of global var
var scanner: Scanner = undefined;

pub fn initScanner(source: []u8) void {
    scanner = Scanner{
        .source = source,
        .start = 0,
        .current = 0,
        .line = 1,
    };
}

pub fn getScanner() Scanner {
    return scanner;
}
