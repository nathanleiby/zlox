const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

const DEBUG_PRINT_TOKENS = true;

pub const TokenType = enum {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,
    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    ERROR,
    EOF,
};

pub const Token = struct {
    ttype: TokenType,
    // TODO: replace this with 'lexeme: []const u8'
    start: usize, // []u8
    length: usize,
    line: usize,
};

pub const Scanner = struct {
    source: []u8,
    start: usize,
    current: usize,
    line: usize,

    pub fn init(source: []u8) Scanner {
        return Scanner{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    fn skipWhitespace(scanner: *Scanner) void {
        while (!scanner.isAtEnd()) {
            const c = scanner.peek();
            if (c == ' ' or c == '\r' or c == '\t') {
                _ = scanner.advance();
            } else if (c == '\n') {
                scanner.line += 1;
                _ = scanner.advance();
            } else if (c == '/') {
                if (scanner.peekNext() == '/') {
                    // A comment goes until the end of the line.
                    while (scanner.peek() != '\n' and !scanner.isAtEnd()) {
                        _ = scanner.advance();
                    }
                }
            } else {
                return;
            }
        }
    }

    fn peek(scanner: *Scanner) u8 {
        return scanner.source[scanner.current];
    }

    fn peekNext(scanner: *Scanner) u8 {
        // TODO: Explore use of https://ziglang.org/documentation/master/#Sentinel-Terminated-Slices
        if (scanner.isAtEnd()) return 0; // can I handle 0s like \0 would work in C implementation?
        return scanner.source[scanner.current + 1];
    }

    pub fn scanToken(scanner: *Scanner) Token {
        scanner.skipWhitespace();
        scanner.start = scanner.current;

        if (scanner.isAtEnd()) return scanner.makeToken(TokenType.EOF);

        const c = scanner.advance();

        if (isDigit(c)) return scanner.number();
        if (isAlpha(c)) return scanner.identifier();

        if (c == '(') return scanner.makeToken(TokenType.LEFT_PAREN);
        if (c == ')') return scanner.makeToken(TokenType.RIGHT_PAREN);
        if (c == '{') return scanner.makeToken(TokenType.LEFT_BRACE);
        if (c == '}') return scanner.makeToken(TokenType.RIGHT_BRACE);
        if (c == ';') return scanner.makeToken(TokenType.SEMICOLON);
        if (c == ',') return scanner.makeToken(TokenType.COMMA);
        if (c == '.') return scanner.makeToken(TokenType.DOT);
        if (c == '-') return scanner.makeToken(TokenType.MINUS);
        if (c == '+') return scanner.makeToken(TokenType.PLUS);
        if (c == '/') return scanner.makeToken(TokenType.SLASH);
        if (c == '*') return scanner.makeToken(TokenType.STAR);
        if (c == '!') {
            const ttype = if (scanner.match('=')) TokenType.BANG_EQUAL else TokenType.BANG;
            return scanner.makeToken(ttype);
        }
        if (c == '=') {
            const ttype = if (scanner.match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL;
            return scanner.makeToken(ttype);
        }
        if (c == '<') {
            const ttype = if (scanner.match('=')) TokenType.LESS_EQUAL else TokenType.LESS;
            return scanner.makeToken(ttype);
        }
        if (c == '>') {
            const ttype = if (scanner.match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER;
            return scanner.makeToken(ttype);
        }
        if (c == '"') return scanner.string();

        var msg: []const u8 = "Unexpected character.";
        return scanner.errorToken(msg);
    }

    fn identifier(scanner: *Scanner) Token {
        while (isAlphaNumeric(scanner.peek())) {
            _ = scanner.advance();
        }

        // const ttype = lookupIdentifier(scanner.source[scanner.start..scanner.current]);
        return scanner.makeToken(scanner.identifierType());
    }

    fn identifierType(scanner: *Scanner) TokenType {
        switch (scanner.source[scanner.start]) {
            'a' => {
                return scanner.checkKeyword(1, 2, "nd", TokenType.AND);
            },
            'c' => {
                return scanner.checkKeyword(1, 4, "lass", TokenType.CLASS);
            },
            'e' => {
                return scanner.checkKeyword(1, 3, "lse", TokenType.ELSE);
            },
            'f' => {
                if (scanner.current - scanner.start > 1) {
                    switch (scanner.source[scanner.start + 1]) {
                        'a' => {
                            return scanner.checkKeyword(2, 3, "lse", TokenType.FALSE);
                        },
                        'o' => {
                            return scanner.checkKeyword(2, 1, "r", TokenType.FOR);
                        },
                        'u' => {
                            return scanner.checkKeyword(2, 1, "n", TokenType.FUN);
                        },
                        else => {},
                    }
                }
            },
            'i' => {
                return scanner.checkKeyword(1, 1, "f", TokenType.IF);
            },
            'n' => {
                return scanner.checkKeyword(1, 2, "il", TokenType.NIL);
            },
            'o' => {
                return scanner.checkKeyword(1, 1, "r", TokenType.OR);
            },
            'p' => {
                return scanner.checkKeyword(1, 4, "rint", TokenType.PRINT);
            },
            'r' => {
                return scanner.checkKeyword(1, 5, "eturn", TokenType.RETURN);
            },
            's' => {
                return scanner.checkKeyword(1, 4, "uper", TokenType.SUPER);
            },
            't' => {
                if (scanner.current - scanner.start > 1) {
                    switch (scanner.source[scanner.start + 1]) {
                        'h' => {
                            return scanner.checkKeyword(2, 2, "is", TokenType.FALSE);
                        },
                        'r' => {
                            return scanner.checkKeyword(2, 2, "ue", TokenType.TRUE);
                        },
                        else => {},
                    }
                }
            },
            'v' => {
                return scanner.checkKeyword(1, 2, "ar", TokenType.VAR);
            },
            'w' => {
                return scanner.checkKeyword(1, 4, "hile", TokenType.WHILE);
            },
            else => {},
        }

        return TokenType.IDENTIFIER;
    }

    fn checkKeyword(scanner: *Scanner, offset: usize, length: usize, rest: []const u8, ttype: TokenType) TokenType {
        const first = scanner.start + offset;
        if (std.mem.eql(u8, scanner.source[first .. first + length], rest)) {
            return ttype;
        }

        return TokenType.IDENTIFIER;
    }

    fn number(scanner: *Scanner) Token {
        while (isDigit(scanner.peek())) {
            _ = scanner.advance();
        }

        // Look for a fractional part.
        if (scanner.peek() == '.' and isDigit(scanner.peekNext())) {
            // Consume the ".".
            _ = scanner.advance();

            while (isDigit(scanner.peek())) {
                _ = scanner.advance();
            }
        }

        return scanner.makeToken(TokenType.NUMBER);
    }

    fn string(scanner: *Scanner) Token {
        while (scanner.peek() != '"' and !scanner.isAtEnd()) {
            if (scanner.peek() == '\n') scanner.line += 1;
            _ = scanner.advance();
        }

        if (scanner.isAtEnd()) return scanner.errorToken("Unterminated string.");

        // The closing quote.
        _ = scanner.advance();

        return scanner.makeToken(TokenType.STRING);
    }

    fn isAtEnd(scanner: *Scanner) bool {
        return scanner.current > scanner.source.len - 1;
    }

    fn match(scanner: *Scanner, expected: u8) bool {
        if (scanner.isAtEnd()) return false;
        if (scanner.source[scanner.current] != expected) return false;
        scanner.current += 1;
        return true;
    }

    fn advance(scanner: *Scanner) u8 {
        scanner.current += 1;
        return scanner.source[scanner.current - 1];
    }

    fn makeToken(scanner: *Scanner, tt: TokenType) Token {
        return Token{
            .ttype = tt,
            .start = scanner.start,
            .length = scanner.current - scanner.start,
            .line = scanner.line,
        };
    }

    fn errorToken(scanner: *Scanner, message: []const u8) Token {
        return Token{
            .ttype = TokenType.ERROR,
            // TODO: This doesn't point to the source code, so gets stored differently
            .start = @ptrToInt(&message),
            .length = message.len,
            .line = scanner.line,
        };
    }
};

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlphaNumeric(c: u8) bool {
    return isAlpha(c) or isDigit(c);
}
