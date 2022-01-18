const std = @import("std");
const print = std.debug.print;
// TODO: consider dep injecting this, so i can use a test one too
const allocator = std.heap.page_allocator;
const expect = std.testing.expect;

const DEBUG_TRACE_EXECUTION = true; // debug mode
const DEBUG_TRACE_EXECUTION_INCLUDE_INSTRUCTIONS = false;

const OpCode = enum(usize) {
    OpReturn,
    OpConstant,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNegate,
};

const InterpretResult = enum {
    InterpretOk,
    InterpretCompileError,
    InterpretRuntimeError,
};

const TokenType = enum {
// Single-character tokens.
LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,
// One or two character tokens.
BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,
// Literals.
IDENTIFIER, STRING, NUMBER,
// Keywords.
AND, CLASS, ELSE, FALSE, FOR, FUN, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE, ERROR, EOF };

const Chunk = struct {
    code: *std.ArrayList(usize),
    lines: *std.ArrayList(i32),
    values: *std.ArrayList(f64),

    pub fn free(self: Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.values.deinit();
    }

    pub fn write(self: Chunk, byte: usize, line: i32) !void {
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn addConstant(self: Chunk, value: f64) !usize {
        try self.values.append(value);
        return self.values.items.len - 1;
    }
};

const VM = struct {
    chunk: Chunk,
    stack: *std.ArrayList(f64),

    // ip is the instruction pointer. it points to the instruction about to be executed; not the one currently being handled
    // ip: usize = 0,
    // ticker: usize = 0,

    pub fn free(self: VM) void {
        self.chunk.free();
        self.stack.deinit();
    }

    pub fn interpret(self: VM, _: *Chunk) !InterpretResult {
        // var foo = c;
        // self.ip = c.code;
        return self.run();
    }

    fn binaryAdd(self: VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();
        try self.stack.append(a + b);
    }

    fn binarySubtract(self: VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();
        try self.stack.append(a - b);
    }

    fn binaryMultiply(self: VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();
        try self.stack.append(a * b);
    }

    fn binaryDivide(self: VM) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();
        try self.stack.append(a / b);
    }

    fn run(self: VM) !InterpretResult {
        var ip: usize = 0; // instruction pointer
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                if (DEBUG_TRACE_EXECUTION_INCLUDE_INSTRUCTIONS) {
                    _ = disassembleInstruction(self.chunk, ip);
                }

                // print the stack
                print("          ", .{});
                for (self.stack.items) |slot| {
                    print("[{d}]", .{slot});
                }
                print("\n", .{});
            }

            const byte = self.chunk.code.items[ip];
            const instruction = @intToEnum(OpCode, byte);
            ip += 1;
            switch (instruction) {
                OpCode.OpReturn => {
                    print("Return: {d}", .{self.stack.pop()});
                    print("\n", .{});
                    return InterpretResult.InterpretOk;
                },
                OpCode.OpNegate => {
                    try self.stack.append(-self.stack.pop());
                },
                OpCode.OpConstant => {
                    const constantIdx = self.chunk.code.items[ip];
                    ip += 1;
                    const constant = self.chunk.values.items[constantIdx];
                    try self.stack.append(constant);
                },
                OpCode.OpAdd => {
                    try self.binaryAdd();
                },
                OpCode.OpSubtract => {
                    try self.binarySubtract();
                },
                OpCode.OpMultiply => {
                    try self.binaryMultiply();
                },
                OpCode.OpDivide => {
                    try self.binaryDivide();
                },
                // else => {
                //     return InterpretResult.InterpretCompileError;
                // },
            }
        }

        return InterpretResult.InterpretOk;
    }
};

fn disassembleChunk(chunk: Chunk) void {
    print("== {s} ==\n", .{"chunk"});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

fn disassembleInstruction(chunk: Chunk, offset: usize) usize {
    const byte = chunk.code.items[offset];
    print("{:04} ", .{offset});
    // line
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        print("   | ", .{});
    } else {
        print("{:04} ", .{chunk.lines.items[offset]});
    }

    const item = @intToEnum(OpCode, byte);
    switch (item) {
        OpCode.OpReturn => {
            print("OP_RETURN            ", .{});
            print("\n", .{});
            return offset + 1;
        },
        OpCode.OpConstant => {
            print("OP_CONSTANT          ", .{});
            const constantIdx = chunk.code.items[offset + 1];
            print("{:04} -- ", .{constantIdx});
            print("{d}", .{chunk.values.items[constantIdx]});
            print("\n", .{});
            return offset + 2;
        },
        OpCode.OpNegate => {
            print("OP_NEGATE            ", .{});
            const constantIdx = chunk.code.items[offset + 1];
            print("{:04} -- ", .{constantIdx});
            print("{d}", .{chunk.values.items[constantIdx]});
            print("\n", .{});
            return offset + 1;
        },
        OpCode.OpAdd => {
            print("OP_ADD               ", .{});
            print("\n", .{});
            return offset + 1;
        },
        OpCode.OpSubtract => {
            print("OP_SUBTRACT          ", .{});
            print("\n", .{});
            return offset + 1;
        },
        OpCode.OpMultiply => {
            print("OP_MULTIPLY          ", .{});
            print("\n", .{});
            return offset + 1;
        },
        OpCode.OpDivide => {
            print("OP_DIVIDE            ", .{});
            print("\n", .{});
            return offset + 1;
        },
    }
}

pub fn main() !void {
    var chunk = Chunk{
        .code = &std.ArrayList(usize).init(allocator),
        .lines = &std.ArrayList(i32).init(allocator),
        .values = &std.ArrayList(f64).init(allocator),
    };
    const vm = VM{
        .chunk = chunk,
        .stack = &std.ArrayList(f64).init(allocator),
    };
    defer vm.free();

    // parse args
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const ally = &arena.allocator;

    const stdout = std.io.getStdOut().writer();

    const args = try std.process.argsAlloc(ally);
    if (args.len > 2) {
        print("Usage: zlox [path]\n", .{});
        std.os.exit(64);
        // try stdout.writeAll(usage);
        // return;
    } else if (args.len == 2) {
        try runFile(args[1]);
    } else {
        try repl();
    }
}

const maxFileSize: usize = 1024;
fn runFile(path: []u8) !void {
    print("runFile({s})\n", .{path});

    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source: []u8 = try file.readToEndAlloc(allocator, maxFileSize);

    const result: InterpretResult = interpret(source);

    if (result == InterpretResult.InterpretCompileError) std.os.exit(65);
    if (result == InterpretResult.InterpretRuntimeError) std.os.exit(70);
}

fn repl() !void {
    const line = try allocator.alloc(u8, 1024);
    while (true) {
        print("> ", .{});
        const stdin = std.io.getStdIn().reader();
        _ = try stdin.readUntilDelimiterOrEof(line[0..], '\n'); // TODO: break if error
        _ = interpret(line);
        print("\n", .{});
    }
    return;
}

fn interpret(source: []u8) InterpretResult {
    print("interpret({s})\n", .{source});
    compile(source);
    return InterpretResult.InterpretOk;
}

const Token = struct {
    ttype: TokenType,
    start: usize, // []u8
    length: usize,
    line: usize,
};

fn compile(source: []u8) void {
    initScanner(source);

    var line: usize = 100000; // TODO: chose a better sentinel value
    while (true) {
        const token: Token = scanToken();
        if (token.line != line) {
            print("line={d} ", .{token.line});
            //   print("%4d ", .{token.line});
            line = token.line;
        } else {
            print("   | ", .{});
        }
        // TODO
        // printf("%2d '%.*s'\n", token.type, token.length, token.start);
        print("ttype={d} 'length={d} start={d}'\n", .{ token.ttype, token.length, token.start });
        print("\n", .{});

        // TODO: we don't yet emit this?
        if (token.ttype == TokenType.EOF) break;
    }
}

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
    if (isAtEnd()) return 0; // can I handle 0s like \0 would work in C implementation?
    return scanner.source[scanner.current + 1];
}

fn scanToken() Token {
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
                        return checkKeyword(2, 2, "ue", TokenType.FOR);
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

fn checkKeyword(start: usize, length: usize, rest: []const u8, ttype: TokenType) TokenType {
    if (scanner.current - scanner.start == start + length and "foo" == "bar") {
        // source[scanner.start + start..])
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
    // print("source len = {d} current = {d} char = {c}", .{ scanner.source.len, scanner.current, scanner.source[scanner.current] });
    // return scanner.source[scanner.current] == '\0'; // TODO: error: invalid character
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

const Scanner = struct {
    source: []u8,
    start: usize,
    current: usize,
    line: usize,
};

var scanner: Scanner = undefined;

fn initScanner(source: []u8) void {
    scanner = Scanner{
        .source = source,
        .start = 0,
        .current = 0,
        .line = 1,
    };
}

test "virtual machine can negate a value" {
    print("\n\n", .{}); // make space for test runner output

    var chunk = Chunk{
        .code = &std.ArrayList(usize).init(allocator),
        .lines = &std.ArrayList(i32).init(allocator),
        .values = &std.ArrayList(f64).init(allocator),
    };

    // free
    defer chunk.free();

    // write
    try chunk.write(@enumToInt(OpCode.OpConstant), 123);
    const constant = try chunk.addConstant(1.2);
    try chunk.write(constant, 1);

    try chunk.write(@enumToInt(OpCode.OpNegate), 123);

    try chunk.write(@enumToInt(OpCode.OpReturn), 123);

    // disassemble
    disassembleChunk(chunk);

    // interpret
    const vm = VM{
        .chunk = chunk,
        .stack = &std.ArrayList(f64).init(allocator),
    };
    const result = try vm.interpret(&chunk);
    print("Interpret result: {s}\n", .{result});
    try expect(result == InterpretResult.InterpretOk);
}

test "virtual machine can do some binary ops (add and divide)" {
    print("\n\n", .{}); // make space for test runner output

    var chunk = Chunk{
        .code = &std.ArrayList(usize).init(allocator),
        .lines = &std.ArrayList(i32).init(allocator),
        .values = &std.ArrayList(f64).init(allocator),
    };

    const fakeLineNumber = 123;

    // free
    defer chunk.free();

    // write
    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant = try chunk.addConstant(1.2);
    try chunk.write(constant, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant2 = try chunk.addConstant(3.4);
    try chunk.write(constant2, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpAdd), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant3 = try chunk.addConstant(5.6);
    try chunk.write(constant3, 1);

    try chunk.write(@enumToInt(OpCode.OpDivide), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpReturn), fakeLineNumber);

    // disassemble
    disassembleChunk(chunk);

    // interpret
    const vm = VM{
        .chunk = chunk,
        .stack = &std.ArrayList(f64).init(allocator),
    };
    const result = try vm.interpret(&chunk);
    print("Interpret result: {s}\n", .{result});
    try expect(result == InterpretResult.InterpretOk);
}

test "virtual machine can do all binary ops (add, subtract, multiply, divide)" {
    // target: 1 + 2 * 3 - 4 / -5
    // note that this gets evaluated like so: ((((1+2) * 3) - 4) / -5)

    print("\n\n", .{}); // make space for test runner output

    var chunk = Chunk{
        .code = &std.ArrayList(usize).init(allocator),
        .lines = &std.ArrayList(i32).init(allocator),
        .values = &std.ArrayList(f64).init(allocator),
    };

    const fakeLineNumber = 123;

    // free
    defer chunk.free();

    // write
    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant = try chunk.addConstant(1);
    try chunk.write(constant, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant2 = try chunk.addConstant(2);
    try chunk.write(constant2, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpAdd), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant3 = try chunk.addConstant(3);
    try chunk.write(constant3, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpMultiply), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant4 = try chunk.addConstant(4);
    try chunk.write(constant4, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpSubtract), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpConstant), fakeLineNumber);
    const constant5 = try chunk.addConstant(5);
    try chunk.write(constant5, fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpNegate), fakeLineNumber);

    // TODO(bug): Why does 'divide' specifically crash here?
    // try chunk.write(@enumToInt(OpCode.OpDivide), fakeLineNumber);
    try chunk.write(@enumToInt(OpCode.OpMultiply), fakeLineNumber);

    try chunk.write(@enumToInt(OpCode.OpReturn), fakeLineNumber);

    // disassemble
    disassembleChunk(chunk);

    // interpret
    const vm = VM{
        .chunk = chunk,
        .stack = &std.ArrayList(f64).init(allocator),
    };
    const result = try vm.interpret(&chunk);
    print("Interpret result: {s}\n", .{result});
    try expect(result == InterpretResult.InterpretOk);
}

// test "compiler can emit tokens" {
// INPUT = 'print 1 + 2;'
//   1 31 'print'
//    | 21 '1'
//    |  7 '+'
//    | 21 '2'
//    |  8 ';'
//    2 39 ''
// }
