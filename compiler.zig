const std = @import("std");
const print = std.debug.print;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const disassembleChunk = @import("./chunk.zig").disassembleChunk;

const initScanner = @import("./scanner.zig").initScanner;
const getScanner = @import("./scanner.zig").getScanner;
const Scanner = @import("./scanner.zig").Scanner;
const scanToken = @import("./scanner.zig").scanToken;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;

const Value = @import("./value.zig").Value;

const ObjManager = @import("./object.zig").ObjManager;

const MAX_CONSTANTS = 256;

var objManager: *ObjManager = undefined;
pub fn setObjManager(om: *ObjManager) void {
    objManager = om;
}

// Debugging flags
const DEBUG_PRINT_CODE = true; // TODO: try out comptime
const DEBUG_MODE = false;

const Precedence = enum {
    PREC_NONE, // base case
    PREC_ASSIGNMENT, // =
    PREC_OR, // or
    PREC_AND, // and
    PREC_EQUALITY, // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM, // + -
    PREC_FACTOR, // * /
    PREC_UNARY, // ! -
    PREC_CALL, // . ()
    PREC_PRIMARY,
};

const ParseRule = struct {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precedence,
};

const ParseFn = fn () void;

const numRules = 40;
var rules: [numRules]ParseRule = undefined;

fn initRules() void {
    rules[@enumToInt(TokenType.LEFT_PAREN)] = ParseRule{ .prefix = grouping, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.RIGHT_PAREN)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.LEFT_BRACE)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.RIGHT_BRACE)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.COMMA)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.DOT)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.MINUS)] = ParseRule{ .prefix = unary, .infix = binary, .precedence = Precedence.PREC_TERM };
    rules[@enumToInt(TokenType.PLUS)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.PREC_TERM };
    rules[@enumToInt(TokenType.SEMICOLON)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.SLASH)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.PREC_FACTOR };
    rules[@enumToInt(TokenType.STAR)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.PREC_FACTOR };
    rules[@enumToInt(TokenType.BANG)] = ParseRule{ .prefix = unary, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.BANG_EQUAL)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.PREC_EQUALITY };
    rules[@enumToInt(TokenType.EQUAL)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.EQUAL_EQUAL)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.PREC_EQUALITY };
    rules[@enumToInt(TokenType.GREATER)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.PREC_COMPARISON };
    rules[@enumToInt(TokenType.GREATER_EQUAL)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.PREC_COMPARISON };
    rules[@enumToInt(TokenType.LESS)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.PREC_COMPARISON };
    rules[@enumToInt(TokenType.LESS_EQUAL)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.PREC_COMPARISON };
    rules[@enumToInt(TokenType.IDENTIFIER)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.STRING)] = ParseRule{ .prefix = string, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.NUMBER)] = ParseRule{ .prefix = number, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.AND)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.CLASS)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.ELSE)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.FALSE)] = ParseRule{ .prefix = literal, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.FOR)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.FUN)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.IF)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.NIL)] = ParseRule{ .prefix = literal, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.OR)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.PRINT)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.RETURN)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.SUPER)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.THIS)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.TRUE)] = ParseRule{ .prefix = literal, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.VAR)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.WHILE)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.ERROR)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };
    rules[@enumToInt(TokenType.EOF)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.PREC_NONE };

    return;
}

const Parser = struct {
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,
};

var parser = Parser{
    // start with placeholder tokens
    .current = undefined,
    .previous = undefined,
    .hadError = false,
    .panicMode = false,
};

pub fn compile(source: []u8, chunk: *Chunk, om: *ObjManager) !bool {
    // startup -- could be comptime TODO
    initRules();
    setObjManager(om);

    initScanner(source);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advance(); // prime
    // scan tokens
    while (!match(TokenType.EOF)) {
        declaration();
    }

    endCompiler();
    return !parser.hadError;
}

fn synchronize() void {
    parser.panicMode = false;

    while (parser.current.ttype != TokenType.EOF) {
        if (parser.previous.ttype == TokenType.SEMICOLON) return;
        switch (parser.current.ttype) {
            .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
            else => {}, // do nothing
        }

        advance();
    }
}

fn match(ttype: TokenType) bool {
    if (DEBUG_MODE) print("match() parser.current = {any}\n", .{parser.current});
    if (!check(ttype)) return false;
    advance();
    return true;
}

fn check(ttype: TokenType) bool {
    if (DEBUG_MODE) print("check() parser.current = {any}\n", .{parser.current});
    return (parser.current.ttype == ttype);
}

fn declaration() void {
    statement();

    if (parser.panicMode) {
        // we hit a compile error while parsing the previous statement.
        // try to resume from the next statement.
        synchronize();
    }
}

fn statement() void {
    if (match(TokenType.PRINT)) {
        printStatement();
    } else {
        expressionStatement();
    }
}

fn printStatement() void {
    expression();
    consume(TokenType.SEMICOLON, "Expect ';' after value.");
    emitByte(@enumToInt(OpCode.OpPrint));
}

fn expressionStatement() void {
    expression();
    consume(TokenType.SEMICOLON, "Expect ';' after expression.");
    emitByte(@enumToInt(OpCode.OpPop));
}

fn expression() void {
    parsePrecedence(Precedence.PREC_ASSIGNMENT);
}

fn literal() void {
    switch (parser.previous.ttype) {
        .FALSE => {
            emitByte(@enumToInt(OpCode.OpFalse));
        },
        .TRUE => {
            emitByte(@enumToInt(OpCode.OpTrue));
        },
        .NIL => {
            emitByte(@enumToInt(OpCode.OpNil));
        },
        else => {
            // unreachable
            return;
        },
    }
}

fn number() void {
    // TODO: for now, falls back to 0 instead of erroring
    const value = std.fmt.parseFloat(f64, tokenString(parser.previous)) catch 0;
    emitConstant(Value{ .number = value });
}

fn grouping() void {
    // we assume the initial '(' has already been consumed.
    expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
}

fn string() void {
    const ts = tokenString(parser.previous);
    // extract the string's value, trimming the surrounding quotes
    const chars = ts[1 .. ts.len - 1];
    var s = objManager.copyString(chars) catch {
        errorAtCurrent("Unable to allocate string");
        return;
    };
    const v = Value{ .objString = s };
    emitConstant(v);
}

fn unary() void {
    const operatorType: TokenType = parser.previous.ttype;

    // Compile the operand.
    parsePrecedence(Precedence.PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        .MINUS => {
            emitByte(@enumToInt(OpCode.OpNegate));
        },
        .BANG => {
            emitByte(@enumToInt(OpCode.OpNot));
        },
        else => {
            unreachable;
        },
    }
}

fn binary() void {
    const operatorType: TokenType = parser.previous.ttype;
    const rule: *ParseRule = getRule(operatorType);
    parsePrecedence(@intToEnum(Precedence, (@enumToInt(rule.precedence) + 1)));

    switch (operatorType) {
        .PLUS => {
            emitByte(@enumToInt(OpCode.OpAdd));
        },
        .MINUS => {
            emitByte(@enumToInt(OpCode.OpSubtract));
        },
        .STAR => {
            emitByte(@enumToInt(OpCode.OpMultiply));
        },
        .SLASH => {
            emitByte(@enumToInt(OpCode.OpDivide));
        },
        .BANG_EQUAL => {
            emitBytes(@enumToInt(OpCode.OpEqual), @enumToInt(OpCode.OpNot));
        },
        .EQUAL_EQUAL => {
            emitByte(@enumToInt(OpCode.OpEqual));
        },
        .GREATER_EQUAL => {
            emitBytes(@enumToInt(OpCode.OpGreater), @enumToInt(OpCode.OpEqual));
        },
        .GREATER => {
            emitByte(@enumToInt(OpCode.OpGreater));
        },
        .LESS_EQUAL => {
            emitBytes(@enumToInt(OpCode.OpLess), @enumToInt(OpCode.OpEqual));
        },
        .LESS => {
            emitByte(@enumToInt(OpCode.OpLess));
        },
        else => {
            // unreachable
            return;
        },
    }
}

fn getRule(ttype: TokenType) *ParseRule {
    return &rules[@enumToInt(ttype)];
}

fn parsePrecedence(precedence: Precedence) void {
    advance();
    const rule = getRule(parser.previous.ttype);
    const prefixRule = rule.prefix;
    print("prefixRule {s}: {s}\n", .{ parser.previous.ttype, rule }); // TODO: this is weird but makes things work. ZIGGGGGG
    if (prefixRule == undefined) {
        err("Expect expression.");
        return;
    }

    prefixRule();
    while (@enumToInt(precedence) <= @enumToInt(getRule(parser.current.ttype).precedence)) {
        advance();
        const infixRule = getRule(parser.previous.ttype).infix;
        infixRule();
    }
}

var compilingChunk: *Chunk = undefined;

fn currentChunk() *Chunk {
    return compilingChunk;
}

fn endCompiler() void {
    emitReturn();
    if (DEBUG_PRINT_CODE) {
        if (!parser.hadError) {
            disassembleChunk(currentChunk().*, "code");
        }
    }
}

fn emitConstant(value: Value) void {
    emitBytes(@enumToInt(OpCode.OpConstant), makeConstant(value));
}

fn makeConstant(value: Value) u8 {
    const constIdx = currentChunk().addConstant(value) catch {
        err("Failed to add constant.");
        return 0;
    };
    if (constIdx > MAX_CONSTANTS) {
        err("Too many constants in one chunk.");
        return 0;
    }

    return @truncate(u8, constIdx);
}

fn emitReturn() void {
    emitByte(@enumToInt(OpCode.OpReturn));
}

fn emitBytes(byte1: u8, byte2: u8) void {
    emitByte(byte1);
    emitByte(byte2);
}

fn consume(ttype: TokenType, message: []const u8) void {
    if (DEBUG_MODE) print("consume({any}) parser.current = {any}\n", .{ ttype, parser.current });
    if (parser.current.ttype == ttype) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

fn advance() void {
    parser.previous = parser.current;

    while (true) {
        parser.current = scanToken();
        if (DEBUG_MODE) print("advance() .previous = {any} .current = {any}\n", .{ parser.previous, parser.current });
        if (parser.current.ttype != TokenType.ERROR) break;

        errorAtCurrent("advance() error"); // TODO
    }
}

fn errorAtCurrent(message: []const u8) void {
    errorAt(&parser.current, message);
}

fn err(message: []const u8) void {
    errorAt(&parser.previous, message);
}

fn errorAt(token: *Token, message: []const u8) void {
    if (parser.panicMode) return;

    print("[line {d}] Error", .{token.line});

    if (token.ttype == TokenType.EOF) {
        print(" at end", .{});
    } else if (token.ttype == TokenType.ERROR) {
        // do nothing
    } else {
        print(" at '{s}'", .{getScanner().source[token.start .. token.start + token.length]});
    }

    print(": {s}\n", .{message});

    parser.hadError = true;
}

fn emitByte(byte: u8) void {
    currentChunk().write(byte, parser.previous.line) catch {};
}

fn tokenString(token: Token) []const u8 {
    return getScanner().source[token.start .. token.start + token.length];
}
