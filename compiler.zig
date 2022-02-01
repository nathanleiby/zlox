const std = @import("std");
const print = std.debug.print;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const initScanner = @import("./scanner.zig").initScanner;
const getScanner = @import("./scanner.zig").getScanner;
const Scanner = @import("./scanner.zig").Scanner;
const scanToken = @import("./scanner.zig").scanToken;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;

const Parser = struct {
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,
};

var parser = Parser{
    // start with placeholder tokens
    .current = Token{
        .ttype = TokenType.ERROR,
        .start = 0,
        .length = 0,
        .line = 0,
    },
    .previous = Token{
        .ttype = TokenType.ERROR,
        .start = 0,
        .length = 0,
        .line = 0,
    },
    .hadError = false,
    .panicMode = false,
};

pub fn compile(source: []u8, chunk: *Chunk) !bool {
    initScanner(source);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advanceCompiler();
    // expression(); // TODO
    consume(TokenType.EOF, "Expect end of expression.");
    try endCompiler();
    return !parser.hadError;
}

var compilingChunk: *Chunk = undefined;

fn currentChunk() *Chunk {
    return compilingChunk;
}

fn endCompiler() !void {
    try emitReturn();
}

fn emitReturn() !void {
    try emitByte(@enumToInt(OpCode.OpReturn));
}
fn emitBytes(byte1: u8, byte2: u8) !void {
    try emitByte(byte1);
    try emitByte(byte2);
}

fn consume(ttype: TokenType, message: []const u8) void {
    if (parser.current.ttype == ttype) {
        advanceCompiler();
        return;
    }

    errorAtCurrent(message);
}

fn advanceCompiler() void {
    parser.previous = parser.current;

    while (true) {
        parser.current = scanToken();
        if (parser.current.ttype != TokenType.ERROR) break;

        errorAtCurrent("advanceCompiler error"); // TODO
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

fn emitByte(byte: u8) !void {
    try currentChunk().write(byte, parser.previous.line);
}
