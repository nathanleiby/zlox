const std = @import("std");
const print = std.debug.print;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const disassembleChunk = @import("./chunk.zig").disassembleChunk;

const Scanner = @import("./scanner.zig").Scanner;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;

const Value = @import("./value.zig").Value;

const ObjManager = @import("./object.zig").ObjManager;
const ObjString = @import("./object.zig").ObjString;
const ObjFunction = @import("./object.zig").ObjFunction;

const U8_MAX = @import("./constants.zig").U8_MAX;
const U8_COUNT = @import("./constants.zig").U8_COUNT;
const U16_MAX = @import("./constants.zig").U16_MAX;
const MAX_CONSTANTS = 256;

const compilerError = error{
    TODO,
    LocalNotFound,
    UpvalueNotFound,
    ParserError,
};

// Debugging flags
const debug = @import("./debug.zig");

const Precedence = enum {
    None, // base case
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,
};

const ParseRule = struct {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precedence,
};

// TODO: refactor so parser fns can error out. right now I'm using catch {} to workaround
// https://ziglang.org/documentation/master/#toc-Errors
// const ParseError = error {
//
// };

// const ParseFn = fn () ParseError!void;

const ParseFn = fn (canAssign: bool) void;

const numRules = 40;
var rules: [numRules]ParseRule = undefined; // TODO: Is it possible to declare this more directly (vs numRules and initRules())

fn initRules() void {
    rules[@enumToInt(TokenType.LEFT_PAREN)] = ParseRule{ .prefix = grouping, .infix = call, .precedence = Precedence.Call };
    rules[@enumToInt(TokenType.RIGHT_PAREN)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.LEFT_BRACE)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.RIGHT_BRACE)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.COMMA)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.DOT)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.MINUS)] = ParseRule{ .prefix = unary, .infix = binary, .precedence = Precedence.Term };
    rules[@enumToInt(TokenType.PLUS)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.Term };
    rules[@enumToInt(TokenType.SEMICOLON)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.SLASH)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.Factor };
    rules[@enumToInt(TokenType.STAR)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.Factor };
    rules[@enumToInt(TokenType.BANG)] = ParseRule{ .prefix = unary, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.BANG_EQUAL)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.Equality };
    rules[@enumToInt(TokenType.EQUAL)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.EQUAL_EQUAL)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.Equality };
    rules[@enumToInt(TokenType.GREATER)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.Comparison };
    rules[@enumToInt(TokenType.GREATER_EQUAL)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.Comparison };
    rules[@enumToInt(TokenType.LESS)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.Comparison };
    rules[@enumToInt(TokenType.LESS_EQUAL)] = ParseRule{ .prefix = undefined, .infix = binary, .precedence = Precedence.Comparison };
    rules[@enumToInt(TokenType.IDENTIFIER)] = ParseRule{ .prefix = variable, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.STRING)] = ParseRule{ .prefix = string, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.NUMBER)] = ParseRule{ .prefix = number, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.AND)] = ParseRule{ .prefix = undefined, .infix = logicalAnd, .precedence = Precedence.And };
    rules[@enumToInt(TokenType.CLASS)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.ELSE)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.FALSE)] = ParseRule{ .prefix = literal, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.FOR)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.FUN)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.IF)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.NIL)] = ParseRule{ .prefix = literal, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.OR)] = ParseRule{ .prefix = undefined, .infix = logicalOr, .precedence = Precedence.Or };
    rules[@enumToInt(TokenType.PRINT)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.RETURN)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.SUPER)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.THIS)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.TRUE)] = ParseRule{ .prefix = literal, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.VAR)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.WHILE)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.ERROR)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };
    rules[@enumToInt(TokenType.EOF)] = ParseRule{ .prefix = undefined, .infix = undefined, .precedence = Precedence.None };

    return;
}

const Parser = struct {
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,
};

const Local = struct {
    token: Token,
    depth: i16,
};

const Upvalue = struct {
    index: u8,
    isLocal: bool,
};

const FunctionType = enum {
    Function,
    Script,
};

const Compiler = struct {
    function: *ObjFunction,
    type_: FunctionType,

    enclosing: *Compiler,

    locals: [U8_COUNT]Local,
    localCount: u8,
    upvalues: [U8_COUNT]Upvalue,
    scopeDepth: i16,

    pub fn init(type_: FunctionType, om: *ObjManager, enclosing: *Compiler) !Compiler {
        var c = Compiler{
            .function = undefined, // see 24.2.1 .. revisit in light of garbage collector
            .type_ = type_,
            .locals = [_]Local{Local{ .token = undefined, .depth = 0 }} ** U8_COUNT,
            .localCount = 1, // claim 1 local as a placeholder
            .upvalues = [_]Upvalue{Upvalue{ .index = 0, .isLocal = false }} ** U8_COUNT,
            .scopeDepth = 0,
            .enclosing = enclosing,
        };
        c.function = try om.newFunction();
        if (type_ != FunctionType.Script) {
            c.function.name = try om.copyString(tokenString(parser.previous));
        }

        return c;
    }
};

// TODO: use a class-like setup here instead of global var
var current: *Compiler = undefined;
var scanner: Scanner = undefined;
var parser: Parser = undefined;
var objManager: *ObjManager = undefined;

pub fn compile(source: []u8, om: *ObjManager) !*ObjFunction {
    // startup -- could be comptime TODO
    initRules();

    // setup the compiler
    objManager = om;
    var compiler = try Compiler.init(FunctionType.Script, objManager, current);
    current = &compiler;

    scanner = Scanner.init(source);
    parser = Parser{
        // start with placeholder tokens
        .current = undefined,
        .previous = undefined,
        .hadError = false,
        .panicMode = false,
    };

    // run the parser/compiler
    advance(); // prime
    // scan tokens
    while (!match(TokenType.EOF)) {
        try declaration();
    }
    const func = endCompiler();
    if (parser.hadError) {
        return compilerError.ParserError;
    } else {
        return func;
    }
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
    if (debug.TRACE_PARSER) print("match() parser.current = {any}\n", .{parser.current});
    if (!check(ttype)) return false;
    advance();
    return true;
}

fn check(ttype: TokenType) bool {
    if (debug.TRACE_PARSER) print("check() parser.current = {any}\n", .{parser.current});
    return (parser.current.ttype == ttype);
}

////////////////////
// Declarations
////////////////////
// TODO: using compilerError for now to work around https://github.com/ziglang/zig/issues/2971
fn declaration() compilerError!void {
    if (match(TokenType.FUN)) {
        funDeclaration() catch return compilerError.TODO;
    } else if (match(TokenType.VAR)) {
        varDeclaration() catch return compilerError.TODO;
    } else {
        statement() catch return compilerError.TODO;
    }

    if (parser.panicMode) {
        // we hit a compile error while parsing the previous statement.
        // try to resume from the next statement.
        synchronize();
    }
}

fn funDeclaration() !void {
    const global = try parseVariable("Expect function name");
    markInitialized();
    try function(FunctionType.Function);
    defineVariable(global);
}

fn varDeclaration() !void {
    const global = try parseVariable("Expect variable name");
    if (match(TokenType.EQUAL)) {
        expression();
    } else {
        emitByte(@enumToInt(OpCode.Nil));
    }

    consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.");
    defineVariable(global);
}

fn parseVariable(errorMessage: []const u8) !u8 {
    consume(TokenType.IDENTIFIER, errorMessage);

    // handle local variables
    declareVariable();
    if (current.scopeDepth > 0) {
        return 0;
    }

    // handle global variables
    return try identifierConstant(parser.previous);
}

fn identifierConstant(token: Token) !u8 {
    const ts = tokenString(token);
    var s = try objManager.copyString(ts);
    return makeConstant(Value{ .objString = s });
}

fn defineVariable(constantsRef: u8) void {
    // handle local variables
    if (current.scopeDepth > 0) {
        markInitialized();
        return;
    }

    // store a reference to the variable in the constants table
    emitBytes(@enumToInt(OpCode.DefineGlobal), constantsRef);
}

fn declareVariable() void {
    // handle globle variables
    if (current.scopeDepth == 0) {
        return;
    }

    const curToken = parser.previous;

    // Check if we're declaring the same varible again within the current scope, i.e.
    // {
    //     var a = "first";
    //     var a = "second";
    // }
    // This should cause an error.
    var i: usize = 0;
    while (i < current.localCount) {
        const local = current.locals[i];
        if (local.depth != -1 and local.depth < current.scopeDepth) {
            // we've visited all the local variables that could collide
            break;
        }

        if (identifiersEqual(local.token, curToken)) {
            err("A variable already exists with this name in this scope.");
        }

        i += 1;
    }

    addLocal(curToken);
}

fn addLocal(token: Token) void {
    if (current.localCount == U8_COUNT) {
        err("Too many local variables in function.");
        return;
    }

    const local: *Local = &current.locals[current.localCount];
    current.localCount += 1;
    local.token = token;
    local.depth = -1; // -1 denotes uninitialized

}

fn markInitialized() void {
    if (current.scopeDepth == 0) return; // function bound to a global variable
    current.locals[current.localCount - 1].depth = current.scopeDepth;
}

fn logicalAnd(_: bool) void {
    var endJump = emitJump(OpCode.JumpIfFalse);

    emitByte(@enumToInt(OpCode.Pop));
    parsePrecedence(Precedence.And);

    patchJump(endJump);
}

fn logicalOr(_: bool) void {
    var elseJump = emitJump(OpCode.JumpIfFalse);
    var endJump = emitJump(OpCode.Jump);

    patchJump(elseJump);
    emitByte(@enumToInt(OpCode.Pop));

    parsePrecedence(Precedence.Or);
    patchJump(endJump);
}

////////////////////
// Statements
////////////////////
fn statement() compilerError!void {
    if (match(TokenType.PRINT)) {
        printStatement();
    } else if (match(TokenType.IF)) {
        ifStatement() catch return compilerError.TODO;
    } else if (match(TokenType.RETURN)) {
        returnStatement();
    } else if (match(TokenType.WHILE)) {
        whileStatement() catch return compilerError.TODO;
    } else if (match(TokenType.FOR)) {
        forStatement() catch return compilerError.TODO;
    } else if (match(TokenType.LEFT_BRACE)) {
        beginScope();
        block() catch return compilerError.TODO;
        endScope();
    } else {
        expressionStatement();
    }
}

fn beginScope() void {
    current.scopeDepth += 1;
}

fn endScope() void {
    current.scopeDepth -= 1;

    // when local variables leave scope, remove them from the stack
    while (current.localCount > 0 and current.locals[current.localCount - 1].depth > current.scopeDepth) {
        emitByte(@enumToInt(OpCode.Pop));
        current.localCount -= 1;
    }
}

fn block() !void {
    while (!check(TokenType.RIGHT_BRACE) and !check(TokenType.EOF)) {
        try declaration();
    }

    consume(TokenType.RIGHT_BRACE, "Expect '}' after block.");
}

fn parseFunctionArg() !void {
    current.function.arity += 1;
    if (current.function.arity > 255) {
        errorAtCurrent("Can't have more than 255 parameters.");
    }
    const constant = try parseVariable("Expect parameter name.");
    defineVariable(constant);
}

fn function(type_: FunctionType) !void {
    var newCompiler = try Compiler.init(type_, objManager, current);
    current = &newCompiler;

    beginScope();

    consume(TokenType.LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TokenType.RIGHT_PAREN)) {
        try parseFunctionArg();
        while (match(TokenType.COMMA)) {
            try parseFunctionArg();
        }
    }
    consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TokenType.LEFT_BRACE, "Expect '{' before function body.");
    try block();

    const func: *ObjFunction = endCompiler();
    emitBytes(@enumToInt(OpCode.Closure), makeConstant(Value{ .objFunction = func }));
}

fn printStatement() void {
    expression();
    consume(TokenType.SEMICOLON, "Expect ';' after value.");
    emitByte(@enumToInt(OpCode.Print));
}

fn returnStatement() void {
    if (current.type_ == FunctionType.Script) {
        err("Can't return from top-level code.");
    }

    if (match(TokenType.SEMICOLON)) {
        // emitByte(@enumToInt(OpCode.Nil));
        emitReturn();
    } else {
        expression();
        consume(TokenType.SEMICOLON, "Expect ';' after return value.");
        emitByte(@enumToInt(OpCode.Return));
    }
}

fn ifStatement() !void {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after 'if' condition.");

    var thenJump = emitJump(OpCode.JumpIfFalse);
    emitByte(@enumToInt(OpCode.Pop)); // pop off condition expression's value
    try statement();

    var elseJump = emitJump(OpCode.Jump);

    patchJump(thenJump);
    emitByte(@enumToInt(OpCode.Pop)); // pop off condition expression's value

    // Handle 'else' (optional)
    if (match(TokenType.ELSE)) {
        try statement();
    }
    patchJump(elseJump);
}

fn emitJump(op: OpCode) usize {
    emitByte(@enumToInt(op));
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk().count() - 2;
}

fn patchJump(offset: usize) void {
    const jump = currentChunk().count() - offset - 2;

    if (jump > U16_MAX) {
        err("Too much code to jump over.");
    }

    currentChunk().code.items[offset] = (jump >> 8) & 0xff;
    currentChunk().code.items[offset + 1] = jump & 0xff;
}

fn whileStatement() !void {
    const loopStart = currentChunk().count();

    consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after 'while' condition.");

    var exitJump = emitJump(OpCode.JumpIfFalse);
    emitByte(@enumToInt(OpCode.Pop)); // pop off condition expressio's value if we enter the loop
    try statement();
    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(@enumToInt(OpCode.Pop)); // pop off condition expression's value as we exit the loop
}

fn forStatement() !void {
    beginScope(); // If a for statement declares a variable, that variable should be scoped to the loop body
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.");

    // (1) For-loop "Initializer"
    if (match(TokenType.SEMICOLON)) {
        // No initializer.
    } else if (match(TokenType.VAR)) {
        try varDeclaration();
    } else {
        expressionStatement();
    }

    var loopStart = currentChunk().count();

    // (2) For-loop "Condition"
    var exitJump: usize = 0;
    var hasExitJump = false;
    if (match(TokenType.SEMICOLON)) {
        // no condition, i.e. always true
    } else {
        expression();
        consume(TokenType.SEMICOLON, "Expect ';' after 'for' loop condition.");

        // Jump out of the loop if the condition is false.
        exitJump = emitJump(OpCode.JumpIfFalse);
        hasExitJump = true;
        emitByte(@enumToInt(OpCode.Pop)); // remove the condition expression's value
    }

    // (3) For-loop "Increment"
    if (match(TokenType.RIGHT_PAREN)) {
        // no increment
    } else {
        var bodyJump = emitJump(OpCode.Jump);
        var incrementStart = currentChunk().count();
        expression();
        emitByte(@enumToInt(OpCode.Pop)); // remove the increment expression's value
        consume(TokenType.RIGHT_PAREN, "Expect ')' after 'for' clauses.");

        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    // body of the for loop
    try statement();

    emitLoop(loopStart); // jump backwards to the loop condition

    if (hasExitJump) {
        // Patch the loop condition
        patchJump(exitJump);
        emitByte(@enumToInt(OpCode.Pop)); // remove the condition expression's value
    }

    endScope();
}

fn emitLoop(loopStart: usize) void {
    emitByte(@enumToInt(OpCode.Loop));

    var offset = currentChunk().count() - loopStart + 2;
    if (offset > U16_MAX) err("Loop body too large.");

    emitByte(@truncate(u8, (offset >> 8) & 0xff));
    emitByte(@truncate(u8, offset & 0xff));
}

fn expressionStatement() void {
    expression();
    consume(TokenType.SEMICOLON, "Expect ';' after expression.");
    emitByte(@enumToInt(OpCode.Pop));
}

fn expression() void {
    parsePrecedence(Precedence.Assignment);
}

fn literal(_: bool) void {
    switch (parser.previous.ttype) {
        .FALSE => {
            emitByte(@enumToInt(OpCode.False));
        },
        .TRUE => {
            emitByte(@enumToInt(OpCode.True));
        },
        .NIL => {
            emitByte(@enumToInt(OpCode.Nil));
        },
        else => {
            // unreachable
            return;
        },
    }
}

fn number(_: bool) void {
    // TODO: for now, falls back to 0 instead of erroring
    const value = std.fmt.parseFloat(f64, tokenString(parser.previous)) catch 0;
    emitConstant(Value{ .number = value });
}

fn grouping(_: bool) void {
    // we assume the initial '(' has already been consumed.
    expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
}

fn string(_: bool) void {
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

fn variable(canAssign: bool) void {
    // TODO: handle errors properly
    namedVariable(parser.previous, canAssign) catch {
        print("FIXME: handle errors properly for variable()", .{});
    };
}

fn namedVariable(token: Token, canAssign: bool) !void {
    var getOp: OpCode = undefined;
    var setOp: OpCode = undefined;
    var arg: u8 = 0;

    if (resolveLocal(current, token)) |localArg| {
        arg = @truncate(u8, localArg);
        getOp = OpCode.GetLocal;
        setOp = OpCode.SetLocal;
    } else if (resolveUpvalue(current, token)) |upvalueArg| {
        arg = @truncate(u8, upvalueArg);
        getOp = OpCode.GetUpvalue;
        setOp = OpCode.SetUpvalue;
    } else |_| { // ignore the error
        arg = try identifierConstant(token);
        getOp = OpCode.GetGlobal;
        setOp = OpCode.SetGlobal;
    }

    if (canAssign and match(TokenType.EQUAL)) {
        expression();
        emitBytes(@enumToInt(setOp), arg);
    } else {
        emitBytes(@enumToInt(getOp), arg);
    }
}

// TODO: Why would this fn take the compiler as an arg whereas it gets passed in as a global?
fn resolveLocal(compilerInstance: *Compiler, token: Token) compilerError!usize {
    var i: usize = compilerInstance.localCount;
    while (true) {
        const local = compilerInstance.locals[i];

        if (identifiersEqual(local.token, token)) {
            if (local.depth == -1) {
                err("Can't read local variable in its own initializer.");
            }
            return i;
        }

        if (i == 0) {
            break;
        }
        i -= 1;
    }

    return compilerError.LocalNotFound;
}

fn resolveUpvalue(compilerInstance: *Compiler, token: Token) compilerError!usize {
    if (compilerInstance.enclosing == null) {
        // we're in the global scope
        return compilerError.UpvalueNotFound;
    }

    if (resolveLocal(compilerInstance.enclosing, token)) |local| {
        return addUpvalue(compilerInstance, local, true);
    }

    return compilerError.LocalNotFound;
}

fn addUpvalue(compilerInstance: *Compiler, index: u8, isLocal: bool) usize {
    const upvalueCount = compilerInstance.function.upvalueCount;

    // check if the upvalue is already in the list
    var i = 0;
    while (i < upvalueCount) {
        const upvalue = compilerInstance.upvalues[i];
        if (upvalue.index == index and upvalue.isLocal == isLocal) {
            return i;
        }
        i += 1;
    }

    // if not, add a new one
    compilerInstance.upvalues[upvalueCount].isLocal = isLocal;
    compilerInstance.upvalues[upvalueCount].index = index;

    compilerInstance.function.upvalueCount += 1;
    return compilerInstance.function.upvalueCount;
}

fn identifiersEqual(a: Token, b: Token) bool {
    const aString = tokenString(a);
    const bString = tokenString(b);
    return std.mem.eql(u8, aString, bString);
}

fn unary(_: bool) void {
    const operatorType: TokenType = parser.previous.ttype;

    // Compile the operand.
    parsePrecedence(Precedence.Unary);

    // Emit the operator instruction.
    switch (operatorType) {
        .MINUS => {
            emitByte(@enumToInt(OpCode.Negate));
        },
        .BANG => {
            emitByte(@enumToInt(OpCode.Not));
        },
        else => {
            unreachable;
        },
    }
}

fn binary(_: bool) void {
    const operatorType: TokenType = parser.previous.ttype;
    const rule: *ParseRule = getRule(operatorType);
    parsePrecedence(@intToEnum(Precedence, (@enumToInt(rule.precedence) + 1)));

    switch (operatorType) {
        .PLUS => {
            emitByte(@enumToInt(OpCode.Add));
        },
        .MINUS => {
            emitByte(@enumToInt(OpCode.Subtract));
        },
        .STAR => {
            emitByte(@enumToInt(OpCode.Multiply));
        },
        .SLASH => {
            emitByte(@enumToInt(OpCode.Divide));
        },
        .BANG_EQUAL => {
            emitBytes(@enumToInt(OpCode.Equal), @enumToInt(OpCode.Not));
        },
        .EQUAL_EQUAL => {
            emitByte(@enumToInt(OpCode.Equal));
        },
        .GREATER_EQUAL => {
            emitBytes(@enumToInt(OpCode.Greater), @enumToInt(OpCode.Equal));
        },
        .GREATER => {
            emitByte(@enumToInt(OpCode.Greater));
        },
        .LESS_EQUAL => {
            emitBytes(@enumToInt(OpCode.Less), @enumToInt(OpCode.Equal));
        },
        .LESS => {
            emitByte(@enumToInt(OpCode.Less));
        },
        else => {
            // unreachable
            return;
        },
    }
}

fn call(_: bool) void {
    const argCount = argumentList();
    emitBytes(@enumToInt(OpCode.Call), argCount);
}

fn argumentList() u8 {
    var argCount: u8 = 0;
    if (!check(TokenType.RIGHT_PAREN)) {
        argCount = argumentListItem(argCount);
        while (match(TokenType.COMMA)) {
            argCount = argumentListItem(argCount);
        }
    }
    consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

fn argumentListItem(argCount: u8) u8 {
    expression();
    if (argCount == 255) {
        err("Can't have more than 255 arguments.");
    }
    return argCount + 1;
}

fn getRule(ttype: TokenType) *ParseRule {
    return &rules[@enumToInt(ttype)];
}

fn parsePrecedence(precedence: Precedence) void {
    advance();
    const rule = getRule(parser.previous.ttype);
    const prefixRule = rule.prefix;
    print("", .{}); // TODO: removing this line causes parsing errors. something gets compiled out?? ZIGGGGGG
    if (prefixRule == undefined) {
        err("Expect expression.");
        return;
    }

    const canAssign = @enumToInt(precedence) <= @enumToInt(Precedence.Assignment);
    prefixRule(canAssign);
    while (@enumToInt(precedence) <= @enumToInt(getRule(parser.current.ttype).precedence)) {
        advance();
        const infixRule = getRule(parser.previous.ttype).infix;
        infixRule(canAssign);
    }

    if (canAssign and match(TokenType.EQUAL)) {
        err("Invalid assignment target.");
    }
}

fn currentChunk() *Chunk {
    return current.function.chunk;
}

fn endCompiler() *ObjFunction {
    // TODO: if the fn has a return statement, this emits an extra return nil in the bytecode
    // ex...
    //   35    | OP_RETURN
    //   36    5 OP_NIL
    //   37    | OP_RETURN
    emitReturn();
    const func = current.function;
    if (debug.PRINT_CODE_AFTER_END_COMPILER) {
        if (!parser.hadError) {
            var name: []const u8 = "<script>";
            if (func.name) |fname| {
                name = fname.*.chars;
            }
            currentChunk().disassemble(name);
            print("\n", .{});
        }
    }

    // when a Compiler finishes, it pops itself off the stack by restoring the previous compiler
    current = current.enclosing;

    return func;
}

fn emitConstant(value: Value) void {
    emitBytes(@enumToInt(OpCode.Constant), makeConstant(value));
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
    emitByte(@enumToInt(OpCode.Nil)); // if no return is specified, default to nil
    emitByte(@enumToInt(OpCode.Return));
}

fn emitBytes(byte1: u8, byte2: u8) void {
    emitByte(byte1);
    emitByte(byte2);
}

fn consume(ttype: TokenType, message: []const u8) void {
    if (debug.TRACE_PARSER) print("consume({any}) parser.current = {any}\n", .{ ttype, parser.current });
    if (parser.current.ttype == ttype) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

fn advance() void {
    parser.previous = parser.current;

    while (true) {
        parser.current = scanner.scanToken();
        if (debug.TRACE_PARSER) print("advance() .previous = {any} .current = {any}\n", .{ parser.previous, parser.current });
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
        print(" at '{s}'", .{tokenString(token.*)});
    }

    print(": {s}\n", .{message});

    parser.hadError = true;
}

fn emitByte(byte: u8) void {
    currentChunk().write(byte, parser.previous.line) catch {};
}

// tokenString uses the source code's text to look up a string
fn tokenString(token: Token) []const u8 {
    return scanner.source[token.start .. token.start + token.length];
}
