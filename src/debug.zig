// Debug Flags

// global flag to enable/disable all
const enable_debugging = true;

// VM (Runtime)
pub const TRACE_EXECUTION_INCLUDE_INSTRUCTIONS = enable_debugging and false;
pub const TRACE_EXECUTION_SHOW_GET_SET_VARS = enable_debugging and false;
pub const TRACE_EXECUTION_PRINT_STACK = enable_debugging and false;

// Compiler
pub const PRINT_CODE_AFTER_END_COMPILER = enable_debugging and false; // TODO: try out comptime
pub const TRACE_PARSER = enable_debugging and false;

// Object Manager (esp Garbage Collection)
pub const STRESS_GC = enable_debugging and true;
pub const LOG_GC = enable_debugging and true;
