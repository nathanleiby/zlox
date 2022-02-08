- [ ] bug: missing `;` (e.g. in repl) results in segfault
  ```
  [line 1] Error: advance() error
  prefixRule TokenType.EOF: ParseRule{ .prefix = fn() void@aaaaaaaaaaaaaaaa, .infix = fn() void@aaaaaaaaaaaaaaaa, .precedence = Precedence.PREC_NONE }
  zsh: segmentation fault  zig run main.zig
  ```
- [ ] (someday) code profiling / speed
  - could profile some of the open-source impls too, for comparison
