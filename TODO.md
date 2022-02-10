- [ ] Try out a zig linter https://github.com/nektro/ziglint
- [ ] Make Enum naming more consistent and succinct
  - e.g. `Precendence.PREC_NONE` => `Precedence.None` or `.none` (not sure what's the Zig convention)
- [ ] Refactor parser into a testable struct
- [ ] Figure out how to do imports without importing the tests
  - maybe via packages https://zig.news/mattnite/import-and-packages-23mb
- [ ] bug: missing `;` (e.g. in repl) results in segfault
  ```
  [line 1] Error: advance() error
  prefixRule TokenType.EOF: ParseRule{ .prefix = fn() void@aaaaaaaaaaaaaaaa, .infix = fn() void@aaaaaaaaaaaaaaaa, .precedence = Precedence.PREC_NONE }
  zsh: segmentation fault  zig run main.zig
  ```
- [ ] (someday) code profiling / speed
  - could profile some of the open-source impls too, for comparison
