- [ ] rename `free()` methods to `deinit()` to follow Zig conventions
  - it seems like the allocator uses the word `free` but other data structs (e.g. ArrayList) use `deinit`
- [ ] `<` works but `<=` doesn't seem to work
  - ... perhaps a bug due to popping numbers off the stack?
- [ ] Do memory allocator as described in the book
  - likely need a wrapper around use of allocator
- [ ] Allow easily setting debug flags at compilation time
  - currently, they are hard-coded to true/false in code
- [ ] Try out a zig linter https://github.com/nektro/ziglint
- [ ] Make Enum naming more consistent and succinct
  - not sure what's the Zig convention for enum naming. Shoutcase? Camelcase?
  - [x] e.g. `Precendence.PREC_NONE` => `Precedence.None`
  - [x] e.g. `OpCode.OpNil` => `OpCode.Nil`
- [ ] Refactor parser into a testable struct
- [ ] Figure out how to do imports without importing the tests
  - maybe via packages https://zig.news/mattnite/import-and-packages-23mb
- [ ] bug: missing `;` (e.g. in repl) results in segfault
  ```
  [line 1] Error: advance() error
  prefixRule TokenType.EOF: ParseRule{ .prefix = fn() void@aaaaaaaaaaaaaaaa, .infix = fn() void@aaaaaaaaaaaaaaaa, .precedence = Precedence.PREC_NONE }
  zsh: segmentation fault  zig run main.zig
  ```
- [ ] Explore how to get doc strings working (e.g. functions) and showing up in VSCode
  - `///` works but the comment shows up below a huge struct def, so not so useful.
- [ ] Use Zig error handling
  - one place: compiler errors like `consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")`. We already have fns like `err` and `errorAt`
- [ ] (someday) code profiling / speed
  - could profile some of the open-source impls too, for comparison
- [ ] repl bug
  ```
  > var a = 10; print a;
  thread 8126636 panic: index out of bounds
  ```
  vs
  ```
  > { var a = 5; print a; }
  5
  ```
- [ ] test colors aren't showing up in GithubActions
  - more context on what should be supported .. https://github.blog/2020-09-23-a-better-logs-experience-with-github-actions/#opening-the-door-to-a-more-colorful-experience
