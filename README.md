Decode and simulate x8086 instructions with in a debug UI. Supports a very [limited set of instructions](https://github.com/araex/sim8086/blob/main/src/x86/opcodes.zig). See `src/data` for example input.

Homework for https://computerenhance.com programming series.

## Prerequisites
- zig >= 0.14.0

To run the tests:
- [`nasm`](https://en.wikipedia.org/wiki/Netwide_Assembler) in PATH
- Write permissions in hardcoded directory [`out_dir`](https://github.com/araex/sim8086/blob/main/src/nasm.zig#L5)

## Usage
`zig build sim8086 -- src/data/listing_0045_challenge_register_movs` build and runs the simulator

`zig build test` builds and runs the tests.
