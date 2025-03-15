Homework for https://computerenhance.com programming series in zig.

## Prerequisites
- zig >= 0.14.0

To run the tests:
- [`nasm`](https://en.wikipedia.org/wiki/Netwide_Assembler)
- Write permissions in directory `out_dir`, hardcoded in `nasm.zig`

## Usage
`zig build main -- path/to/binary` build & runs the main executable with the given path. Decoded ASM is written to stdout.
`zig build test` builds & runs the tests.
