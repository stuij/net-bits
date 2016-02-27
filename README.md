net-bits
========

Blatantly forked from dzamlo's [rust-bitfield](https://github.com/dzamlo/rust-bitfield)

Modified
--------

I've modified rust-bitfield a tiny bit to fit with [chucker](https://github.com/tstuij/chucker), my toy/pet network
project, which has some wonky requirements. It seems a bit silly to somehow
merge this with rust-bitfields, as I don't think the changes contribute
anything to the common good. The description below still stands:


Original text
------------

This project provides a procedural macro to generate bitfield-like struct.

The generated structs use an array of u8 for the data and provide methods to
get and set the values of the fields.

The generatated structs are not compatible with C bitfield. Unlike in C, the
position of each bytes and bits in the underling bytes array is specifed. The
bytes are in network order, and the bits are MSB first. No padding is added.

Because the generated struct is just a normal struct, you can add other 
methods to it or implement the trait you want.

Possible use includes decoding some binary file formats and reading the
headers of some network protocols or some low-level protocols.

Usage
-----

To use with [chucker](https://github.com/tstuij/chucker).

Don't look at the [examples folder](examples) for examples of the use the macro.
Those examples were meant for rust-bitfields and are terribly broken
at the moment.

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work by you, as defined in the Apache-2.0
license, shall be dual licensed as above, without any additional terms or
conditions.
