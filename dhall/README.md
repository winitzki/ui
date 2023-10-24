# Implementing Dhall in Scala

This is an implementation of the Dhall language following dhall-lang.org and the standard documentation.

Roadmap:

1. Parse from Dhall text into Dhall expression structures.
2. Serialize Dhall expressions into CBOR and deserialize back into Dhall.
3. Evaluate and normalize Dhall values according to Dhall semantics.
4. Import Dhall values from the network according to the Dhall security model.
5. Convert between Dhall values and Scala values (as much as possible given the Scala type system).
6. Create Scala-based Dhall values at compile time from Dhall files or from literal Dhall strings (compile-time constants).
7. Compile Dhall values into a library JAR. Publish the standard and taking JAR dependencies.
8. Extend Dhall on the Scala side (no changes to the Dhall standard) so that certain Dhall types or values are interpreted via custom Scala code.

## Parsing with `fastparse`

The ABNF grammar of Dhall is translated into rules of `fastparse`.

The "cut" is used sparingly as the `~/` operator, usually after a keyword or after a required whitespace.

However, in some cases adding this "cut" operator made the parsing results incorrect.

### Limitations

So far, there are some issues with the Unicode characters:

- If the input contains non-UTF8 sequences, the `fastparse` library appears to skip some of the input and create a valid UTF-8 string. However, the Dhall standard specifies that non-UTF8 input should be rejected by the parser.
- If the input contains Unicode characters greater than 65535 the `fastparse` library seems to truncate those characters.

There is also a failing test with `missing//blah` or `missingas text` and such. The keyword `missing` somehow conflicts with parsing.

## CBOR encoding

The CBOR encoding is implemented using the library `"com.upokecenter" % "cbor" % "4.5.2"` because it seems to be the only open-source CBOR library providing support for big integers out of the box.

### Limitations

The CBOR library does not preserve the precision of special `Double` values such as `0.0` and `-0.0`. The library converts those values to ordinary `Double` values equal to zero. This fails some of the Dhall acceptance tests.

