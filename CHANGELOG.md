# Revision history for text-ascii

## 2.0.0 -- 

* New back-end based on `ByteArray#`.
* No longer depend on `text`, `bytestring`, `optics-extras`.
* Implement `decodeAscii`, `decodeBytesAscii` in `Unsafe`.
* Remove `fromText`, `fromByteString`, `toText`, `toByteString`.
* Implement `decodeAscii`, `decodeAsciiMay`, `decodeBytesAscii`,
  `decodeBytesAsciiMay`, `encodeAscii`, `toBytes` in `Text.Ascii`.
* Optimize string matching implementation, update documentation to indicate
  this.
* Add `copy` function in `Text.Ascii` for manually escaping memory sharing.
* Add `Arbitrary` instances for `AsciiChar`, `AsciiText`, `Unsafe`.
* Add `Hashable` instance for `AsciiText`.
* Add `Prim`, `Uniform`, `UniformRange` instances for `AsciiChar`, `Unsafe`.
* New benchmarks.
* Add new optics `_AsAscii`, `_AsBytesAscii`, `_PackedAscii` in `Text.Ascii`.
  These replace all old optics in that module, except `bytes` and `chars`.
* `chars` and `bytes` now have `Int` indices.
* The `char` quasiquoter can now be used in pattern contexts as well.

## 1.0.1 -- 2021-03-02

* Support GHC 9.
* Replace 8.10.3 with 8.10.4 in CI.
* Expose `Text.Ascii.Internal` and `Text.Ascii.QQ`.
* Add `Ixed` instance (and supporting type instances) for `AsciiText`.
* Add `Stream`, `VisualStream` and `TraversableStream` instances (and supporting
  type instances) for `AsciiText`.
* Drop Parsec in favour of Megaparsec.
* Add `FoldCase` instances for `AsciiChar` and `AsciiText`.
* Implement `lines`, `unlines`, `words`, `unwords`, `replicate`, `chunksOf`, 
  `index`, `zipWith`, `justifyLeft`, `justifyRight`, `center`, `takeEnd`, 
  `dropEnd`, `dropAround`, `strip`, `stripStart`, `stripEnd`, `commonPrefixes` 
  for `AsciiText`.
* Implement [NSN](https://www-igm.univ-mlv.fr/~lecroq/string/node13.html), as 
  well as the following functions that use it:
  * `count`
  * `replace`
  * `splitOn`
  * `stripInfix`
  * `breakOnAll`
  * `breakOn`
  * `breakOnEnd`
* Add `Unsafe` module containing an `Unsafe` wrapper, plus instances and
  functions.
* Add a range of optics for `AsciiText`.

## 1.0.0 -- 2021-02-07

* First version. Released on an unsuspecting world.
