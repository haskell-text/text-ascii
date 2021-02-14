# Revision history for text-ascii

## Unreleased

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

## 1.0.0 -- 2021-02-07

* First version. Released on an unsuspecting world.
