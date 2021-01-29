module Text.Ascii.Char
  ( -- * ASCII characters
    AsciiChar (AsByte, AsChar),
    fromChar,
    fromByte,
    upcase,
    downcase,

    -- * Categorization
    AsciiType (Control, Printable),
    charType,
    AsciiCategory (Other, Punctuation, Letter, Number, Symbol),
    categorize,
    categorizeGeneral,
    AsciiCase (Upper, Lower),
    caseOf,

    -- * Optics
    charWise,
    byteWise,
  )
where

import Text.Ascii.Char.Internal
