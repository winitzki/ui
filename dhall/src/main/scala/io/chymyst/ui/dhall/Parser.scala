package io.chymyst.ui.dhall

import fastparse._
import NoWhitespace._

object Grammar {

  def shebang[$: P] = P(
    "#!" ~ not_end_of_line.rep ~ end_of_line
  )

  def complete_expression[$: P] = P(
    shebang.rep ~ whsp ~ expression ~ whsp ~ line_comment_prefix.rep(min = 0, max = 1)
  )

  def end_of_line[$: P] = P("\n" / "\r\n")

  def valid_non_ascii[$: P] = P(
    CharIn(
      "\u0080-\uD7FF",
      // %xD800_DFFF = surrogate pairs
      "\uE000-\uFFFD",
      // %xFFFE_FFFF = non_characters
      //        / % x10000_1FFFD
      //      // %x1FFFE_1FFFF = non_characters
      //      / % x20000_2FFFD
      //        // %x2FFFE_2FFFF = non_characters
      //        / % x30000_3FFFD
      //      // %x3FFFE_3FFFF = non_characters
      //      / % x40000_4FFFD
      //        // %x4FFFE_4FFFF = non_characters
      //        / % x50000_5FFFD
      //      // %x5FFFE_5FFFF = non_characters
      //      / % x60000_6FFFD
      //        // %x6FFFE_6FFFF = non_characters
      //        / % x70000_7FFFD
      //      // %x7FFFE_7FFFF = non_characters
      //      / % x80000_8FFFD
      //        // %x8FFFE_8FFFF = non_characters
      //        / % x90000_9FFFD
      //      // %x9FFFE_9FFFF = non_characters
      //      / % xA0000_AFFFD
      //        // %xAFFFE_AFFFF = non_characters
      //        / % xB0000_BFFFD
      //      // %xBFFFE_BFFFF = non_characters
      //      / % xC0000_CFFFD
      //        // %xCFFFE_CFFFF = non_characters
      //        / % xD0000_DFFFD
      //      // %xDFFFE_DFFFF = non_characters
      //      / % xE0000_EFFFD
      //        // %xEFFFE_EFFFF = non_characters
      //        / % xF0000_FFFFD
      //      // %xFFFFE_FFFFF = non_characters
      //      / % x100000_10FFFD
      // %x10FFFE_10FFFF = non_characters
    ))

  def tab[$: P] = P("\t")

  def block_comment[$: P] = P(
    "{-" ~ block_comment_continue)

  def block_comment_char[$: P] = P(
    CharIn("\u0020-\u007F")
      / valid_non_ascii
      / tab
      / end_of_line
  )

  def block_comment_continue[$: P]: P[Unit] = P(
    "-}"
      / (block_comment ~ block_comment_continue)
      / (block_comment_char ~ block_comment_continue)
  )

  def not_end_of_line[$: P] = P(
    CharIn("\u0020-\u007F") / valid_non_ascii / tab
  )

  def line_comment_prefix[$: P] = P(
    "--" ~ (not_end_of_line.rep)
  )

  def line_comment[$: P] = P(
    line_comment_prefix ~ end_of_line
  )

  def whitespace_chunk[$: P] = P(
    " "
      / tab
      / end_of_line
      / line_comment
      / block_comment
  )

  def whsp[$: P] = P(
    whitespace_chunk.rep
  )

  def whsp1[$: P] = P(
    whitespace_chunk.rep(exactly = 1)
  )

  def ALPHA[$: P] = P(CharIn("\u0041-\u005A", "\u0061-\u007A"))

  def DIGIT[$: P] = P(CharIn("0-9"))

  def ALPHANUM[$: P] = P(
    ALPHA / DIGIT
  )

  def HEXDIG[$: P] = P(
    CharIn("0-9A-F")
  )

  def simple_label_first_char[$: P] = P(
    ALPHA / "_"
  )

  def simple_label_next_char[$: P] = P(
    ALPHANUM / "-" / "/" / "_"
  )

  def simple_label[$: P] = P(
    simple_label_first_char.rep ~ simple_label_next_char
  )

  def quoted_label_char[$: P] = P(
    CharIn("\u0020-\u005F", "\u0061-\u007E")
    // %x60 = '`'
  )

  def quoted_label[$: P] = P(
    quoted_label_char.rep
  )

  def label[$: P] = P(
    ("`" ~ quoted_label ~ "`" / simple_label)
  )

  def nonreserved_label[$: P] = P(
    label
  )

  def any_label[$: P] = P(
    label
  )

  def any_label_or_some[$: P] = P(
    any_label / "Some"
  )

  def with_component[$: P] = P(
    any_label_or_some / "?"
  )

  def double_quote_chunk[$: P] = P(
    interpolation
      // '\'    Beginning of escape sequence
      / ("\\" ~ double_quote_escaped)
      / double_quote_char
  )

  def double_quote_escaped[$: P] = P(
    CharIn("\"$\\/bfnrt") // '"'    quotation mark  U+0022
      //      / % x24 // '$'    dollar sign     U+0024
      //        / % x5C // '\'    reverse solidus U+005C
      //      / % x2F // '/'    solidus         U+002F
      //        / % x62 // 'b'    backspace       U+0008
      //      / % x66 // 'f'    form feed       U+000C
      //        / % x6E // 'n'    line feed       U+000A
      //      / % x72 // 'r'    carriage return U+000D
      //        / % x74 // 't'    tab             U+0009
      / "u" ~ unicode_escape // 'uXXXX' / 'u{XXXX}'    U+XXXX
  )

  def unicode_escape[$: P] = P(
    unbraced_escape / ("{" ~ braced_escape ~ "}")
  )

  def unicode_suffix[$: P] = P(
    (CharIn("0-9A-E") ~ HEXDIG.rep(exactly = 3))
      / ("F" ~ HEXDIG.rep(exactly = 2) ~ CharIn("0-9A-D"))
  )

  def unbraced_escape[$: P] = P(
    ((DIGIT / "A" / "B" / "C") ~ HEXDIG.rep(exactly = 3))
      / ("D" ~ CharIn("0-7") ~ HEXDIG ~ HEXDIG)
      // %xD800_DFFF Surrogate pairs
      / ("E" ~ HEXDIG)
      / ("F" ~ HEXDIG.rep(exactly = 2) ~ CharIn("0-9A-D"))
    // %xFFFE_FFFF Non_characters
  )


  def braced_codepoint[$: P] = P(
    (CharIn("1-9A-F") / "10") ~ unicode_suffix
      //;
      //  (Planes
      //  1_16
      //  )
      / unbraced_escape // (Plane 0)
      / HEXDIG.rep(min = 1, max = 3) // %x000_FFF
  )

  def braced_escape[$: P] = P(
    "0".rep ~ braced_codepoint
  )

  def double_quote_char[$: P] = P(
    CharIn("\u0020-\u0021", "\u0023-\u005B", "\u005D-\u007F")
      //    %x20_21
      //      // %x22 = '"'
      //      / %x23_5B
      //        // %x5C = "\"
      //        / %x5D_7F
      / valid_non_ascii
  )

  def double_quote_literal[$: P] = P(
    "\"" ~ double_quote_chunk.rep ~ "\""
  )


  def single_quote_continue[$: P]: P[Unit] = P(
    interpolation ~ single_quote_continue
      / (escaped_quote_pair ~ single_quote_continue)
      / (escaped_interpolation ~ single_quote_continue)
      / "''" // End of text literal
      / (single_quote_char ~ single_quote_continue)
  )

  def escaped_quote_pair[$: P] = P(
    "'''"
  )

  def escaped_interpolation[$: P] = P(
    "''${"
  )

  def single_quote_char[$: P] = P(
    CharIn("\u0020-\u007F")
      / valid_non_ascii
      / tab
      / end_of_line
  )

  def single_quote_literal[$: P] = P(
    "''" ~ end_of_line ~ single_quote_continue
  )

  def interpolation[$: P] = P(
    "${" ~ complete_expression ~ "}"
  )
}


object Parser {
  /*
  Generate dhall.condensed.txt from dhall.abnf.txt by:

  grep -v  '^;' dhall/src/main/resources/dhall.abnf.txt |grep -v '^\s*$'|sed -e 's|^\([^ ]*\) = |\1 =\n    |'|sed -e 's/^\([^ ]*\) =/def \1[$: P] = P(/; s! ; ! // !; s|\([a-z0-9]\)-\([a-z0-9]\)|\1_\2|g;' > dhall/src/main/resources/dhall.condensed.txt
  */

}
