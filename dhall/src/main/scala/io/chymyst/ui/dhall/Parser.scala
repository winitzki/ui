package io.chymyst.ui.dhall

import fastparse._
import NoWhitespace._
import io.chymyst.ui.dhall.Syntax.{DhallFile, Expression}
import io.chymyst.ui.dhall.SyntaxConstants.VarName

object Grammar {

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
    whitespace_chunk.rep(1)
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

  def label[$: P]: P[String] = P(
    ("`" ~ quoted_label.! ~ "`" / simple_label.!)
  )

  def nonreserved_label[$: P] = P(
    label
  )

  def any_label[$: P] = P(
    label
  )

  def any_label_or_some[$: P] = P(
    any_label / requireKeyword("Some")
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

  def text_literal[$: P] = P(
    (double_quote_literal / single_quote_literal)
  )

  def bytes_literal[$: P] = P(
    "0x\"" ~ (HEXDIG.rep(exactly = 2)).rep ~ "\""
  )

  val simpleKeywords = Set(
    "if",
    "then",
    "else",
    "let",
    "in",
    "as",
    "using",
    "merge",
    "missing",
    "Infinity",
    "NaN",
    "Some",
    "toMap",
    "assert",
    "with",
    "forall",
    "showConstructor",
  )

  def forall_symbol[$: P] = P(
    "\u2200" // Unicode FOR ALL
  )

  def forall[$: P] = P(
    forall_symbol / requireKeyword("forall")
  )

  def keyword[$: P] = P(
    simpleKeywords.map(P(_)).reduce(_ / _)
  )

  val builtinSymbols = Set(
    "Natural/fold",
    "Natural/build",
    "Natural/isZero",
    "Natural/even",
    "Natural/odd",
    "Natural/toInteger",
    "Natural/show",
    "Integer/toDouble",
    "Integer/show",
    "Integer/negate",
    "Integer/clamp",
    "Natural/subtract",
    "Double/show",
    "List/build",
    "List/fold",
    "List/length",
    "List/head",
    "List/last",
    "List/indexed",
    "List/reverse",
    "Text/show",
    "Text/replace",
    "Date/show",
    "Time/show",
    "TimeZone/show",
    "Bool",
    "True",
    "False",
    "Optional",
    "None",
    "Natural",
    "Integer",
    "Double",
    "Text",
    "Date",
    "Time",
    "TimeZone",
    "List",
    "Type",
    "Kind",
    "Sort",
  )

  def builtin[$: P] = P(
    builtinSymbols.map(P(_)).reduce(_ / _)
  )

  def combine[$: P] = P(
    "\u2227" / "/\\"
  )

  def combine_types[$: P] = P(
    "\u2A53" / "//\\\\"
  )

  def equivalent[$: P] = P(
    "\u2261" / "==="
  )

  def prefer[$: P] = P(
    "\u2AFD" / "//"
  )

  def lambda[$: P] = P(
    "\u03BB" / "\\"
  )

  def arrow[$: P] = P(
    "\u2192" / "->"
  )

  def complete[$: P] = P(
    "::"
  )

  def exponent[$: P] = P(
    "e" ~ ("+" / "-").? ~ DIGIT.rep(1)
  )

  def numeric_double_literal[$: P] = P(
    // [ "+" / "-" ] 1*DIGIT ( "." 1*DIGIT [ exponent ] / exponent)
    ("+" / "-").? ~ DIGIT.rep(1) ~ ("." ~ DIGIT.rep(1) ~ exponent.? / exponent)
  )

  def minus_infinity_literal[$: P] = P(
    "-" ~ requireKeyword("Infinity")
  )

  def plus_infinity_literal[$: P] = P(
    requireKeyword("Infinity")
  )

  def double_literal[$: P] = P(
    // "-Infinity"
    minus_infinity_literal
      // "Infinity"
      / plus_infinity_literal
      // "NaN"
      / "NaN"
      // "2.0"
      / numeric_double_literal
  )

  def natural_literal[$: P] = P(
    // Hexadecimal with "0x" prefix
    "0x" ~ HEXDIG.rep(1)
      // Decimal; leading 0 digits are not allowed
      / CharIn("1-9") ~ (DIGIT.rep)
      // ... except for 0 itself
      / "0"
  )

  def integer_literal[$: P] = P(
    ("+" / "-") ~ natural_literal
  )

  def temporal_literal[$: P] = P(
    // "YYYY_MM_DDThh:mm:ss[+-]HH:MM", parsed as a `{ date : Date, time : Time, timeZone : TimeZone }`
    full_date ~ "T" ~ partial_time ~ time_offset
      // "YYYY_MM_DDThh:mm:ss", parsed as a `{ date : Date, time : Time }`
      / full_date ~ "T" ~ partial_time
      // "hh:mm:ss[+-]HH:MM", parsed as a `{ time : Time, timeZone, TimeZone }`
      / partial_time ~ time_offset
      // "YYYY_MM_DD", parsed as a `Date`
      / full_date
      // "hh:mm:ss", parsed as a `Time`
      / partial_time
      // "[+-]HH:MM", parsed as a `TimeZone`
      // Carefully note that this `time_numoffset` and not `time_offset`, meaning
      // that a standalone `Z` is not a valid Dhall literal for a `TimeZone`
      / time_numoffset
  )

  def date_fullyear[$: P] = P(
    DIGIT.rep(exactly = 4)
  )

  def date_month[$: P] = P(
    DIGIT.rep(exactly = 2) // 01_12
  )

  def date_mday[$: P] = P(
    DIGIT.rep(exactly = 2) // 01_28, 01_29, 01_30, 01_31 based on
    // month/year
  )

  def time_hour[$: P] = P(
    DIGIT.rep(exactly = 2) // 00_23
  )

  def time_minute[$: P] = P(
    DIGIT.rep(exactly = 2) // 00_59
  )

  def time_second[$: P] = P(
    DIGIT.rep(exactly = 2) // 00_59 (**UNLIKE** RFC 3339, we don't support leap seconds)
  )

  def time_secfrac[$: P] = P(
    "." ~ DIGIT.rep(1) // RFC 3339
  )

  def time_numoffset[$: P] = P(
    ("+" / "-") ~ time_hour ~ ":" ~ time_minute
  )

  def time_offset[$: P] = P(
    "Z" / time_numoffset // "Z" desugars to "+00:00"
  )

  def partial_time[$: P] = P(
    time_hour ~ ":" ~ time_minute ~ ":" ~ time_second
      ~ time_secfrac.?
  )

  def full_date[$: P] = P(
    date_fullyear ~ "-" ~ date_month ~ "-" ~ date_mday
  )

  def identifier[$: P] = P(
    variable / builtin
  )

  def variable[$: P] = P(
    nonreserved_label ~ (whsp ~ "@" ~ whsp ~ natural_literal).?
  )

  def path_character[$: P] = P( // Note: character 002D is the hyphen and needs to be escaped when used under CharIn().
    CharIn("\u0021\u0024-\u0027\u002A-\u002B\\-\u002E\u0030\u003B\u0040-\u005A\u005E-\u007A\u007C\u007E")
  )

  def quoted_path_character[$: P] = P(
    CharIn("\u0020\u0021", "\u0023-\u002E", "\u0030-\u007F")
      / valid_non_ascii
  )

  def unquoted_path_component[$: P] = P(
    path_character.rep(1)
  )

  def quoted_path_component[$: P] = P(
    quoted_path_character.rep(1)
  )

  def path_component[$: P] = P(
    "/" ~ (unquoted_path_component / ("\"" ~ quoted_path_component ~ "\""))
  )

  def path[$: P] = P(
    path_component.rep(1)
  )

  def local[$: P] = P(
    parent_path
      / here_path
      / home_path
      // NOTE: Backtrack if parsing this alternative fails.
      // This is because the first character of this alternative will be "/", but
      // if the second character is "/" or "\" then this should have been parsed
      // as an operator instead of a path
      / absolute_path
  )

  def parent_path[$: P] = P(
    ".." ~ path // Relative path
  )

  def here_path[$: P] = P(
    "." ~ path // Relative path
  )

  def home_path[$: P] = P(
    "~" ~ path // Home_anchored path
  )

  def absolute_path[$: P] = P(
    path // Absolute path
  )


  def scheme[$: P] = P(
    "http" ~ "s".?
  )

  def http_raw[$: P] = P(
    scheme ~ "://" ~ authority ~ path_abempty ~ ("?" ~ query).?
  )

  def path_abempty[$: P] = P(
    ("/" ~ segment).rep
  )

  def authority[$: P] = P(
    (userinfo ~ "@").? ~ host ~ (":" ~ port).?
  )

  def userinfo[$: P] = P(
    (unreserved / pct_encoded / sub_delims / ":").rep
  )

  def host[$: P] = P(
    IP_literal / IPv4address / domain
  )

  def port[$: P] = P(
    DIGIT.rep
  )

  def IP_literal[$: P] = P(
    "[" ~ (IPv6address / IPvFuture) ~ "]"
  )

  def IPvFuture[$: P] = P(
    "v" ~ HEXDIG.rep(1) ~ "." ~ (unreserved / sub_delims / ":").rep(1)
  )

  def IPv6address[$: P] = P(
    (h16 ~ ":").rep(exactly = 6) ~ ls32
      / ("::" ~ (h16 ~ ":").rep(exactly = 5) ~ ls32)
      / (h16.? ~ "::" ~ (h16 ~ ":").rep(exactly = 4) ~ ls32)
      / ((h16 ~ (":" ~ h16).rep(max = 1)).? ~ "::" ~ (h16 ~ ":").rep(exactly = 3) ~ ls32)
      / ((h16 ~ (":" ~ h16).rep(max = 2)).? ~ "::" ~ (h16 ~ ":").rep(exactly = 2) ~ ls32)
      / ((h16 ~ (":" ~ h16).rep(max = 3)).? ~ "::" ~ (h16 ~ ":").rep(exactly = 1) ~ ls32)
      / ((h16 ~ (":" ~ h16).rep(max = 4)).? ~ "::" ~ ls32)
      / ((h16 ~ (":" ~ h16).rep(max = 5)).? ~ "::" ~ h16)
      / ((h16 ~ (":" ~ h16).rep(max = 6)).? ~ "::")
  )
  /*

                                           6( h16 ":" ) ls32
              /                       "::" 5( h16 ":" ) ls32
              / [ h16               ] "::" 4( h16 ":" ) ls32
              / [ h16 *1( ":" h16 ) ] "::" 3( h16 ":" ) ls32
              / [ h16 *2( ":" h16 ) ] "::" 2( h16 ":" ) ls32
              / [ h16 *3( ":" h16 ) ] "::"    h16 ":"   ls32
              / [ h16 *4( ":" h16 ) ] "::"              ls32
              / [ h16 *5( ":" h16 ) ] "::"              h16
              / [ h16 *6( ":" h16 ) ] "::"
  )
   */

  def h16[$: P] = P(
    HEXDIG.rep(min = 1, max = 4)
  )

  def ls32[$: P] = P(
    h16 ~ ":" ~ h16 / IPv4address
  )

  def IPv4address[$: P] = P(
    dec_octet ~ "." ~ dec_octet ~ "." ~ dec_octet ~ "." ~ dec_octet
  )

  def dec_octet[$: P] = P(
    "25" ~ CharIn("0-5") //%x30_35       // 250_255
      / ("2" ~ CharIn("0-4") ~ DIGIT) // 200_249
      / ("1" ~ DIGIT.rep(exactly = 2)) // 100_199
      / (CharIn("1-9") ~ DIGIT) // 10_99
      / DIGIT // 0_9
  )

  def domain[$: P] = P(
    domainlabel ~ ("." ~ domainlabel).rep ~ ".".?
  )

  def domainlabel[$: P] = P(
    ALPHANUM.rep(1) ~ ("-".rep(1) ~ ALPHANUM.rep(1)).rep
  )

  def segment[$: P] = P(
    pchar.rep
  )

  def pchar[$: P] = P(
    unreserved / pct_encoded / sub_delims / ":" / "@"
  )

  def query[$: P] = P(
    (pchar / "/" / "?").rep
  )

  def pct_encoded[$: P] = P(
    "%" ~ HEXDIG ~ HEXDIG
  )

  def unreserved[$: P] = P(
    ALPHANUM / "-" / "." / "_" / "~"
  )

  def sub_delims[$: P] = P(
    "!" / "$" / "&" / "'" / "*" / "+" / ";" / "="
  )

  def http[$: P]: P[Unit] = P(
    http_raw ~ (whsp ~ requireKeyword("using") ~ whsp1 ~ import_expression).?
  )

  def env[$: P] = P(
    "env:" ~
      (bash_environment_variable
        / ("\u0022" ~ posix_environment_variable ~ "\u0022")
        )
  )

  def bash_environment_variable[$: P] = P(
    (ALPHA / "_") ~ (ALPHANUM / "_").rep
  )

  def posix_environment_variable[$: P] = P(
    posix_environment_variable_character.rep(1)
  )

  def posix_environment_variable_character[$: P] = P(
    "\\" ~ CharIn("\"\\abfnrtv")
      //    %x5C                 // '\'    Beginning of escape sequence
      //      ( %x22               // '"'    quotation mark  U+0022
      //        / %x5C               // '\'    reverse solidus U+005C
      //          / %x61               // 'a'    alert           U+0007
      //        / %x62               // 'b'    backspace       U+0008
      //          / %x66               // 'f'    form feed       U+000C
      //        / %x6E               // 'n'    line feed       U+000A
      //          / %x72               // 'r'    carriage return U+000D
      //        / %x74               // 't'    tab             U+0009
      //          / %x76               // 'v'    vertical tab    U+000B
      // Printable characters except double quote, backslash and equals
      / CharIn("\u0020-\u0021", "\u0023-\u003C", "\u003E-\u005B", "\u005D-\u007E")
    //  %x20_21
    //      // %x22 = '"'
    //      / %x23_3C
    //        // %x3D = '='
    //        / %x3E_5B
    //      // %x5C = "\"
    //      / %x5D_7E
  )

  def import_type[$: P] = P(
    requireKeyword("missing") / local / http / env
  )

  def hash[$: P] = P(
    "sha256:" ~ HEXDIG.rep(exactly = 64) // "sha256:XXX...XXX"
  )

  def import_hashed[$: P] = P(
    import_type(whsp1 ~ hash).?
  )

  def import_[$: P] = P(
    import_hashed ~ (whsp ~ requireKeyword("as") ~ whsp1 ~ (requireKeyword("Text") / requireKeyword("Location"))).?
  )

  def expression[$: P]: P[Expression] = P(
    //  "\(x : a) -> b"
    (lambda ~ whsp ~ "(" ~ whsp ~ nonreserved_label ~ whsp ~ ":" ~ whsp1 ~ expression ~ whsp ~ ")" ~ whsp ~ arrow ~ whsp ~ expression)
      .map { case (name, tipe, body) => Syntax.Expression.Lambda(VarName(name), tipe, body) }
      //
      //  "if a then b else c"
      / (requireKeyword("if") ~ whsp1 ~ expression ~ whsp ~ requireKeyword("then") ~ whsp1 ~ expression ~ whsp ~ requireKeyword("else") ~ whsp1 ~ expression)
      .map { case (cond, ifTrue, ifFalse) => Syntax.Expression.If(cond, ifTrue, ifFalse) }
      //
      //  "let x : t = e1 in e2"
      //  "let x     = e1 in e2"
      //  We allow dropping the `in` between adjacent let_expressions; the following are equivalent:
      //  "let x = e1 let y = e2 in e3"
      //  "let x = e1 in let y = e2 in e3"
      / (let_binding.rep(1) ~ requireKeyword("in") ~ whsp1 ~ expression)
      .map { case (x) => ??? }
      //
      //  "forall (x : a) -> b"
      / (requireKeyword("forall") ~ whsp ~ "(" ~ whsp ~ nonreserved_label ~ whsp ~ ":" ~ whsp1 ~ expression ~ whsp ~ ")" ~ whsp ~ arrow ~ whsp ~ expression)
      //
      //  "a -> b"
      //
      //  NOTE: Backtrack if parsing this alternative fails
      / (operator_expression ~ whsp ~ arrow ~ whsp ~ expression)
      //
      //  "a with x = b"
      //
      //  NOTE: Backtrack if parsing this alternative fails
      / with_expression
      //
      //  "merge e1 e2 : t"
      //
      //  NOTE: Backtrack if parsing this alternative fails since we can't tell
      //  from the keyword whether there will be a type annotation or not
      / (requireKeyword("merge") ~ whsp1 ~ import_expression ~ whsp1 ~ import_expression ~ whsp ~ ":" ~ whsp1 ~ expression)
      //
      //  "[] : t"
      //
      //  NOTE: Backtrack if parsing this alternative fails since we can't tell
      //  from the opening bracket whether or not this will be an empty list or
      //  a non_empty list
      / empty_list_literal
      //
      //  "toMap e : t"
      //
      //  NOTE: Backtrack if parsing this alternative fails since we can't tell
      //  from the keyword whether there will be a type annotation or not
      / (requireKeyword("toMap") ~ whsp1 ~ import_expression ~ whsp ~ ":" ~ whsp1 ~ expression)
      //
      //  "assert : Natural/even 1 === False"
      / (requireKeyword("assert") ~ whsp ~ ":" ~ whsp1 ~ expression)
      //
      //  "x : t"
      / annotated_expression
  )

  def annotated_expression[$: P] = P(
    operator_expression ~ (whsp ~ ":" ~ whsp1 ~ expression).?
  )

  def let_binding[$: P] = P(
    requireKeyword("let") ~ whsp1 ~ nonreserved_label ~ whsp ~ (":" ~ whsp1 ~ expression ~ whsp).? ~ "=" ~ whsp ~ expression ~ whsp1
  )

  def empty_list_literal[$: P] = P(
    "[" ~ whsp ~ ("," ~ whsp).? ~ "]" ~ whsp ~ ":" ~ whsp1 ~ expression
  )

  def with_expression[$: P] = P(
    import_expression ~ (whsp1 ~ "with" ~ whsp1 ~ with_clause).rep(1)
  )

  def with_clause[$: P] = P(
    with_component ~ (whsp ~ "." ~ whsp ~ with_component).rep ~ whsp ~ "=" ~ whsp ~ operator_expression
  )

  def operator_expression[$: P] = P(
    equivalent_expression
  )

  def equivalent_expression[$: P] = P(
    import_alt_expression ~ (whsp ~ equivalent ~ whsp ~ import_alt_expression).rep
  )

  def import_alt_expression[$: P] = P(
    or_expression ~ (whsp ~ "?" ~ whsp1 ~ or_expression).rep
  )

  def or_expression[$: P] = P(
    plus_expression ~ (whsp ~ "||" ~ whsp ~ plus_expression).rep
  )

  def plus_expression[$: P] = P(
    text_append_expression ~ (whsp ~ "+" ~ whsp1 ~ text_append_expression).rep
  )

  def text_append_expression[$: P] = P(
    list_append_expression ~ (whsp ~ "++" ~ whsp ~ list_append_expression).rep
  )

  def list_append_expression[$: P] = P(
    and_expression ~ (whsp ~ "#" ~ whsp ~ and_expression).rep
  )

  def and_expression[$: P] = P(
    combine_expression ~ (whsp ~ "&&" ~ whsp ~ combine_expression).rep
  )

  def combine_expression[$: P] = P(
    prefer_expression ~ (whsp ~ combine ~ whsp ~ prefer_expression).rep
  )

  def prefer_expression[$: P] = P(
    combine_types_expression ~ (whsp ~ prefer ~ whsp ~ combine_types_expression).rep
  )

  def combine_types_expression[$: P] = P(
    times_expression ~ (whsp ~ combine_types ~ whsp ~ times_expression).rep
  )

  def times_expression[$: P] = P(
    equal_expression ~ (whsp ~ "*" ~ whsp ~ equal_expression).rep
  )

  def equal_expression[$: P] = P(
    not_equal_expression ~ (whsp ~ "==" ~ whsp ~ not_equal_expression).rep
  )

  def not_equal_expression[$: P] = P(
    application_expression ~ (whsp ~ "!=" ~ whsp ~ application_expression).rep
  )

  def application_expression[$: P] = P(
    first_application_expression ~ (whsp1 ~ import_expression).rep
  )

  def first_application_expression[$: P] = P(
    //  "merge e1 e2"
    (requireKeyword("merge") ~ whsp1 ~ import_expression ~ whsp1 ~ import_expression)
      //
      //  "Some e"
      / requireKeyword("Some") ~ whsp1 ~ import_expression
      //
      //  "toMap e"
      / requireKeyword("toMap") ~ whsp1 ~ import_expression
      //
      //  "showConstructor e"
      / requireKeyword("showConstructor") ~ whsp1 ~ import_expression
      //
      / import_expression
  )

  def import_expression[$: P] = P(
    import_ / completion_expression
  )

  def completion_expression[$: P] = P(
    selector_expression ~ (whsp ~ complete ~ whsp ~ selector_expression).?
  )

  def selector_expression[$: P] = P(
    primitive_expression ~ (whsp ~ "." ~ whsp ~ selector).rep
  )

  def selector[$: P] = P(
    any_label / labels / type_selector
  )

  def labels[$: P] = P(
    "{" ~ whsp ~ ("," ~ whsp).rep ~ (any_label_or_some ~ whsp ~ ("," ~ whsp ~ any_label_or_some ~ whsp).rep ~ ("," ~ whsp).?).? ~ "}"
  )

  def type_selector[$: P] = P(
    "(" ~ whsp ~ expression ~ whsp ~ ")"
  )

  def primitive_expression[$: P] = P(
    temporal_literal
      //
      //  "2.0"
      / double_literal
      //
      //  "2"
      / natural_literal
      //
      //  "+2"
      / integer_literal
      //
      //  '"ABC"'
      / text_literal
      //
      //  '0x"01234567689abcdef"'
      / bytes_literal
      //
      //  "{ foo = 1      , bar = True }"
      //  "{ foo : Integer, bar : Bool }"
      / ("{" ~ whsp ~ ("," ~ whsp).? ~ record_type_or_literal ~ whsp ~ "}")
      //
      //  "< Foo : Integer | Bar : Bool >"
      //  "< Foo | Bar : Bool >"
      / ("<" ~ whsp ~ ("|" ~ whsp).? ~ union_type ~ whsp ~ ">")
      //
      //  "[1, 2, 3]"
      / non_empty_list_literal
      //
      //  "x"
      //  "x@2"
      / identifier
      //
      //  "( e )"
      / "(" ~ complete_expression ~ ")"
  )

  def record_type_or_literal[$: P] = P(
    empty_record_literal / non_empty_record_type_or_literal.?
  )

  def empty_record_literal[$: P] = P(
    "=" ~ (whsp ~ ",").?
  )

  def non_empty_record_type_or_literal[$: P] = P(
    (non_empty_record_type / non_empty_record_literal)
  )

  def non_empty_record_type[$: P] = P(
    record_type_entry ~ (whsp ~ "," ~ whsp ~ record_type_entry).rep ~ (whsp ~ ",").?
  )

  def record_type_entry[$: P] = P(
    any_label_or_some ~ whsp ~ ":" ~ whsp1 ~ expression
  )

  def non_empty_record_literal[$: P] = P(
    record_literal_entry ~ (whsp ~ "," ~ whsp ~ record_literal_entry).rep ~ (whsp ~ ",").?

  )

  def record_literal_entry[$: P] = P(
    any_label_or_some ~ record_literal_normal_entry.?
  )

  def record_literal_normal_entry[$: P] = P(
    (whsp ~ "." ~ whsp ~ any_label_or_some).rep ~ whsp ~ "=" ~ whsp ~ expression
  )

  def union_type[$: P] = P(
    (union_type_entry ~ (whsp ~ "|" ~ whsp ~ union_type_entry).rep ~ (whsp ~ "|").?).?
  )

  def union_type_entry[$: P] = P(
    any_label_or_some ~ (whsp ~ ":" ~ whsp1 ~ expression).?
  )

  def non_empty_list_literal[$: P] = P(
    "[" ~ whsp ~ ("," ~ whsp).? ~ expression ~ whsp ~ ("," ~ whsp ~ expression ~ whsp).rep ~ ("," ~ whsp).? ~ "]"
  )

  def shebang[$: P] = P(
    "#!" ~ not_end_of_line.rep.! ~ end_of_line
  )

  def complete_expression[$: P] = P(
    shebang.rep ~ whsp ~ expression ~ whsp ~ line_comment_prefix.?
  ).map { case (shebangContents, expr) => DhallFile(shebangContents, expr) }

  def requireKeyword[$: P](name: String) = {
    assert(simpleKeywords contains name, s"Keyword $name must be one of the supported Dhall keywords")
    P(name)
  }
}


object Parser {
  def parseFile(source: String): Parsed[DhallFile] = parse(source, Grammar.complete_expression(_))

}
