package io.chymyst.ui.dhall

import fastparse.NoWhitespace._
import fastparse._
import io.chymyst.ui.dhall.Syntax.Expression.{KeywordSome => ExpressionSome, _}
import io.chymyst.ui.dhall.Syntax.{DhallFile, Expression, PathComponent}
import io.chymyst.ui.dhall.SyntaxConstants.{ConstructorName, FieldName, ImportType, VarName}

import java.io.InputStream
import java.time.{LocalDate, LocalTime, ZoneOffset}
import scala.util.{Failure, Success, Try}

object Grammar {

  def end_of_line[$: P] = P("\n" | "\r\n")

  def valid_non_ascii[$: P] = P(
    CharIn(
      "\u0080-\uD7FF",
      // %xD800_DFFF = surrogate pairs
      "\uE000-\uFFFD",
    )
      | (CharIn("\uD800-\uD83E") ~ CharIn("\uDC00-\uDFFF"))
      | (CharIn("\uD83F") ~ CharIn("\uDC00-\uDFFD"))
    // TODO encode other Unicode ranges into Java's UTF-16
    // %xFFFE_FFFF = non_characters
    //        | % x10000_1FFFD
    // U+10000 = "\uD800\uDC00"
    // U+103FF = "\uD800\uDFFF"
    // U+10400 = "\uD801\uDC00"
    // U+1FFFD = "\uD83F\uDFFD"
    //      // %x1FFFE_1FFFF = non_characters
    //      | % x20000_2FFFD
    //        // %x2FFFE_2FFFF = non_characters
    //        | % x30000_3FFFD
    //      // %x3FFFE_3FFFF = non_characters
    //      | % x40000_4FFFD
    //        // %x4FFFE_4FFFF = non_characters
    //        | % x50000_5FFFD
    //      // %x5FFFE_5FFFF = non_characters
    //      | % x60000_6FFFD
    //        // %x6FFFE_6FFFF = non_characters
    //        | % x70000_7FFFD
    //      // %x7FFFE_7FFFF = non_characters
    //      | % x80000_8FFFD
    //        // %x8FFFE_8FFFF = non_characters
    //        | % x90000_9FFFD
    //      // %x9FFFE_9FFFF = non_characters
    //      | % xA0000_AFFFD
    //        // %xAFFFE_AFFFF = non_characters
    //        | % xB0000_BFFFD
    //      // %xBFFFE_BFFFF = non_characters
    //      | % xC0000_CFFFD
    //        // %xCFFFE_CFFFF = non_characters
    //        | % xD0000_DFFFD
    //      // %xDFFFE_DFFFF = non_characters
    //      | % xE0000_EFFFD
    //        // %xEFFFE_EFFFF = non_characters
    //        | % xF0000_FFFFD
    // U+F0000 = "\uDB80\uDC00"
    // U+FFFFD = "\uDBBFuDFFD"
    //      // %xFFFFE_FFFFF = non_characters
    //      | % x100000_10FFFD
    // U+100000 = "\uDBC0\uDC00"
    // U+10FFFD = "\uDBFF\uDFFD"
    // %x10FFFE_10FFFF = non_characters
  )

  def tab[$: P] = P("\t")

  def block_comment[$: P] = P(
    "{-" ~/ block_comment_continue
  )

  def block_comment_char[$: P] = P(
    CharIn("\u0020-\u007F")
      | valid_non_ascii
      | tab
      | end_of_line
  )

  def block_comment_continue[$: P]: P[Unit] = P(
    "-}"
      | (block_comment ~ block_comment_continue)
      | (block_comment_char ~ block_comment_continue)
  )

  def not_end_of_line[$: P] = P(
    CharIn("\u0020-\u007F") | valid_non_ascii | tab
  )

  def line_comment_prefix[$: P] = P(
    "--" ~/ (not_end_of_line.rep)
  )

  def line_comment[$: P] = P(
    line_comment_prefix ~ end_of_line
  )

  def whitespace_chunk[$: P] = P(
    " "
      | tab
      | end_of_line
      | line_comment
      | block_comment
  )

  def whsp[$: P]: P[Unit] = P(
    whitespace_chunk.rep
  )

  def whsp1[$: P]: P[Unit] = P(
    whitespace_chunk.rep(1)
  )

  def ALPHA[$: P] = P(CharIn("\u0041-\u005A", "\u0061-\u007A"))

  def DIGIT[$: P] = P(CharIn("0-9"))

  def ALPHANUM[$: P] = P(
    ALPHA | DIGIT
  )

  def HEXDIG[$: P] = P(
    CharIn("0-9A-F")
  )

  def simple_label_first_char[$: P] = P(
    ALPHA | "_"
  )

  def simple_label_next_char[$: P] = P(
    ALPHANUM | "-" | "/" | "_"
  )

  /*
  ; A simple label cannot be one of the reserved keywords
  ; listed in the `keyword` rule.
  ; A PEG parser could use negative lookahead to
  ; enforce this, e.g. as follows:
  ; simple-label =
  ;       keyword 1*simple-label-next-char
  ;     / !keyword (simple-label-first-char *simple-label-next-char)
   */
  def simple_label[$: P]: P[Unit] = P(
    (keyword.map(_ => ()) ~ simple_label_next_char.rep(1)) // Do not insert a cut after keyword.
      | (!keyword ~ simple_label_first_char ~ simple_label_next_char.rep)
  )

  def quoted_label_char[$: P] = P(
    CharIn("\u0020-\u005F", "\u0061-\u007E")
    // %x60 = '`'
  )

  def quoted_label[$: P] = P(
    quoted_label_char.rep
  )

  def label[$: P]: P[String] = P(
    ("`" ~ quoted_label.! ~ "`") | simple_label.!
  )

  def nonreserved_label[$: P] = P(
    label.map(VarName)
  )

  def any_label[$: P]: P[String] = P(
    label
  )

  def any_label_or_some[$: P]: P[String] = P(
    any_label | requireKeyword("Some").!
  )

  def with_component[$: P]: P[String] = P(
    any_label_or_some | "?".!
  ).!

  // Either a complete interpolated expression ${...} or a single character.
  def double_quote_chunk[$: P]: P[Either[Expression.TextLiteral, Expression.TextLiteralNoInterp]] = P( // text literal with or without interpolations
    interpolation.map(Expression.TextLiteral.ofExpression).map(Left.apply)
      // '\'    Beginning of escape sequence
      | ("\\" ~/ double_quote_escaped).map(Expression.TextLiteralNoInterp).map(Right.apply)
      | double_quote_char.!.map(Expression.TextLiteralNoInterp).map(Right.apply)
  )

  def double_quote_escaped[$: P]: P[String] = P(
    //    CharIn("\"$\\/bfnrt")
    CharIn("\u0022").! // '"'    quotation mark  U+0022
      | "$".!.map(_ => "\u0024") // '$'    dollar sign     U+0024
      | "\\".! //| % x5C // '\'    reverse solidus U+005C
      | "/".! // '/'    solidus         U+002F
      | "b".!.map(_ => "\b") // 'b'    backspace       U+0008
      | "f".!.map(_ => "\f") // 'f'    form feed       U+000C
      | "n".!.map(_ => "\n") // 'n'    line feed       U+000A
      | "r".!.map(_ => "\r") // 'r'    carriage return U+000D
      | "t".!.map(_ => "\t") // 't'    tab             U+0009
      | ("u" ~ unicode_escape.!.map(hex => Integer.parseInt(hex, 16).toChar.toString)) // 'uXXXX' | 'u{XXXX}'    U+XXXX
  )

  def unicode_escape[$: P] = P(
    unbraced_escape | ("{" ~ braced_escape ~ "}")
  )

  def unicode_suffix[$: P] = P(
    (CharIn("0-9A-E") ~ HEXDIG.rep(exactly = 3))
      | ("F" ~ HEXDIG.rep(exactly = 2) ~ CharIn("0-9A-D"))
  )

  def unbraced_escape[$: P] = P(
    ((DIGIT | "A" | "B" | "C") ~ HEXDIG.rep(exactly = 3))
      | ("D" ~ CharIn("0-7") ~ HEXDIG ~ HEXDIG)
      // %xD800_DFFF Surrogate pairs
      | ("E" ~ HEXDIG)
      | ("F" ~ HEXDIG.rep(exactly = 2) ~ CharIn("0-9A-D"))
    // %xFFFE_FFFF Non_characters
  )


  def braced_codepoint[$: P] = P(
    ((CharIn("1-9A-F") | "10") ~ unicode_suffix)
      //;
      //  (Planes
      //  1_16
      //  )
      | unbraced_escape // (Plane 0)
      | HEXDIG.rep(min = 1, max = 3) // %x000_FFF
  )

  def braced_escape[$: P] = P(
    "0".rep ~ braced_codepoint
  )

  def double_quote_char[$: P] = P(
    CharIn("\u0020-\u0021", "\u0023-\u005B", "\u005D-\u007F")
      //    %x20_21
      //      // %x22 = '"'
      //      | %x23_5B
      //        // %x5C = "\"
      //        | %x5D_7F
      | valid_non_ascii
  )

  def double_quote_literal[$: P]: P[TextLiteral] = P(
    "\"" ~/ double_quote_chunk.rep ~ "\""
  ).map(_.map(literalOrInterp => literalOrInterp.map(TextLiteral.ofText).merge).fold(TextLiteral.empty)(_ ++ _))

  def single_quote_continue[$: P]: P[Expression.TextLiteral] = P(
    (interpolation ~ single_quote_continue).map { case (head, tail) => Expression.TextLiteral.ofExpression(head) ++ tail }
      | (escaped_quote_pair ~ single_quote_continue)
      | (escaped_interpolation ~ single_quote_continue)
      | P("''").map(_ => Expression.TextLiteral.empty) // End of text literal
      | (single_quote_char ~ single_quote_continue).map { case (char, tail) => TextLiteral.ofText(TextLiteralNoInterp(char)) ++ tail }
  )

  def escaped_quote_pair[$: P] = P(
    "'''"
  )

  def escaped_interpolation[$: P] = P(
    "''${"
  )

  def single_quote_char[$: P] = P(
    CharIn("\u0020-\u007F")
      | valid_non_ascii
      | tab
      | end_of_line
  ).!

  def single_quote_literal[$: P] = P(
    "''" ~ end_of_line ~/ single_quote_continue
  )

  def interpolation[$: P]: P[Expression] = P(
    "${" ~ complete_expression ~/ "}"
  )

  def text_literal[$: P]: P[TextLiteral] = P(
    double_quote_literal
      | single_quote_literal
  )

  // See https://stackoverflow.com/questions/140131/convert-a-string-representation-of-a-hex-dump-to-a-byte-array-using-java
  def hexStringToByteArray(s: String): Array[Byte] = { // `s` must be a String of even length.
    val len = s.length
    val data = new Array[Byte](len >> 1)
    var i = 0
    while (i < len) {
      data(i >> 1) = ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i + 1), 16)).toByte
      i += 2
    }
    data
  }

  def bytes_literal[$: P]: P[Expression.BytesLiteral] = P(
    "0x\"" ~ HEXDIG.rep(exactly = 2).rep.! ~ "\""
  ).map(Expression.BytesLiteral)

  val simpleKeywords = Seq(
    "if",
    "then",
    "else",
    "let",
    "in",
    "assert",
    "as",
    "using",
    "merge",
    "missing",
    "Infinity",
    "NaN",
    "Some",
    "toMap",
    "with",
    "forall",
    "showConstructor",
    "Text",
    "Location",
    "Bytes",
  )

  val simpleKeywordsSet = simpleKeywords.toSet

  def opOr[$: P] = P("||")

  def opPlus[$: P] = P("+")

  def opTextAppend[$: P] = P("++")

  def opListAppend[$: P] = P("#")

  def opAnd[$: P] = P("&&")

  def opTimes[$: P] = P("*")

  def opEqual[$: P] = P("==")

  def opNotEqual[$: P] = P("!=")

  def opAlternative[$: P] = P("?")

  def forall_symbol[$: P] = P(
    "\u2200" // Unicode FOR ALL
  )

  def forall[$: P] = P(
    forall_symbol | requireKeyword("forall")
  )

  private def concatKeywords[$: P](keywords: Seq[String]): P[String] = {
    keywords
      .sorted(Ordering[String].reverse) // Reversing the order will disambiguate parsing of keywords that are substrings of another keyword.
      .map {
        k => implicit ctx: P[_] => P(k)
      }.reduce {
        (p1, p2) => implicit ctx: P[_] => P(p1(ctx) | p2(ctx))
      }(implicitly[P[$]]).!
  }

  def keyword[$: P]: P[String] = concatKeywords(simpleKeywords)

  def builtin[$: P]: P[Expression.Builtin] =
    concatKeywords(SyntaxConstants.Builtin.namesToValuesMap.keys.toSeq)
      .map(SyntaxConstants.Builtin.withName).map(Expression.Builtin)

  def combine[$: P] = P(
    "\u2227" | "/\\"
  )

  def combine_types[$: P] = P(
    "\u2A53" | "//\\\\"
  )

  def equivalent[$: P] = P(
    "\u2261" | "==="
  )

  def prefer[$: P] = P(
    "\u2AFD" | "//"
  )

  def lambda[$: P] = P(
    "\u03BB" | "\\"
  )

  def arrow[$: P] = P(
    "\u2192" | "->"
  )

  def complete[$: P] = P(
    "::"
  )

  def exponent[$: P] = P(
    "e" ~ ("+" | "-").? ~ DIGIT.rep(1)
  )

  def numeric_double_literal[$: P]: P[DoubleLiteral] = P(
    // [ "+" | "-" ] 1*DIGIT ( "." 1*DIGIT [ exponent ] | exponent)
    (("+" | "-").? ~ DIGIT.rep(1) ~ ("." ~ DIGIT.rep(1) ~ exponent.? | exponent)).!
  ).map(digits => DoubleLiteral(digits.toDouble))

  def minus_infinity_literal[$: P]: P[DoubleLiteral] = P(
    "-" ~ requireKeyword("Infinity")
  ).map(_ => DoubleLiteral(Double.NegativeInfinity))

  def plus_infinity_literal[$: P]: P[DoubleLiteral] = P(
    requireKeyword("Infinity")
  ).map(_ => DoubleLiteral(Double.PositiveInfinity))

  def double_literal[$: P]: P[DoubleLiteral] = P(
    // "-Infinity"
    minus_infinity_literal
      // "Infinity"
      | plus_infinity_literal
      // "NaN"
      | requireKeyword("NaN").map(_ => DoubleLiteral(Double.NaN))
      // "2.0"
      | numeric_double_literal
  )

  def natural_literal[$: P]: P[NaturalLiteral] = P(
    // Hexadecimal with "0x" prefix
    ("0x" ~ HEXDIG.rep(1).!).map(hexdigits => BigInt(hexdigits, 16))
      // Decimal; leading 0 digits are not allowed
      | (CharIn("1-9") ~ DIGIT.rep).!.map(digits => BigInt(digits, 10))
      // ... except for 0 itself
      | P("0").map(_ => BigInt(0))
  ).map(NaturalLiteral.apply)

  def integer_literal[$: P]: P[IntegerLiteral] = P(
    ("+" | "-").! ~ natural_literal
  ).map {
    case ("+", nat) => nat.value
    case ("-", nat) => -nat.value
  }.map(IntegerLiteral.apply)

  def temporal_literal[$: P]: P[Expression] = P(
    // "YYYY_MM_DDThh:mm:ss[+-]HH:MM", parsed as a `{ date : Date, time : Time, timeZone : TimeZone }`
    (full_date ~ "T" ~ partial_time ~ time_offset)
      .map { case (date, time, zone) => Parser.localDateTimeWithZone(date, time, zone) }
      // "YYYY_MM_DDThh:mm:ss", parsed as a `{ date : Date, time : Time }`
      | (full_date ~ "T" ~ partial_time)
      .map { case (date, time) => Parser.localDateTime(date, time) }
      // "hh:mm:ss[+-]HH:MM", parsed as a `{ time : Time, timeZone, TimeZone }`
      | (partial_time ~ time_offset)
      .map { case (time, zone) => Parser.localTimeWithZone(time, zone) }
      // "YYYY_MM_DD", parsed as a `Date`
      | full_date
      // "hh:mm:ss", parsed as a `Time`
      | partial_time
      // "[+-]HH:MM", parsed as a `TimeZone`
      // Carefully note that this `time_numoffset` and not `time_offset`, meaning
      // that a standalone `Z` is not a valid Dhall literal for a `TimeZone`
      | time_numoffset.map(TimeZoneLiteral)
  )

  def date_fullyear[$: P]: P[Int] = P(
    DIGIT.rep(exactly = 4)
  ).!.map(_.toInt)

  def date_month[$: P]: P[Int] = P(
    DIGIT.rep(exactly = 2)
    //("0" ~ CharIn("1-9")) | "1" ~ CharIn("0-2") // 01, 02, ..., 11, 12
  ).!.map(_.toInt)

  def date_mday[$: P]: P[Int] = P(
    DIGIT.rep(exactly = 2)
    //    ("0" ~ CharIn("1-9")) | (CharIn("12") ~ DIGIT) | ("3" ~ CharIn("01")) // 01_28, 01_29, 01_30, 01_31 based on
    // month/year
  ).!.map(_.toInt)

  def time_hour[$: P]: P[Int] = P(
    DIGIT.rep(exactly = 2) // 00_23
  ).!.map(_.toInt)

  def time_minute[$: P]: P[Int] = P(
    DIGIT.rep(exactly = 2) // 00_59
  ).!.map(_.toInt)

  def time_second[$: P]: P[Int] = P(
    DIGIT.rep(exactly = 2) // 00_59 (**UNLIKE** RFC 3339, we don't support leap seconds)
  ).!.map(_.toInt)

  def time_secfrac[$: P]: P[Int] = P( // Convert trailing fraction of a second, like .2345, into nanoseconds, like 234500000.
    "." ~ (DIGIT.!
      .rep(1) // RFC 3339
      .map(digits => (digits ++ Seq.fill(9)("0")).take(9).mkString("").toInt)
      )
  )

  def time_numoffset[$: P] = P(
    ("+" | "-").! ~ time_hour ~ ":" ~ time_minute
  ).map {
    case ("+", h, m) => ZoneOffset.ofHoursMinutes(h, m)
    case ("-", h, m) => ZoneOffset.ofHoursMinutes(-h, -m)
  }

  def time_offset[$: P]: P[ZoneOffset] = P(
    P("Z").map(_ => ZoneOffset.ofHours(0)) // "Z" desugars to "+00:00"
      | time_numoffset
  )

  def partial_time[$: P]: P[TimeLiteral] = P(
    time_hour ~ ":" ~ time_minute ~ ":" ~ time_second
      ~ time_secfrac.?
  ).flatMap { case (h, m, s, nanos) =>
    Try(TimeLiteral(LocalTime.of(h, m, s, nanos.getOrElse(0)))) match {
      case Failure(exception) => Fail(s"Invalid local time literal $h:$m:$s:$nanos - $exception")
      case Success(value) => Pass(value)
    }
  }

  def full_date[$: P]: P[DateLiteral] = P(
    date_fullyear ~ "-" ~ date_month ~ "-" ~ date_mday
  ).flatMap { case (y, m, d) =>
    Try(DateLiteral(LocalDate.of(y, m, d))) match {
      case Failure(exception) => Fail(s"Invalid date literal $y-$m-$d - $exception")
      case Success(value) => Pass(value)
    }
  }

  /*
  If the identifier matches one of the names in the `builtin` rule, then it is a
  builtin, and should be treated as the corresponding item in the list of
  "Reserved identifiers for builtins" specified in the `standard/README.md` document.
  It is a syntax error to specify a de Bruijn index in this case.
  Otherwise, this is a variable with name and index matching the label and index.

   */
  def identifier[$: P]: P[Expression] = P(
    variable.flatMap { case (name, index) =>
      SyntaxConstants.Builtin.namesToValuesMap.get(name.name) match {
        case None =>
          // Also, identifier may not equal a keyword!
          if (simpleKeywordsSet contains name.name) Fail(s"Identifier ${name.name} matches a keyword name, which is not acceptable.")
          else Pass(Expression.Variable(name, index.map(_.value).getOrElse(BigInt(0))))
        case Some(builtinName) =>
          if (index contains NaturalLiteral(BigInt(0)))
            Pass(Expression.Builtin(builtinName))
          else Fail(s"Identifier ${name.name} matches a builtin name but has invalid de Bruijn index $index")
      }
    }
      | builtin
  )

  def variable[$: P] = P(
    nonreserved_label ~ (whsp ~ "@" ~ whsp ~ natural_literal).?
  )

  def path_character[$: P] = P( // Note: character 002D is the hyphen and needs to be escaped when used under CharIn().
    CharIn("\u0021\u0024-\u0027\u002A-\u002B\\-\u002E\u0030\u003B\u0040-\u005A\u005E-\u007A\u007C\u007E")
  )

  def quoted_path_character[$: P] = P(
    CharIn("\u0020\u0021", "\u0023-\u002E", "\u0030-\u007F")
      | valid_non_ascii
  )

  def unquoted_path_component[$: P] = P(
    path_character.rep(1)
  )

  def quoted_path_component[$: P] = P(
    quoted_path_character.rep(1)
  )

  def path_component[$: P] = P(
    "/" ~ (unquoted_path_component | ("\"" ~ quoted_path_component ~ "\"")).!
  )

  def path[$: P] = P(
    path_component.rep(1)
  )

  def local[$: P] = P(
    parent_path
      | here_path
      | home_path
      // NOTE: Backtrack if parsing this alternative fails.
      // This is because the first character of this alternative will be "/", but
      // if the second character is "/" or "\" then this should have been parsed
      // as an operator instead of a path
      | absolute_path
  )

  def parent_path[$: P] = P(
    ".." ~ path // Relative path
  ).map(segments => ImportType.Path(SyntaxConstants.FilePrefix.Parent, SyntaxConstants.File(segments)))

  def here_path[$: P] = P(
    "." ~ path // Relative path
  ).map(segments => ImportType.Path(SyntaxConstants.FilePrefix.Here, SyntaxConstants.File(segments)))

  def home_path[$: P] = P(
    "~" ~ path // Home_anchored path
  ).map(segments => ImportType.Path(SyntaxConstants.FilePrefix.Home, SyntaxConstants.File(segments)))

  def absolute_path[$: P] = P(
    path // Absolute path
  ).map(segments => ImportType.Path(SyntaxConstants.FilePrefix.Absolute, SyntaxConstants.File(segments)))


  def scheme[$: P]: P[SyntaxConstants.Scheme] = P(
    "http" ~ "s".?
  ).!.map(s => SyntaxConstants.Scheme.withNameInsensitive(s))

  def http_raw[$: P]: P[SyntaxConstants.URL] = P(
    scheme ~ "://" ~ authority.! ~ path_abempty ~ ("?" ~ query.!).?
  ).map { case (s, a, p, q) => SyntaxConstants.URL(s, a, p, q) }

  def path_abempty[$: P]: P[SyntaxConstants.File] = P(
    ("/" ~ segment.!).rep
  ).map { segments => SyntaxConstants.File(segments) }

  def authority[$: P] = P(
    (userinfo ~ "@").? ~ host ~ (":" ~ port).?
  )

  def userinfo[$: P] = P(
    (unreserved | pct_encoded | sub_delims | ":").rep
  )

  def host[$: P] = P(
    IP_literal | IPv4address | domain
  )

  def port[$: P] = P(
    DIGIT.rep
  )

  def IP_literal[$: P] = P(
    "[" ~ (IPv6address | IPvFuture) ~ "]"
  )

  def IPvFuture[$: P] = P(
    "v" ~ HEXDIG.rep(1) ~ "." ~ (unreserved | sub_delims | ":").rep(1)
  )

  def IPv6address[$: P] = P(
    ((h16 ~ ":").rep(exactly = 6) ~ ls32)
      | ("::" ~ (h16 ~ ":").rep(exactly = 5) ~ ls32)
      | (h16.? ~ "::" ~ (h16 ~ ":").rep(exactly = 4) ~ ls32)
      | ((h16 ~ (":" ~ h16).rep(max = 1)).? ~ "::" ~ (h16 ~ ":").rep(exactly = 3) ~ ls32)
      | ((h16 ~ (":" ~ h16).rep(max = 2)).? ~ "::" ~ (h16 ~ ":").rep(exactly = 2) ~ ls32)
      | ((h16 ~ (":" ~ h16).rep(max = 3)).? ~ "::" ~ (h16 ~ ":").rep(exactly = 1) ~ ls32)
      | ((h16 ~ (":" ~ h16).rep(max = 4)).? ~ "::" ~ ls32)
      | ((h16 ~ (":" ~ h16).rep(max = 5)).? ~ "::" ~ h16)
      | ((h16 ~ (":" ~ h16).rep(max = 6)).? ~ "::")
  )
  /*

                                           6( h16 ":" ) ls32
              |                       "::" 5( h16 ":" ) ls32
              | [ h16               ] "::" 4( h16 ":" ) ls32
              | [ h16 *1( ":" h16 ) ] "::" 3( h16 ":" ) ls32
              | [ h16 *2( ":" h16 ) ] "::" 2( h16 ":" ) ls32
              | [ h16 *3( ":" h16 ) ] "::"    h16 ":"   ls32
              | [ h16 *4( ":" h16 ) ] "::"              ls32
              | [ h16 *5( ":" h16 ) ] "::"              h16
              | [ h16 *6( ":" h16 ) ] "::"
  )
   */

  def h16[$: P] = P(
    HEXDIG.rep(min = 1, max = 4)
  )

  def ls32[$: P] = P(
    (h16 ~ ":" ~ h16) | IPv4address
  )

  def IPv4address[$: P] = P(
    dec_octet ~ "." ~ dec_octet ~ "." ~ dec_octet ~ "." ~ dec_octet
  )

  def dec_octet[$: P] = P(
    ("25" ~ CharIn("0-5")) //%x30_35       // 250_255
      | ("2" ~ CharIn("0-4") ~ DIGIT) // 200_249
      | ("1" ~ DIGIT.rep(exactly = 2)) // 100_199
      | (CharIn("1-9") ~ DIGIT) // 10_99
      | DIGIT // 0_9
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
    unreserved | pct_encoded | sub_delims | ":" | "@"
  )

  def query[$: P] = P(
    (pchar | "/" | "?").rep
  )

  def pct_encoded[$: P] = P(
    "%" ~ HEXDIG ~ HEXDIG
  )

  def unreserved[$: P] = P(
    ALPHANUM | "-" | "." | "_" | "~"
  )

  def sub_delims[$: P] = P(
    "!" | "$" | "&" | "'" | "*" | "+" | ";" | "="
  )

  val emptyHeaders: Expression = Expression.EmptyList(Expression.RecordType(Seq(
    (FieldName("mapKey"), Expression.Builtin(SyntaxConstants.Builtin.Text)),
    (FieldName("mapValue"), Expression.Builtin(SyntaxConstants.Builtin.Text)),
  )))

  def http[$: P]: P[ImportType.Remote] = P(
    http_raw ~ (whsp ~ requireKeyword("using") ~ whsp1 ~/ import_expression).?
  ).map { case (url, headers) => ImportType.Remote(url, headers.getOrElse(emptyHeaders)) }

  def env[$: P]: P[ImportType.Env] = P(
    "env:" ~/
      (bash_environment_variable.!
        | ("\u0022" ~ posix_environment_variable.! ~ "\u0022")
        )
  ).map(name => ImportType.Env(name))

  def bash_environment_variable[$: P] = P(
    (ALPHA | "_") ~ (ALPHANUM | "_").rep
  )

  def posix_environment_variable[$: P] = P(
    posix_environment_variable_character.rep(1)
  )

  def posix_environment_variable_character[$: P] = P(
    ("\\" ~ CharIn("\"\\abfnrtv"))
      //    %x5C                 // '\'    Beginning of escape sequence
      //      ( %x22               // '"'    quotation mark  U+0022
      //        | %x5C               // '\'    reverse solidus U+005C
      //          | %x61               // 'a'    alert           U+0007
      //        | %x62               // 'b'    backspace       U+0008
      //          | %x66               // 'f'    form feed       U+000C
      //        | %x6E               // 'n'    line feed       U+000A
      //          | %x72               // 'r'    carriage return U+000D
      //        | %x74               // 't'    tab             U+0009
      //          | %x76               // 'v'    vertical tab    U+000B
      // Printable characters except double quote, backslash and equals
      | CharIn("\u0020-\u0021", "\u0023-\u003C", "\u003E-\u005B", "\u005D-\u007E")
    //  %x20_21
    //      // %x22 = '"'
    //      | %x23_3C
    //        // %x3D = '='
    //        | %x3E_5B
    //      // %x5C = "\"
    //      | %x5D_7E
  )

  def import_type[$: P]: P[ImportType] = P(
    requireKeyword("missing").map(_ => ImportType.Missing)
      | local
      | http
      | env
  )

  def hash[$: P] = P(
    "sha256:" ~/ HEXDIG.rep(exactly = 64).! // "sha256:XXX...XXX"
  )

  def import_hashed[$: P]: P[(ImportType, Option[String])] = P(
    import_type ~ (whsp1 ~ hash).?
  )

  def import_only[$: P]: P[Import] = P(
    import_hashed ~ (whsp ~ requireKeyword("as") ~ whsp1 ~/ (requireKeyword("Text") | requireKeyword("Location") | requireKeyword("Bytes")).!).?
  ).map { case (importType, digest, mode) =>
    val importMode = mode match {
      case Some("Bytes") => SyntaxConstants.ImportMode.RawBytes
      case Some("Location") => SyntaxConstants.ImportMode.Location
      case Some("Text") => SyntaxConstants.ImportMode.RawText
      case None => SyntaxConstants.ImportMode.Code
    }
    Import(importType, importMode, digest.map(BytesLiteral))
  }

  // The ABNF spec does not define those sub-rules. They are created only to help with debugging.

  def expression_lambda[$: P]: P[Lambda] = P(lambda ~ whsp ~/ "(" ~ whsp ~/ nonreserved_label ~ whsp ~ ":" ~ whsp1 ~/ expression ~ whsp ~ ")" ~ whsp ~ arrow ~/
    whsp ~ expression)
    .map { case (name, tipe, body) => Lambda(name, tipe, body) }

  def expression_if_then_else[$: P]: P[If] = P(
    requireKeyword("if") ~ whsp1 ~/ expression ~ whsp ~ requireKeyword("then") ~ whsp1 ~/ expression ~ whsp ~ requireKeyword("else") ~ whsp1 ~/ expression
  ).map { case (cond, ifTrue, ifFalse) =>
    If(cond, ifTrue, ifFalse)
  }

  def expression_let_binding[$: P]: P[Expression] = P(let_binding.rep(1) ~ requireKeyword("in") ~ whsp1 ~/ expression)
    .map { case (letBindings, expr) =>
      letBindings.foldRight(expr) { case ((varName, tipe, body), prev) => Let(varName, tipe, body, prev) }
    }

  def expression_forall[$: P]: P[Forall] = P(forall ~ whsp ~/ "(" ~ whsp ~ nonreserved_label ~ whsp ~/ ":" ~ whsp1 ~/ expression ~ whsp ~ ")" ~ whsp ~ arrow ~/
    whsp ~ expression)
    .map { case (varName, tipe, body) => Forall(varName, tipe, body) }

  // (`A → B` is short-hand for `∀(_ : A) → B`)
  def expression_arrow[$: P]: P[Expression] = P(operator_expression ~ whsp ~ arrow ~/ whsp ~ expression)
    .map { case (head, body) => Expression.Forall(VarName("_"), head, body) }

  def expression_merge[$: P]: P[Merge] = P(requireKeyword("merge") ~ whsp1 ~/ import_expression ~ whsp1 ~/ import_expression ~ whsp ~/ ":" ~ whsp1 ~/
    expression)
    .map { case (e1, e2, t) => Merge(e1, e2, Some(t)) }

  def expression_toMap[$: P]: P[ToMap] = P(requireKeyword("toMap") ~ whsp1 ~/ import_expression ~/ whsp ~ ":" ~ whsp1 ~/ expression)
    .map { case (e1, e2) => ToMap(e1, Some(e2)) }

  def expression_assert[$: P]: P[Assert] = P(requireKeyword("assert") ~ whsp ~/ ":" ~ whsp1 ~/ expression)
    .map { expr => Assert(expr) }

  def expression[$: P]: P[Expression] = P( // TODO: remove some of these NoCut() because they are probably not needed.
    //  "\(x : a) -> b"
    NoCut(expression_lambda)
      //
      //  "if a then b else c"
      | NoCut(expression_if_then_else)
      //
      //  "let x : t = e1 in e2"
      //  "let x     = e1 in e2"
      //  We allow dropping the `in` between adjacent let_expressions; the following are equivalent:
      //  "let x = e1 let y = e2 in e3"
      //  "let x = e1 in let y = e2 in e3"
      | NoCut(expression_let_binding)
      //
      //  "forall (x : a) -> b"
      | NoCut(expression_forall)
      //
      //  "a -> b"
      //
      //  NOTE: Backtrack if parsing this alternative fails
      | NoCut(expression_arrow)
      //
      //  "a with x = b"
      //
      //  NOTE: Backtrack if parsing this alternative fails
      | NoCut(with_expression)
      //
      //  "merge e1 e2 : t"
      //
      //  NOTE: Backtrack if parsing this alternative fails since we can't tell
      //  from the keyword whether there will be a type annotation or not
      | NoCut(expression_merge)
      //
      //  "[] : t"
      //
      //  NOTE: Backtrack if parsing this alternative fails since we can't tell
      //  from the opening bracket whether or not this will be an empty list or
      //  a non_empty list
      | NoCut(empty_list_literal)
      //
      //  "toMap e : t"
      //
      //  NOTE: Backtrack if parsing this alternative fails since we can't tell
      //  from the keyword whether there will be a type annotation or not
      | NoCut(expression_toMap)
      //
      //  "assert : Natural/even 1 === False"
      | NoCut(expression_assert)
      //
      //  "x : t"
      | NoCut(annotated_expression)
  )

  def annotated_expression[$: P]: P[Expression] = P(
    operator_expression ~ (whsp ~ ":" ~ whsp1 ~/ expression).?
  ).map { case (expr, tipe) =>
    tipe match {
      case Some(t) => Annotation(expr, t)
      case None => expr
    }
  }

  def let_binding[$: P] = P(
    requireKeyword("let") ~ whsp1 ~/ nonreserved_label ~ whsp ~ (":" ~ whsp1 ~/ expression ~ whsp).? ~ "=" ~ whsp ~/ expression ~ whsp1./
  )

  def empty_list_literal[$: P] = P(
    "[" ~ whsp ~ ("," ~ whsp).? ~ "]" ~ whsp ~/ ":" ~ whsp1 ~/ expression
  ).map(EmptyList)

  def with_expression[$: P] = P(
    import_expression ~ (whsp1 ~ "with" ~ whsp1 ~/ with_clause).rep(1)
    // record with x1.y1.z1 = expr1 with x2.y2.z2 = expr2   should be represented by With(  With(record, Seq(x1, y1, z1), expr1), Seq(x2, y2, z2), expr2)
  ).map { case (expr, substs) =>
    substs.foldLeft(expr) { case (prev, (varName, fields, target)) => With(prev, PathComponent.Label(FieldName(varName.name)) +: fields.map(PathComponent
      .Label), target)
    }
  }

  def with_clause[$: P] = P(
    with_component.map(VarName) ~ (whsp ~ "." ~ whsp ~/ with_component.map(FieldName)).rep ~ whsp ~ "=" ~ whsp ~/ operator_expression
  )

  def operator_expression[$: P]: P[Expression] = P(
    equivalent_expression
  )

  private implicit class FoldOpExpression(resultWithExpressionSequence: P[(Expression, Seq[Expression])]) {
    def withOperator(op: SyntaxConstants.Operator): P[Expression] =
      resultWithExpressionSequence.map { case (head, tail) => tail.foldLeft(head)((prev, arg) => Operator(prev, op, arg)) }
  }

  def equivalent_expression[$: P]: P[Expression] = P(
    import_alt_expression ~ (whsp ~ equivalent ~ whsp ~/ import_alt_expression).rep
  ).withOperator(SyntaxConstants.Operator.Equivalent)

  def import_alt_expression[$: P]: P[Expression] = P(
    or_expression ~ (whsp ~ opAlternative ~ whsp1 ~/ or_expression).rep
  ).withOperator(SyntaxConstants.Operator.Alternative)

  def or_expression[$: P]: P[Expression] = P(
    plus_expression ~ (whsp ~ opOr ~ whsp ~/ plus_expression).rep
  ).withOperator(SyntaxConstants.Operator.Or)

  def plus_expression[$: P]: P[Expression] = P(
    text_append_expression ~ (whsp ~ opPlus ~ whsp1 ~/ text_append_expression).rep
  ).withOperator(SyntaxConstants.Operator.Plus)

  def text_append_expression[$: P]: P[Expression] = P(
    list_append_expression ~ (whsp ~ opTextAppend ~ whsp ~/ list_append_expression).rep
  ).withOperator(SyntaxConstants.Operator.TextAppend)

  def list_append_expression[$: P]: P[Expression] = P(
    and_expression ~ (whsp ~ opListAppend ~ whsp ~/ and_expression).rep
  ).withOperator(SyntaxConstants.Operator.ListAppend)

  def and_expression[$: P]: P[Expression] = P(
    combine_expression ~ (whsp ~ opAnd ~ whsp ~/ combine_expression).rep
  ).withOperator(SyntaxConstants.Operator.And)

  def combine_expression[$: P]: P[Expression] = P(
    prefer_expression ~ (whsp ~ combine ~ whsp ~/ prefer_expression).rep
  ).withOperator(SyntaxConstants.Operator.CombineRecordTerms)

  def prefer_expression[$: P]: P[Expression] = P(
    combine_types_expression ~ (whsp ~ prefer ~ whsp ~/ combine_types_expression).rep
  ).withOperator(SyntaxConstants.Operator.Prefer)

  def combine_types_expression[$: P]: P[Expression] = P(
    times_expression ~ (whsp ~ combine_types ~ whsp ~/ times_expression).rep
  ).withOperator(SyntaxConstants.Operator.CombineRecordTypes)

  def times_expression[$: P]: P[Expression] = P(
    equal_expression ~ (whsp ~ opTimes ~ whsp ~/ equal_expression).rep
  ).withOperator(SyntaxConstants.Operator.Times)

  def equal_expression[$: P]: P[Expression] = P(
    not_equal_expression ~ (whsp ~ opEqual ~ whsp ~ not_equal_expression).rep // Should not cut because == can be confused with ===
  ).withOperator(SyntaxConstants.Operator.Equal)

  def not_equal_expression[$: P]: P[Expression] = P(
    application_expression ~ (whsp ~ opNotEqual ~ whsp ~/ application_expression).rep
  ).withOperator(SyntaxConstants.Operator.NotEqual)

  def application_expression[$: P]: P[Expression] = P(
    first_application_expression ~ (whsp1 ~ import_expression).rep
  ).map { case (head, tail) => tail.foldLeft(head)((prev, expr) => Application(prev, expr)) }

  def first_application_expression[$: P]: P[Expression] = P(
    //  "merge e1 e2"
    (requireKeyword("merge") ~ whsp1 ~/ import_expression ~ whsp1 ~/ import_expression)
      .map { case (e1, e2) => Merge(e1, e2, None) }
      //
      //  "Some e"
      | (requireKeyword("Some") ~ whsp1 ~/ import_expression)
      .map(expr => ExpressionSome(expr))
      //
      //  "toMap e"
      | (requireKeyword("toMap") ~ whsp1 ~/ import_expression)
      .map(expr => ToMap(expr, None))
      //
      //  "showConstructor e"
      | (requireKeyword("showConstructor") ~ whsp1 ~/ import_expression)
      .map(expr => ShowConstructor(expr))
      //
      | import_expression
  )

  def import_expression[$: P]: P[Expression] = P(
    import_only | completion_expression
  )

  def completion_expression[$: P]: P[Expression] = P(
    selector_expression ~ (whsp ~ complete ~ whsp ~ selector_expression).?
  ).map {
    case (expr, None) => expr
    case (expr, Some(tipe)) => Expression.Completion(expr, tipe)
  }

  def selector_expression[$: P]: P[Expression] = P(
    primitive_expression ~ (whsp ~ "." ~ whsp ~/ selector).rep
  ).map { case (base, selectors) => selectors.foldLeft(base)((prev, selector) => selector.chooseExpression(prev)) }

  sealed trait ExpressionSelector {
    def chooseExpression(base: Expression): Expression = this match {
      case ExpressionSelector.ByField(fieldName) => Expression.Field(base, fieldName)
      case ExpressionSelector.ByLabels(fieldNames) => Expression.ProjectByLabels(base, fieldNames)
      case ExpressionSelector.ByType(typeExpr) => Expression.ProjectByType(base, typeExpr)
    }
  }

  object ExpressionSelector {
    final case class ByField(fieldName: FieldName) extends ExpressionSelector

    final case class ByLabels(fieldName: Seq[FieldName]) extends ExpressionSelector

    final case class ByType(typeExpr: Expression) extends ExpressionSelector
  }

  def selector[$: P]: P[ExpressionSelector] = P(
    any_label.map(FieldName).map(ExpressionSelector.ByField)
      | labels.map(ExpressionSelector.ByLabels)
      | type_selector.map(ExpressionSelector.ByType)
  )

  def labels[$: P]: P[Seq[FieldName]] = P(
    "{" ~ whsp ~ ("," ~ whsp).? ~ (any_label_or_some ~ whsp ~ ("," ~ whsp ~ any_label_or_some ~ whsp).rep ~ ("," ~ whsp).?).? ~ "}"
  ).map(_.map { case (x, y) => x +: y }.toSeq.flatten).map(_.map(FieldName))

  def type_selector[$: P] = P(
    "(" ~ whsp ~ expression ~ whsp ~ ")"
  )

  def primitive_expression[$: P]: P[Expression] = P(
    temporal_literal
      // Put bytes_literal first, or else we will just parse the initial 0 as natural_literal
      //  '0x"01234567689abcdef"'
      | bytes_literal
      //
      //  "2.0"
      | double_literal
      //
      //  "2"
      | natural_literal
      //
      //  "+2" or "-2"
      | integer_literal
      //
      //  '"ABC"'
      | text_literal
      //
      //  "{ foo = 1      , bar = True }"
      //  "{ foo : Integer, bar : Bool }"
      | ("{" ~ whsp ~/ ("," ~/ whsp./).? ~ record_type_or_literal ~ whsp ~ "}")
      .map(_.getOrElse(Expression.RecordLiteral(Seq())))
      //
      //  "< Foo : Integer | Bar : Bool >"
      //  "< Foo | Bar : Bool >"
      | ("<" ~ whsp ~/ ("|" / whsp./).? ~ union_type ~ whsp ~ ">")
      //
      //  "[1, 2, 3]"
      | non_empty_list_literal
      //
      //  "x"
      //  "x@2"
      | identifier
      //
      //  "( e )"
      | ("(" ~/ complete_expression ~/ ")")
  )

  def record_type_or_literal[$: P]: P[Option[Expression]] = P(
    empty_record_literal.map(Some.apply)
      | non_empty_record_type_or_literal.?
  )

  def empty_record_literal[$: P]: P[Expression.RecordLiteral] = P(
    "=" ~/ (whsp ~ ",").?
  ).map(_ => Expression.RecordLiteral(Seq()))

  def non_empty_record_type_or_literal[$: P]: P[Expression] = P(
    non_empty_record_type | non_empty_record_literal
  )

  def non_empty_record_type[$: P]: P[Expression.RecordType] = P(
    record_type_entry ~ (whsp ~ "," ~ whsp ~ record_type_entry).rep ~ (whsp ~ ",").?
  ).map { case (headName, headExpr, tail) => (headName, headExpr) +: tail }.map(Expression.RecordType)

  def record_type_entry[$: P]: P[(FieldName, Expression)] = P(
    any_label_or_some.map(FieldName) ~ whsp ~ ":" ~ whsp1 ~ expression
  )

  def non_empty_record_literal[$: P]: P[Expression.RecordLiteral] = P(
    record_literal_entry ~ (whsp ~ "," ~ whsp ~ record_literal_entry).rep ~ (whsp ~ ",").?
  ).map { case (head, tail) => Expression.RecordLiteral.of(head +: tail) }

  def record_literal_entry[$: P]: P[RawRecordLiteral] = P(
    any_label_or_some.map(FieldName) ~ record_literal_normal_entry.?
  ).map(RawRecordLiteral.tupled)

  def record_literal_normal_entry[$: P]: P[(Seq[FieldName], Expression)] = P(
    (whsp ~ "." ~ whsp ~/ any_label_or_some.map(FieldName)).rep ~ whsp ~ "=" ~ whsp ~/ expression
  )

  def union_type[$: P]: P[Expression.UnionType] = P(
    (union_type_entry ~ (whsp ~ "|" ~ whsp ~/ union_type_entry).rep ~ (whsp ~ "|").?).?
  ).map {
    case Some((headName, headType, tail)) => Expression.UnionType((headName, headType) +: tail)
    case None => Expression.UnionType(Seq())
  }

  def union_type_entry[$: P] = P(
    any_label_or_some.map(ConstructorName) ~ (whsp ~ ":" ~ whsp1 ~/ expression).?
  )

  def non_empty_list_literal[$: P]: P[Expression.NonEmptyList] = P(
    "[" ~ whsp ~ ("," ~ whsp).? ~ expression ~ whsp ~ ("," ~ whsp ~/ expression ~ whsp).rep ~ ("," ~ whsp).? ~ "]"
  ).map { case (head, tail) => Expression.NonEmptyList(head, tail) }

  def shebang[$: P] = P(
    "#!" ~/ not_end_of_line.rep.! ~ end_of_line
  )

  def complete_dhall_file[$: P] = P(
    shebang.rep ~ whsp ~ expression ~ whsp ~ line_comment_prefix.?
  ).map { case (shebangContents, expr) => DhallFile(shebangContents, expr) }

  def complete_expression[$: P] = P(
    whsp ~ expression ~ whsp
  )

  // Helpers to make sure we are using valid keyword and operator names.
  def requireKeyword[$: P](name: String): P[Unit] = {
    assert(simpleKeywordsSet contains name, s"Keyword $name must be one of the supported Dhall keywords")
    P(name)
  }

}

object Parser {
  // Fail with a message.  See https://github.com/com-lihaoyi/fastparse/issues/213
  // The message shows up as "Expected ..."; phrase it appropriately.
  //  private def Fail[T](expected: String)(implicit ctx: P[_]): P[T] = {
  //    val res = ctx.freshFailure()
  //    if (ctx.verboseFailures) ctx.setMsg(ctx.index, () => expected)
  //    res
  //  }

  def parseDhall(source: String): Parsed[DhallFile] = parse(source, Grammar.complete_dhall_file(_))

  def parseDhall(source: InputStream): Parsed[DhallFile] = parse(source, Grammar.complete_dhall_file(_))

  private def localDateTimeZone(dateOption: Option[DateLiteral], timeOption: Option[TimeLiteral], zoneOption: Option[ZoneOffset]): Expression = {
    val dateR = dateOption.map { date => (FieldName("date"), date) }
    val dateT = dateOption.map { date => (FieldName("date"), Builtin(SyntaxConstants.Builtin.Date)) }
    val timeR = timeOption.map { time => (FieldName("time"), time) }
    val timeT = timeOption.map { time => (FieldName("time"), Builtin(SyntaxConstants.Builtin.Time)) }
    val zoneR = zoneOption.map { zone => (FieldName("timeZone"), TimeZoneLiteral(zone)) }
    val zoneT = zoneOption.map { zone => (FieldName("timeZone"), Builtin(SyntaxConstants.Builtin.TimeZone)) }

    val record = RecordLiteral(Seq(dateR, timeR, zoneR).flatten)
    val recordType = RecordType(Seq(dateT, timeT, zoneT).flatten)

    Annotation(record, recordType) // Return { date : Date, time : Time, timeZone : TimeZone } or some subset of that record.
  }

  def localDateTimeWithZone(date: DateLiteral, time: TimeLiteral, zone: ZoneOffset): Expression = localDateTimeZone(Some(date), Some(time), Some(zone))

  def localTimeWithZone(time: TimeLiteral, zone: ZoneOffset): Expression = localDateTimeZone(None, Some(time), Some(zone))

  def localDateTime(date: DateLiteral, time: TimeLiteral): Expression = localDateTimeZone(Some(date), Some(time), None)
}
