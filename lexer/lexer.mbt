///|
pub(all) enum Keyword {
  Let
  If
  Else
  While
  Return
  Fn
  Struct
  As
  Ref
  SizeOf
} derive(Show, Eq)

///|
pub enum Token {
  Bool(Bool)
  Int(Int)
  Int64(Int64)
  UInt(UInt)
  UInt64(UInt64)
  Float(Float)
  Double(Double)
  Keyword(Keyword)
  Upper(String)
  Lower(String)
  Operator(String) // +, -, *, /, %, =, ==, !=, <, >, <=, >=, &&, ||
  Bracket(Char) // (, ), [, ], {, }
  Symbol(String) // , : , . , @
  Terminator // ;
  LeftArrow // ->
  EOF
} derive(Show, Eq)

///|
pub fn lex(code : String) -> Array[Token] {
  let tokens = Array::new()
  loop code[:] {
    [] => {
      tokens.push(Token::EOF)
      break tokens
    }
    [';', .. rest] => {
      tokens.push(Token::Terminator)
      continue rest
    }
    [.. "::", .. rest] => {
      tokens.push(Token::Symbol("::"))
      continue rest
    }
    [':' | '.' | ',' as c, .. rest] => {
      tokens.push(Token::Symbol(c.to_string()))
      continue rest
    }
    [' ' | '\n' | '\r' | '\t', .. rest] => continue rest
    [.. "fn", .. rest] => { // skip whitespace
      tokens.push(Token::Keyword(Keyword::Fn))
      continue rest
    }
    [.. "let", .. rest] => {
      tokens.push(Token::Keyword(Keyword::Let))
      continue rest
    }
    [.. "if", .. rest] => {
      tokens.push(Token::Keyword(Keyword::If))
      continue rest
    }
    [.. "else", .. rest] => {
      tokens.push(Token::Keyword(Keyword::Else))
      continue rest
    }
    [.. "while", .. rest] => {
      tokens.push(Token::Keyword(Keyword::While))
      continue rest
    }
    [.. "return", .. rest] => {
      tokens.push(Token::Keyword(Keyword::Return))
      continue rest
    }
    [.. "struct", .. rest] => {
      tokens.push(Token::Keyword(Keyword::Struct))
      continue rest
    }
    [.. "as", .. rest] => {
      tokens.push(Token::Keyword(Keyword::As))
      continue rest
    }
    [.. "ref", .. rest] => {
      tokens.push(Token::Keyword(Keyword::Ref))
      continue rest
    }
    [.. "sizeof", .. rest] => {
      tokens.push(Token::Keyword(Keyword::SizeOf))
      continue rest
    }
    [.. "true", .. rest] => {
      tokens.push(Token::Bool(true))
      continue rest
    }
    [.. "false", .. rest] => {
      tokens.push(Token::Bool(false))
      continue rest
    }
    ['+', .. rest] => {
      tokens.push(Token::Operator("+".to_string()))
      continue rest
    }
    [.. "->", .. rest] => {
      tokens.push(Token::LeftArrow)
      continue rest
    }
    ['-', .. rest] => {
      tokens.push(Token::Operator("-".to_string()))
      continue rest
    }
    ['*', .. rest] => {
      tokens.push(Token::Operator("*".to_string()))
      continue rest
    }
    ['/', .. rest] => {
      tokens.push(Token::Operator("/"))
      continue rest
    }
    ['%', .. rest] => {
      tokens.push(Token::Operator("%"))
      continue rest
    }
    [.. ">>", .. rest] => {
      tokens.push(Token::Operator(">>".to_string()))
      continue rest
    }
    [.. "<<", .. rest] => {
      tokens.push(Token::Operator("<<".to_string()))
      continue rest
    }
    [.. "==", .. rest] => {
      tokens.push(Token::Operator("==".to_string()))
      continue rest
    }
    [.. "!=", .. rest] => {
      tokens.push(Token::Operator("!=".to_string()))
      continue rest
    }
    [.. "<=", .. rest] => {
      tokens.push(Token::Operator("<=".to_string()))
      continue rest
    }
    [.. ">=", .. rest] => {
      tokens.push(Token::Operator(">=".to_string()))
      continue rest
    }
    [.. "&&", .. rest] => {
      tokens.push(Token::Operator("&&".to_string()))
      continue rest
    }
    [.. "&", .. rest] => {
      tokens.push(Token::Operator("&".to_string()))
      continue rest
    }
    [.. "||", .. rest] => {
      tokens.push(Token::Operator("||".to_string()))
      continue rest
    }
    [.. "|", .. rest] => {
      tokens.push(Token::Operator("|".to_string()))
      continue rest
    }
    ['<', .. rest] => {
      tokens.push(Token::Operator("<".to_string()))
      continue rest
    }
    ['>', .. rest] => {
      tokens.push(Token::Operator(">".to_string()))
      continue rest
    }
    ['=', .. rest] => {
      tokens.push(Token::Operator("=".to_string()))
      continue rest
    }
    ['(', .. rest] => {
      tokens.push(Token::Bracket('('))
      continue rest
    }
    [')', .. rest] => {
      tokens.push(Token::Bracket(')'))
      continue rest
    }
    ['[', .. rest] => {
      tokens.push(Token::Bracket('['))
      continue rest
    }
    [']', .. rest] => {
      tokens.push(Token::Bracket(']'))
      continue rest
    }
    ['{', .. rest] => {
      tokens.push(Token::Bracket('{'))
      continue rest
    }
    ['}', .. rest] => {
      tokens.push(Token::Bracket('}'))
      continue rest
    }
    ['a'..='z' as c, .. rest] => {
      let ident = [c]
      let rest_str = loop rest {
        ['a'..='z' | 'A'..='Z' | '0'..='9' | '_' as c, .. rest_str] => {
          ident.push(c)
          continue rest_str
        }
        _ as rest_str => break rest_str
      }
      tokens.push(Token::Lower(String::from_array(ident)))
      continue rest_str
    }
    ['A'..='Z' as c, .. rest] => {
      let ident = [c]
      let rest_str = loop rest {
        ['a'..='z' | 'A'..='Z' | '0'..='9' | '_' as c, .. rest_str] => {
          ident.push(c)
          continue rest_str
        }
        _ as rest_str => break rest_str
      }
      tokens.push(Token::Upper(String::from_array(ident)))
      continue rest_str
    }
    ['0'..='9' as c, .. rest] => {
      let mut has_dot = false
      let num_chars = [c]
      let rest_str = loop rest {
        ['0'..='9' as c, .. rest_str] => {
          num_chars.push(c)
          continue rest_str
        }
        ['.', .. rest_str] if not(has_dot) => {
          has_dot = true
          num_chars.push('.')
          continue rest_str
        }
        ['L' | 'l', .. rest_str] => {
          let num_str = String::from_array(num_chars)
          let num = match (try? @strconv.parse_int64(num_str)) {
            Ok(n) => n
            Err(_) => {
              println("Parse Int64 failed: \{num_str}")
              panic()
            }
          }
          tokens.push(Token::Int64(num))
          break rest_str
        }
        [.. "UL", .. rest_str] => {
          let num_str = String::from_array(num_chars)
          let num = match (try? @strconv.parse_uint64(num_str)) {
            Ok(n) => n
            Err(_) => {
              println("Parse UInt64 failed: \{num_str}")
              panic()
            }
          }
          tokens.push(Token::UInt64(num))
          break rest_str
        }
        ['U' | 'u', .. rest_str] => {
          let num_str = String::from_array(num_chars)
          let num = match (try? @strconv.parse_uint(num_str)) {
            Ok(n) => n
            Err(_) => {
              println("Parse UInt failed: \{num_str}")
              panic()
            }
          }
          tokens.push(Token::UInt(num))
          break rest_str
        }
        ['F' | 'f', .. rest_str] => {
          let num_str = String::from_array(num_chars)
          let num = match (try? @strconv.parse_double(num_str)) {
            Ok(n) => n
            Err(_) => {
              println("Parse Float failed: \{num_str}")
              panic()
            }
          }
          tokens.push(Token::Float(num.to_float()))
          break rest_str
        }
        _ as rest if has_dot => {
          let num_str = String::from_array(num_chars)
          let num = match (try? @strconv.parse_double(num_str)) {
            Ok(n) => n
            Err(_) => {
              println("Parse Double failed: \{num_str}")
              panic()
            }
          }
          tokens.push(Token::Double(num))
          break rest
        }
        _ as rest => {
          let num_str = String::from_array(num_chars)
          let num = match (try? @strconv.parse_int(num_str)) {
            Ok(n) => n
            Err(_) => {
              println("Parse Int failed: \{num_str}")
              panic()
            }
          }
          tokens.push(Token::Int(num))
          break rest
        }
      }
      continue rest_str
    }
    other_strs => {
      println("Parse Error: Unexpected char: \{other_strs}")
      panic()
    }
  }
}

///|
test "lex" {
  let code =
    #|fn add(x: Int, y: Int) -> Int {
    #|  return x + y - 342UL
    #|}
  let toks = lex(code)
  inspect(
    toks,
    content="[Keyword(Fn), Lower(\"add\"), Bracket('('), " +
      "Lower(\"x\"), Symbol(\":\"), Upper(\"Int\"), " +
      "Symbol(\",\"), Lower(\"y\"), Symbol(\":\"), " +
      "Upper(\"Int\"), Bracket(')'), LeftArrow, " +
      "Upper(\"Int\"), Bracket('{'), Keyword(Return), " +
      "Lower(\"x\"), Operator(\"+\"), Lower(\"y\"), " +
      "Operator(\"-\"), UInt64(342), Bracket('}'), EOF]",
  )
}
