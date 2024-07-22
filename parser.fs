//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// Ricardo Gonzalez
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // matchToken
  //
  let private matchToken expected_token tokens =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

  //
  // expr_value
  //
  let private expr_value (tokens:string List) = 
    let next_token = List.head tokens
  
    match next_token with
    | next_token when next_token.StartsWith("identifier")   -> matchToken next_token tokens
    | next_token when next_token.StartsWith("int_literal")  -> matchToken next_token tokens
    | next_token when next_token.StartsWith("real_literal")  -> matchToken next_token tokens
    | next_token when next_token.StartsWith("str_literal")  -> matchToken next_token tokens
    | "true"                                                -> matchToken "true" tokens
    | "false"                                               -> matchToken "false" tokens
    |_ -> failwith ("expecting identifier or literal, but found " + next_token)

  //
  // output_value
  // 
  let private output_value tokens =
    let next_token = List.head tokens
    if next_token = "endl" then 
      matchToken "endl" tokens  // consume it
    else 
      expr_value tokens

  //
  //  Helper function: variable declaration consumer
  //

  let private vardDeclConsumer intOrReal tokens =

    let T2 = matchToken intOrReal tokens
    let next_token = List.head T2
    if next_token.StartsWith("identifier") then 

      let T3 = matchToken next_token  T2  // consume identifier
      let T4 = matchToken ";" T3  // the next token must be a ;
      T4
    else  failwith ("expecting identifier, but found " + next_token)


  //
  // vardecl
  //
  let private vardecl tokens =

    let theHead = (List.head tokens)

    match theHead with
    | "int"     ->  vardDeclConsumer "int" tokens
    | "real"    ->  vardDeclConsumer "real" tokens
    | _         ->  tokens

  //
  // input
  //
  let private input tokens =
    let T2 = matchToken "cin" tokens // We know the head of the list is cin
    let T3 = matchToken ">>" T2

    let next_token = List.head T3

    if next_token.StartsWith("identifier") then 
      let T4 = matchToken next_token  T3  // consume identifier
      let T5 = matchToken ";" T4  // the next token must be a ;
      T5
    else  failwith ("expecting identifier, but found " + next_token)

  //
  // output
  //
  let private output tokens =
    let T2 = matchToken "cout" tokens // We know the head of the list is cout
    let T3 = matchToken "<<" T2
    let T4 = output_value T3
    let T5 = matchToken ";" T4
    T5

  //
  // expr_op
  //
  let private expr_op (tokens:string List) = 
    let next_token = List.head tokens

    match next_token with
    | "+"   -> matchToken "+" tokens
    | "-"   -> matchToken "-" tokens
    | "*"   -> matchToken "*" tokens
    | "/"   -> matchToken "/" tokens
    | "^"   -> matchToken "^" tokens
    | "<"   -> matchToken "<" tokens
    | "<="   -> matchToken "<=" tokens
    | ">"   -> matchToken ">" tokens
    | ">="   -> matchToken ">=" tokens
    | "=="   -> matchToken "==" tokens
    | "!="   -> matchToken "!=" tokens
    | _      ->  failwith ("expecting operator, but found " + next_token)

  //
  // Helper function: isOperator     
  //
  let private isOperator str =
    str = "+" || str = "-" || str = "*" || 
    str = "/" || str = "^" || str = "<" ||
    str = "<=" || str = ">" || str = ">=" || 
    str = "==" || str = "!=" 

  //
  // expr    
  //
  let private expr (tokens:string List) = 
     
      let T2 = expr_value tokens
  
      let next_token = List.head T2

      if isOperator next_token then
        let T3 = expr_op T2
        let T4 = expr_value T3
        T4
      else 
        T2

  //
  // assignment
  //
  let private assignment (tokens:string List) =
    let next_token = List.head tokens

    if next_token.StartsWith("identifier") then 
      let T2 = matchToken next_token tokens  // consume the identifier
      let T3 = matchToken "=" T2
      let T4 = expr T3
      let T5 = matchToken ";" T4
      T5
    else 
      failwith ("expecting identifier, but found " + next_token)

  //
  // empty
  //
  let empty tokens = 
    matchToken ";" tokens

  //
  // condition
  //
  let private condition tokens = 
    expr tokens

  //
  //stmt
  //
  let rec private stmt tokens =
    let next_token = List.head tokens

    match next_token with
    | ";"                                                 -> empty tokens
    | "int"                                               -> vardecl tokens
    | "real"                                              -> vardecl tokens
    | "cin"                                               -> input tokens
    | "cout"                                              -> output tokens
    | next_token when next_token.StartsWith("identifier") -> assignment tokens
    | "if"                                                -> ifstmt tokens
    | "}"                                                 ->  failwith ("expecting statement, but found " + next_token)
    |_ -> tokens

  //
  // ifstmt
  //
  and private ifstmt tokens = 
    // We know if is at the top of the list!
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3
    let T5 = matchToken ")" T4
    let T6 = then_part T5
    let T7 = else_part T6
    T7

  //
  // then_part
  //
  and private then_part tokens = 
     stmt tokens

  //
  // else_part
  //
  and private else_part tokens =
    let next_token = List.head tokens

    if next_token = "else" then
      let T2 = matchToken "else" tokens
      stmt T2
    else
      tokens

  //
  // morestmts
  //
  let rec private morestmts tokens =
    let next_token = List.head tokens
    match next_token with  
    | "}"   -> tokens
    | "$"   -> tokens
    | _     -> let T2 = stmt tokens
               morestmts T2

  //
  // stmts
  //
  let private stmts tokens =
    let T2 = stmt tokens
    morestmts T2

  //
  // simpleC
  //
  let private simpleC tokens = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6
    let T8 = matchToken "}" T7
    matchToken "$" T8


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message
