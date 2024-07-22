//
// Analyzer for simple C programs.  This component performs
// semantic analysis, in particular collecting variable
// names and their types. The analysis also checks to ensure
// variable names are unique --- no duplicates.
//
// If all is well, a "symbol table" is built and returned,
// containing all variables and their types. A symbol table
// is a list of tuples of the form (name, type).  Example:
//
//   [("x", "int"); ("y", "int"); ("z", "real")]
//
// Modified by:
//   Ricardo Gonzalez
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module analyzer =

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
    | next_token when next_token.StartsWith("real_literal") -> matchToken next_token tokens
    | next_token when next_token.StartsWith("str_literal")  -> matchToken next_token tokens
    | "true"                                                -> matchToken "true" tokens
    | "false"                                               -> matchToken "false" tokens
    |_ ->  tokens //failwith ("expecting identifier or literal, but found " + next_token)

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
  // Helper function: duplicate checker
  //

  let rec private dup_Checker symtab identifier = 
    match symtab with 
    | []                                                       -> false
    | (varName, varType)::tl when identifier = varName         -> true
    | hd::tl                                                   ->  dup_Checker tl identifier

  //
  //  Helper function: variable declaration consumer
  //
  let private vardDeclConsumer intOrReal tokens symtab =

    let T2 = matchToken intOrReal tokens  // consume 'int' or 'real'
    let next_token = List.head T2    // this token is of the form identifier:[name]
    let identifier = next_token.[11..] // we know ':' in 'identifier:' is at index 10

    if dup_Checker symtab identifier then  
      failwith ("redefinition of variable '" + identifier + "'")
    else 
      let ST2 = (identifier, intOrReal) :: symtab
      let T3 = matchToken next_token  T2  // consume identifier
      let T4 = matchToken ";" T3  // the next token must be a ;
      (T4, ST2)

  //
  // vardecl
  //
  let private vardecl tokens symtab =
    let theHead = (List.head tokens)
    match theHead with
    | "int"     ->  vardDeclConsumer "int" tokens symtab
    | "real"    ->  vardDeclConsumer "real" tokens symtab
    | _         ->  (tokens, symtab)

  //
  // input
  //
  let private input tokens symtab =
    let T2 = matchToken "cin" tokens // We know the head of the list is cin
    let T3 = matchToken ">>" T2
    let next_token = List.head T3

    if next_token.StartsWith("identifier") then 
        let T4 = matchToken next_token  T3  // consume identifier
        let T5 = matchToken ";" T4  // the next token must be a ;
        (T5, symtab)
    else  
        (T3, symtab) //failwith ("expecting identifier, but found " + next_token)

  //
  // output
  //
  let private output tokens symtab =
    let T2 = matchToken "cout" tokens // We know the head of the list is cout
    let T3 = matchToken "<<" T2
    let T4 = output_value T3
    let T5 = matchToken ";" T4
    (T5, symtab)

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
    | _      ->  tokens //failwith ("expecting operator, but found " + next_token)

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
  let private expr (tokens:string List)  = 
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
  let private assignment (tokens:string List) symtab =
    let next_token = List.head tokens

    if next_token.StartsWith("identifier") then 
      let T2 = matchToken next_token tokens  // consume the identifier
      let T3 = matchToken "=" T2
      let T4 = expr T3
      let T5 = matchToken ";" T4
      (T5, symtab)
    else 
      (tokens, symtab)

  //
  // empty
  //
  let empty tokens symtab = 
    let T2 = matchToken ";" tokens
    (T2, symtab)

  //
  // condition
  //
  let private condition tokens = 
    expr tokens 

  //
  //stmt
  //
  let rec private stmt tokens symtab =
    let next_token = List.head tokens

    match next_token with
    | ";"                                                 -> empty tokens symtab
    | "int"                                               -> vardecl tokens symtab
    | "real"                                              -> vardecl tokens symtab
    | "cin"                                               -> input tokens symtab
    | "cout"                                              -> output tokens symtab
    | next_token when next_token.StartsWith("identifier") -> assignment tokens symtab
    | "if"                                                -> ifstmt tokens symtab
    | "}"                                                 -> (tokens, symtab)
    |_ -> (tokens, symtab)

  //
  // ifstmt
  //
  and private ifstmt tokens symtab = 
    // We know 'if' is at the top of the list!
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3
    let T5 = matchToken ")" T4
    let (T6, ST6) = then_part T5 symtab
    let (T7, ST7) = else_part T6 ST6
    (T7, ST7)

  //
  // then_part
  //
  and private then_part tokens symtab = 
     stmt tokens symtab

  //
  // else_part
  //
  and private else_part tokens symtab =
    let next_token = List.head tokens

    if next_token = "else" then
      let T2 = matchToken "else" tokens
      stmt T2 symtab
    else
      (tokens, symtab)

  //
  // morestmts
  //
  let rec private morestmts tokens symtab =      // The recursive loop happens here
    let next_token = List.head tokens
    match next_token with  
    | "}"   -> (tokens, symtab)
    | "$"   -> (tokens, symtab)
    | _     -> let (T2, ST2) = stmt tokens symtab   // Modify stmt to return tuple in ALL CASES
               morestmts T2 ST2

  //
  // stmts
  //
  let rec private stmts tokens symtab =
    let (T2, ST2) = stmt tokens symtab  // Read at least one statement
    let (T3, ST3) = morestmts T2 ST2
    (T3, ST3) 

  // 
  // simpleC
  //
  let private simpleC tokens = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7, symboltable) = stmts T6 []     // Change this call
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8
    (T9, symboltable)   // return the tuple (empty list (T8), symboltable)

  //
  // build_symboltable tokens
  //
  // Given a list of tokens, analyzes the program by looking
  // at variable declarations and collecting them into a
  // list. This list is known as a symbol table. Returns
  // a tuple (result, symboltable), where result is a string 
  // denoting "success" if valid, otherwise a string of the 
  // form "semantic_error:...".
  //
  // On success, the symboltable is a list of tuples of the
  // form (name, type), e.g. [("x","int"); ("y","real")]. On 
  // an error, the returned list is empty [].
  //
  let build_symboltable tokens = 
    try
      let (T2, symboltable) = simpleC tokens
      ("success", symboltable)
    with 
      | ex -> ("semantic_error: " + ex.Message, [])
