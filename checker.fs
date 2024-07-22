//
// Analyzer for simple C programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid simple C program.
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

module checker =
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
  // Helper function: declaration checker
  //

  let rec private dec_Checker symtab identifier = 
    match symtab with 
    | []                                                       -> false
    | (varName, varType)::tl when identifier = varName         -> true
    | hd::tl                                                   -> dec_Checker tl identifier

  //
  // expr_value
  //
  let private expr_value (tokens:string List) symtab = 
    let next_token = List.head tokens
  
    match next_token with
    | next_token when next_token.StartsWith("identifier")   ->   if dec_Checker symtab next_token.[11..] then 
                                                                    matchToken next_token tokens
                                                                 else 
                                                                    failwith ("variable '" + next_token.[11..] + "' undefined")                       
    | next_token when next_token.StartsWith("int_literal")  -> matchToken next_token tokens
    | next_token when next_token.StartsWith("real_literal") -> matchToken next_token tokens
    | next_token when next_token.StartsWith("str_literal")  -> matchToken next_token tokens
    | "true"                                                -> matchToken "true" tokens
    | "false"                                               -> matchToken "false" tokens
    |_ ->  tokens

  //
  // output_value
  // 
  let private output_value tokens symtab =
    let next_token = List.head tokens
    if next_token = "endl" then 
        matchToken "endl" tokens  // consume it
    else 
        expr_value tokens symtab

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
    else  
        failwith ("expecting identifier, but found " + next_token)


  //
  // vardecl
  //
  let private vardecl tokens symtab =
    let theHead = (List.head tokens)

    match theHead with
    | "int"     ->  vardDeclConsumer "int" tokens 
    | "real"    ->  vardDeclConsumer "real" tokens
    | _         ->  tokens

  //
  // input
  //
  let private input tokens symtab =
    let T2 = matchToken "cin" tokens // We know the head of the list is cin
    let T3 = matchToken ">>" T2
    let next_token = List.head T3

    if next_token.StartsWith("identifier") then 

       if dec_Checker symtab next_token.[11..] then 
            let T4 = matchToken next_token  T3  // consume identifier
            let T5 = matchToken ";" T4  // the next token must be a ";"
            T5
       else 
           failwith ("variable '" + next_token.[11..] + "' undefined")                                                        
    else  T3

  //
  // output
  //
  let private output tokens symtab =
      let T2 = matchToken "cout" tokens // We know the head of the list is cout
      let T3 = matchToken "<<" T2
      let T4 = output_value T3 symtab
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
      | "<="  -> matchToken "<=" tokens
      | ">"   -> matchToken ">" tokens
      | ">="  -> matchToken ">=" tokens
      | "=="  -> matchToken "==" tokens
      | "!="  -> matchToken "!=" tokens
      | _     ->  tokens //failwith ("expecting operator, but found " + next_token)

  //
  // Helper function: is arithmetic operator
  //
  let private isArithOp str =
    str = "+" || str = "-" || str = "*" || 
    str = "/" || str = "^" 

  //
  // Helper function: is comparison operator
  //
  let private isCompOp str =
    str = "<" || str = "<=" || str = ">" || 
    str = ">=" || str = "==" || str = "!=" 

  //
  // Helper function: is operator     
  //
  let private isOperator str =
    str = "+" || str = "-" || str = "*" || 
    str = "/" || str = "^" || str = "<" ||
    str = "<=" || str = ">" || str = ">=" || 
    str = "==" || str = "!=" 

  //
  // Helper function: get identifier type
  //
  let rec private getIdentifierType identifier symtab =
   
    match symtab with 
    | []                                                       -> " "
    | (varName, varType)::tl when identifier = varName         -> varType
    | hd::tl                                                   -> getIdentifierType identifier tl

  //
  // Helper function: get type of an identifer or a literal
  //
  let rec private getType (value:string) symtab =

    if value.StartsWith("identifier") then
        getIdentifierType value.[11..] symtab
    elif value.StartsWith("real_") then
        "real"
    elif value.StartsWith("int_") then
        "int"
    elif value.StartsWith("str_") then
        "str"
    elif value.StartsWith("bool_") then  
        "bool"
    elif value = "false" || value = "true" then
        "bool"
    else 
        " "
  //
  // expr    
  //
  let private expr (tokens:string List) symtab = 

      let left = List.head tokens // get second operand
      let left_type = getType (List.head tokens) symtab // get first operand type
      let T2 = expr_value tokens symtab   // consume it

      // At this point we cannot assume the head is an operator
      if isOperator (List.head T2) then 

          // we now know for sure an operator is at the head
          let operator = List.head T2    // get the operator
          let T3 = expr_op T2    // consume it

          let right = List.head T3 // get second operand
          let right_type = getType (List.head T3) symtab // get second operand type
          let T4 = expr_value T3 symtab // consume it

          // now check the type of the operator
          if isArithOp operator then // Arithmetic operator
  
               if ((left_type <> "int" && left_type <> "real") || (right_type <> "int" && right_type <> "real")) then
                  failwith("operator " + operator + " must involve 'int' or 'real'")
               elif left_type <> right_type then 
                  failwith("type mismatch '" + left_type + "' "  + operator + " '" + right_type + "'")
               else // if we hit this branch then left_type = right_type
                  (T4, left_type) // might as well could be (T4, right_type)

          elif isCompOp operator then  // Comparison operator

               if left_type <> right_type then 
                    failwith("type mismatch '" + left_type + "' "  + operator + " '" + right_type + "'")
               elif operator = "==" then 
                    if (left_type = "real" && right_type = "real") then
                          printfn "warning: comparing real numbers with == may never be true"
                          (T4, "bool")
                    else 
                          (T4, "bool") 
               else
                    (T4, "bool")

          else 
               (T4, " ")
      else  // if we hit this branch then the expression must be an assignment
          (T2, left_type) 

  //
  // assignment
  //
  let private assignment (tokens:string List) symtab =
    let left = List.head tokens
    let left_type = getType (List.head tokens) symtab

    if dec_Checker symtab left.[11..] then  // Check if variable has been declared

         let T2 = matchToken left tokens  // consume the identifier
         let T3 = matchToken "=" T2
         let (T4, exprType) = expr T3 symtab

         if left_type = exprType then 
             let T5 = matchToken ";" T4
             T5
         elif (left_type = "real" && exprType = "int") then
             let T5 = matchToken ";" T4
             T5
         else
             failwith("cannot assign '" + exprType + "' to variable of type '" + left_type + "'")
    else 
         failwith ("variable '" + left.[11..] + "' undefined")

  //
  // empty
  //
  let empty tokens symtab = 
    let T2 = matchToken ";" tokens
    T2

  //
  // condition
  //
  let private condition tokens symtab = 
    let (T2, exprType) = expr tokens symtab
    if exprType <> "bool" then 
        failwith("if condition must be 'bool', but found '" + exprType + "'")
    else    
        T2

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
    | "}"                                                 -> tokens
    |_ -> tokens

  //
  // ifstmt
  //
  and private ifstmt tokens symtab = 
    // We know 'if' is at the top of the list!
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3 symtab
    let T5 = matchToken ")" T4
    let T6 = then_part T5 symtab
    let T7 = else_part T6 symtab
    T7

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
      tokens

  //
  // morestmts
  //
  let rec private morestmts tokens symtab =      // The recursive loop happens here
    let next_token = List.head tokens
    match next_token with  
    | "}"   -> tokens
    | "$"   -> tokens
    | _     -> let T2 = stmt tokens symtab
               morestmts T2 symtab

  //
  // stmts
  //
  let rec private stmts tokens symtab =
    let T2 = stmt tokens symtab  // Read at least one statement
    let T3 = morestmts T2 symtab
    T3


  //
  // simpleC
  //
  let private simpleC tokens symboltable = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6 symboltable     // Change this call
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8
    T9

  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck tokens symboltable = 
    try
      let T2 = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message

