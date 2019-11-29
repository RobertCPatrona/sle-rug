module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id "{" Question* "}"; 

// question, computed question, block, if-then-else, if-then
syntax Question
  = Str Id ":" Type
  | "if" "(" Expr ")" "{" Question* "}"
  | "if" "(" Expr ")" "{" Question* "}" "else" "{" Question* "}"
  | Question "=" Expr
  ; 

// +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
syntax Expr 
  = Id \ "true" \ "false" // true/false are reserved keywords.
  | "-" Expr
  | '!' Expr
  | "(" Expr ")"
  > left (Expr '*' Expr | Expr '/' Expr)
  > left (Expr '+' Expr | Expr '-' Expr)
  > left (Expr '\>' Expr | Expr '\<' Expr | Expr '\>=' Expr | Expr '\<=' Expr | Expr '==' Expr | Expr '!=' Expr)
  > left (Expr '&&' Expr | Expr '||' Expr)
  | Int
  | Bool
  | Str
  ;
  
syntax Type
  = "boolean"
  | "integer"
  ;  
  
lexical Str 
  = [\"] ![\"]* [\"]
  ;	

lexical Int 
  = [0-9]+;

lexical Bool = 
  | "true"
  | "false";

