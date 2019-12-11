module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(AId name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
  = question(str ques, AId id, AType typ)
  | \if(AExpr condition, list[AQuestion] questions)
  | \if(AExpr condition, list[AQuestion] thenQuestions, list[AQuestion] elseQuestions)
  | assign(AQuestion q, AExpr exp)
  ; 

data AExpr(loc src = |tmp:///|)
  = ref(AId id)
  | negative(AExpr exp)
  | not(AExpr exp)
  | bracketExp(AExpr exp)
  | multiply(AExpr lhs, AExpr rhs)
  | divide(AExpr lhs, AExpr rhs)
  | plus(AExpr lhs, AExpr rhs)
  | minus(AExpr lhs, AExpr rhs)
  | less(AExpr lhs, AExpr rhs)
  | lessEq(AExpr lhs, AExpr rhs)
  | equal(AExpr lhs, AExpr rhs)
  | notEqual(AExpr lhs, AExpr rhs)
  | greaterEq(AExpr lhs, AExpr rhs)
  | greater(AExpr lhs, AExpr rhs)
  | and(AExpr lhs, AExpr rhs)
  | or(AExpr lhs, AExpr rhs)
  | integer(int intValue)
  | boolean(bool boolValue)
  | string(str strValue)
  ;
  
data AId(loc src = |tmp:///|)
  = id(str name);

data AType(loc src = |tmp:///|)
  = boolType()
  | integerType()
  | stringType()
  ;

