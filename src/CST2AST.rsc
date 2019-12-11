module CST2AST

import Syntax;
import AST;
import IO;

import ParseTree;
import String;
import Boolean;


/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  switch (f) {
  	case (Form)`form <Id name> { <Question* qs> }`: return form(id("<name>", src=name@\loc), [cst2ast(q) | Question q <- qs], src=f@\loc);
  	default: throw "Unknown Form format: <f>";
  }
}

AQuestion cst2ast(Question q) {
  switch(q) {
    case (Question)`<Str ques> <Id ident>: <Type typ>`: return question("<ques>", id("<ident>", src=ident@\loc), cst2ast(typ), src=q@\loc);
    case (Question)`if (<Expr cond>) {<Question* qs>}`: return \if(cst2ast(cond), [cst2ast(q) | Question q <- qs], src=q@\loc);
    case (Question)`if (<Expr cond>) {<Question* thenQs>} else {<Question* elseQs> }`: return \if(cst2ast(cond), [cst2ast(q) | Question q <- thenQs], [cst2ast(q) | Question q <- elseQs], src=q@\loc);
    case (Question)`<Question q> = <Expr exp>`: return assign(cst2ast(q), cst2ast(exp), src=q@\loc);
    default: throw "Unknown Question format: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(id("<x>", src=x@\loc));
    case (Expr)`-<Expr exp>`: return negative(cst2ast(exp), src=e@\loc);
    case (Expr)`!<Expr exp>`: return not(cst2ast(exp), src=e@\loc);
    case (Expr)`(<Expr exp>)`: return bracketExp(cst2ast(exp), src=e@\loc);
    case (Expr)`<Expr lhs> * <Expr rhs>`: return multiply(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> / <Expr rhs>`: return divide(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> + <Expr rhs>`: return plus(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs> - <Expr rhs>`: return minus(cst2ast(lhs), cst2ast(rhs), src=e@\loc);    //, src=(lhs,rhs)@\loc);
    case (Expr)`<Expr lhs>\<<Expr rhs>`: return less(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs>\<=<Expr rhs>`: return lessEq(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs>==<Expr rhs>`: return equal(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs>!=<Expr rhs>`: return notEqual(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs>\>=<Expr rhs>`: return greaterEq(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs>\><Expr rhs>`: return greater(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs>&&<Expr rhs>`: return and(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Expr lhs>||<Expr rhs>`: return or(cst2ast(lhs), cst2ast(rhs), src=e@\loc);
    case (Expr)`<Int integerValue>`: return integer(toInt("<integerValue>"), src=e@\loc);
    case (Expr)`<Bool booleanValue>`: return boolean(fromString("<booleanValue>"), src=e@\loc);
    case (Expr)`<Str stringValue>`: return string(stringValue, src=e@\loc);
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  switch (t) {
    case (Type)`boolean`: return boolType();
    case (Type)`integer`: return integerType();
    case (Type)`string`: return stringType();
    default: throw "Unknown Type format: <t>";
  }
}
