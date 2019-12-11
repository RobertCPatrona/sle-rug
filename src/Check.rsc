module Check

import AST;
import Resolve;
import Message; // see standard library
import Set;
import List;

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` )

TEnv collect(AForm f) {
  TEnv result = {};
  for (/question(str ques, AId id, AType typ) := f) {
    result += {<id.src, id.name, ques, ATypeToType(typ)>};
  }
  return result;
}

Type ATypeToType(AType x) {
  switch(x) {
    case boolType(): return tbool();
    case integerType(): return tint();
    case stringType(): return tstr();
    default: return tunknown();
  }
}


set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  result = {};
  for(/AQuestion q := f.questions) {
    result += check(q, tenv, useDef);
  }
  return result;
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.

set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
  switch(q) {
    case question(str ques, AId id, AType typ):
      return { error("Questions with the same name but different types", q) | size(toSet([y | <str x, Type y> <- tenv<name, \type>, x == id.name])) > 1} + 
        { warning("Duplicate question labels ", q) | quesLabelCheck([x==ques | <_,_,str x,_> <- tenv<def,name,label,\type>])};
    case assign(question(str ques, AId id, AType typ), AExpr exp):
      return { error("Computed question type should match the type of the expression.", q) | ATypeToType(typ) != typeOf(exp, tenv, useDef) && check(exp, tenv, useDef) == {}} + check(exp, tenv, useDef);
    case \if(AExpr cond, list[AQuestion] qs):
      return { error("Wrong if condition expression type", cond) | typeOf(cond, tenv, useDef) != tbool() && check(cond, tenv, useDef) == {}} + check(cond, tenv, useDef);
    case \if(AExpr cond, list[AQuestion] thenQs, list[AQuestion] elseQs):
      return { error("Wrong if condition expression type", cond) | typeOf(cond, tenv, useDef) != tbool() && check(cond, tenv, useDef) == {}} + check(cond, tenv, useDef);
    default: return {};
  } 
}

bool quesLabelCheck(list[bool] boolList){
  int cnt  = 0;
  for(bool b <- boolList) {
    if(b) {
      cnt = cnt+1;
    }
    if(cnt == 2) {
      return true;
    }
  }
  return false;
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (e) {
    case ref(str x, src = loc u):
      msgs += { error("Undeclared question", u) | useDef[u] == {} };
    case negative(AExpr exp): {
      checkMsg = check(exp, tenv, useDef);
      msgs += { error("Wrong type for negative Expr", exp) | isEmpty(checkMsg) && typeOf(exp, tenv, useDef) != tint()} + checkMsg;
    }
    case not(AExpr exp): {
      checkMsg = check(exp, tenv, useDef);
      msgs += { error("Wrong type for boolean notExpr", exp) | isEmpty(checkMsg) && typeOf(exp, tenv, useDef) != tbool()} + checkMsg;
    }
    case bracketExp(AExpr exp):
      msgs += check(exp, tenv, useDef);
    case multiply(AExpr lhs, AExpr rhs): {
      lhsCheckMsg = check(lhs, tenv, useDef);
      rhsCheckMsg = check(rhs, tenv, useDef);
      msgs += { error("Wrong lhs operand type for multiplication of Expr", lhs) | isEmpty(lhsCheckMsg) && typeOf(lhs, tenv, useDef) != tint()} + lhsCheckMsg;
      msgs += { error("Wrong rhs operand type for multiplication of Expr", rhs) | isEmpty(rhsCheckMsg) && typeOf(rhs, tenv, useDef) != tint()} + rhsCheckMsg;
    }
    case divide(AExpr lhs, AExpr rhs): {
      lhsCheckMsg = check(lhs, tenv, useDef);
      rhsCheckMsg = check(rhs, tenv, useDef);
      msgs += { error("Wrong lhs operand type for division of Expr", lhs) | isEmpty(lhsCheckMsg) && typeOf(lhs, tenv, useDef) != tint()} + lhsCheckMsg;
      msgs += { error("Wrong rhs operand type for division of Expr", rhs) | isEmpty(rhsCheckMsg) && typeOf(rhs, tenv, useDef) != tint()} + rhsCheckMsg;
    }
    case plus(AExpr lhs, AExpr rhs): {
      lhsCheckMsg = check(lhs, tenv, useDef);
      rhsCheckMsg = check(rhs, tenv, useDef);
      msgs += { error("Wrong lhs operand type for addition of Expr", lhs) | isEmpty(lhsCheckMsg) && typeOf(lhs, tenv, useDef) != tint()} + lhsCheckMsg;
      msgs += { error("Wrong rhs operand type for addition of Expr", rhs) | isEmpty(rhsCheckMsg) && typeOf(rhs, tenv, useDef) != tint()} + rhsCheckMsg;
    }
    case minus(AExpr lhs, AExpr rhs): {
      lhsCheckMsg = check(lhs, tenv, useDef);
      rhsCheckMsg = check(rhs, tenv, useDef);
      msgs += { error("Wrong lhs operand type for subtraction of Expr", lhs) | isEmpty(lhsCheckMsg) && typeOf(lhs, tenv, useDef) != tint()} + lhsCheckMsg;
      msgs += { error("Wrong rhs operand type for subtraction of Expr", rhs) | isEmpty(rhsCheckMsg) && typeOf(rhs, tenv, useDef) != tint()} + rhsCheckMsg;
    }
    case less(AExpr lhs, AExpr rhs): {
      lhsCheckMsg = check(lhs, tenv, useDef);
      rhsCheckMsg = check(rhs, tenv, useDef);
      msgs += { error("Wrong lhs operand type for less than operation of Expr", lhs) | isEmpty(lhsCheckMsg) && typeOf(lhs, tenv, useDef) != tint()} + lhsCheckMsg;
      msgs += { error("Wrong rhs operand type for less than operation of Expr", rhs) | isEmpty(rhsCheckMsg) && typeOf(rhs, tenv, useDef) != tint()} + rhsCheckMsg;
    }
    case lessEq(AExpr lhs, AExpr rhs): {
      lhsCheckMsg = check(lhs, tenv, useDef);
      rhsCheckMsg = check(rhs, tenv, useDef);
      msgs += { error("Wrong lhs operand type for less or equal to operation of Expr", lhs) | isEmpty(lhsCheckMsg) && typeOf(lhs, tenv, useDef) != tint()} + lhsCheckMsg;
      msgs += { error("Wrong rhs operand type for less or equal to operation of Expr", rhs) | isEmpty(rhsCheckMsg) && typeOf(rhs, tenv, useDef) != tint()} + rhsCheckMsg;
    }
    case equal(AExpr lhs, AExpr rhs): {
      lhsCheckMsg = check(lhs, tenv, useDef);
      rhsCheckMsg = check(rhs, tenv, useDef);
      msgs += { error("Wrong lhs operand type for equal to operation of Expr", lhs) | isEmpty(lhsCheckMsg) && typeOf(lhs, tenv, useDef) != tint()} + lhsCheckMsg;
      msgs += { error("Wrong rhs operand type for equal to operation of Expr", rhs) | isEmpty(rhsCheckMsg) && typeOf(rhs, tenv, useDef) != tint()} + rhsCheckMsg;
    }
    case notEqual(AExpr lhs, AExpr rhs): {
      lhsCheckMsg = check(lhs, tenv, useDef);
      rhsCheckMsg = check(rhs, tenv, useDef);
      msgs += { error("Wrong lhs operand type for not equal to operation of Expr", lhs) | isEmpty(lhsCheckMsg) && typeOf(lhs, tenv, useDef) != tint()} + lhsCheckMsg;
      msgs += { error("Wrong rhs operand type for not equal to operation of Expr", rhs) | isEmpty(rhsCheckMsg) && typeOf(rhs, tenv, useDef) != tint()} + rhsCheckMsg;
    }
    case greaterEq(AExpr lhs, AExpr rhs): {
      lhsCheckMsg = check(lhs, tenv, useDef);
      rhsCheckMsg = check(rhs, tenv, useDef);
      msgs += { error("Wrong lhs operand type for greater or equal to operation of Expr", lhs) | isEmpty(lhsCheckMsg) && typeOf(lhs, tenv, useDef) != tint()} + lhsCheckMsg;
      msgs += { error("Wrong rhs operand type for greater or equal to operation of Expr", rhs) | isEmpty(rhsCheckMsg) && typeOf(rhs, tenv, useDef) != tint()} + rhsCheckMsg;
    }
    case greater(AExpr lhs, AExpr rhs): {
      lhsCheckMsg = check(lhs, tenv, useDef);
      rhsCheckMsg = check(rhs, tenv, useDef);
      msgs += { error("Wrong lhs operand type for greater than operation of Expr", lhs) | isEmpty(lhsCheckMsg) && typeOf(lhs, tenv, useDef) != tint()} + lhsCheckMsg;
      msgs += { error("Wrong rhs operand type for greater than operation of Expr", rhs) | isEmpty(rhsCheckMsg) && typeOf(rhs, tenv, useDef) != tint()} + rhsCheckMsg;
    }
    case and(AExpr lhs, AExpr rhs): {
      lhsCheckMsg = check(lhs, tenv, useDef);
      rhsCheckMsg = check(rhs, tenv, useDef);
      msgs += { error("Wrong lhs operand type for AND operation of Expr", lhs) | isEmpty(lhsCheckMsg) && typeOf(lhs, tenv, useDef) != tbool()} + lhsCheckMsg;
      msgs += { error("Wrong rhs operand type for AND operation of Expr", rhs) | isEmpty(rhsCheckMsg) && typeOf(rhs, tenv, useDef) != tbool()} + rhsCheckMsg;
    }
    case or(AExpr lhs, AExpr rhs): {
      lhsCheckMsg = check(lhs, tenv, useDef);
      rhsCheckMsg = check(rhs, tenv, useDef);
      msgs += { error("Wrong lhs operand type for OR operation of Expr", lhs) | isEmpty(lhsCheckMsg) && typeOf(lhs, tenv, useDef) != tbool()} + lhsCheckMsg;
      msgs += { error("Wrong rhs operand type for OR operation of Expr", rhs) | isEmpty(rhsCheckMsg) && typeOf(rhs, tenv, useDef) != tbool()} + rhsCheckMsg;
    }
  }
  
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(str x, src = loc u)):  
      if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        return t;
      }
    case negative(AExpr exp):
      if(typeOf(exp, tenv, useDef) == tint()){
       return tint();
     }
    case not(AExpr exp):
      if(typeOf(exp, tenv, useDef) == tbool()) {
       return tbool();
     }
    case bracketExp(AExpr exp): return typeOf(exp, tenv, useDef);
    case multiply(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
       return tint();
     }
    case divide(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
       return tint();
     }
    case plus(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
       return tint();
     }
    case minus(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
       return tint();
     }
    case less(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
       return tbool();
     }
    case lessEq(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
       return tbool();
     }
    case equal(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
       return tbool();
    }
    case notEqual(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
       return tbool();
    }
    case greaterEq(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
       return tbool();
    }
    case greater(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tint() && typeOf(rhs, tenv, useDef) == tint()) {
       return tbool();
    }
    case and(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tbool() && typeOf(rhs, tenv, useDef) == tbool()) {
       return tbool();
    }
    case or(AExpr lhs, AExpr rhs):
      if(typeOf(lhs, tenv, useDef) == tbool() && typeOf(rhs, tenv, useDef) == tbool()) {
       return tbool();
    }
     case integer(int intValue): return tint();
     case boolean(bool boolValue): return tbool();
     case string(str strValue): return tstr();
  }
  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(str x, src = loc u), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 

