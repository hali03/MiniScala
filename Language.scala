package project3

import scala.compiletime.uninitialized

/**
 * Definition of our target language.
 *
 * The different nodes of the AST also keep Position information
 * for error handling during the semantic analysis.
 *
 * TODO: Every time you add an AST node, you must also track the position
 */
abstract class Exp {
  var pos: Position = uninitialized
  var tp: Type = UnknownType

  def withPos(p: Position) = {
    pos = p
    this
  }

  def withType(pt: Type) = {
    tp = pt
    this
  }
}

abstract class Type
case object UnknownType extends Type
case class BaseType(v: String) extends Type {
  override def toString = v
}
case class FunType(args: List[(String, Type)], rtp: Type) extends Type {
  override def toString = s"(${args mkString ","}) => $rtp"
}
case class ArrayType(tp: Type) extends Type

val IntType = BaseType("Int")
val UnitType = BaseType("Unit")
val BooleanType = BaseType("Boolean")

// Arithmetic
case class Lit(x: Any) extends Exp
// CHANGED: instead of creating a node for different operator arity,
// we use a single node with a list of arguments.
case class Prim(op: String, args: List[Exp]) extends Exp

// Immutable variables
case class Let(x: String, xtp: Type, a: Exp, b: Exp) extends Exp
case class Ref(x: String) extends Exp

// Branches
case class If(cond: Exp, tBranch: Exp, eBranch: Exp) extends Exp

// Mutable variables
case class VarDec(x: String, xtp: Type, rhs: Exp, body: Exp) extends Exp
case class VarAssign(x: String, rhs: Exp) extends Exp

// While loops
case class While(cond: Exp, lbody: Exp, body: Exp) extends Exp

// Functions
case class LetRec(funs: List[Exp], body: Exp) extends Exp
case class Arg(name: String, tp: Type, pos: Position)
case class FunDef(name: String, args: List[Arg], rtp: Type, fbody: Exp) extends Exp
case class App(f: Exp, args: List[Exp]) extends Exp

// Arrays
case class ArrayDec(size: Exp, etp: Type) extends Exp
