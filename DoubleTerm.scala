package https://www.facebook.com/photo.php?fbid=10207696314588414&set=p.10207696314588414&type=3&theater 


import booleans.BooleanTerm
import functions._
import vectors.{VectorTerm, VectorScalarApp}
/**
 * @author Teino Boswell
 */

trait DoubleTerm extends BoundedTerm[Double] {
  def +(rhs: DoubleTerm) = AddApp(this, rhs)

  def *(rhs: DoubleTerm) = TimesApp(this, rhs)

  def *(rhs: VectorTerm) = VectorScalarApp(rhs, this)

  //def ground(env:Env) : DoubleTerm = super.ground(env).asInstanceOf[DoubleTerm]

  def ground(env: Env): DoubleTerm
}

case class AddApp(lhs: DoubleTerm, rhs: DoubleTerm) extends DoubleFunApp(FunApp(Constant(Add), lhs), rhs) {
  override def ground(env: Env) = AddApp(lhs.ground(env), rhs.ground(env))
}

case class TimesApp(lhs: DoubleTerm, rhs: DoubleTerm) extends DoubleFunApp(FunApp(Constant(Times), lhs), rhs) {
  override def upperBound = Math.max(lhs.upperBound * rhs.upperBound, 0.0)
}
case class Sum(override val args: Seq[DoubleTerm]) extends Fold[Double](Constant(Add), args, Constant(0.0))
  with DoubleTerm{
  def upperBound = args.foldLeft(0.0){(b,a)=> b + a.upperBound }

  override def ground(env: Env) = Sum(args.map(a => a.ground(env)))

}

object SumHelper {
  def sum(terms : Collection[DoubleTerm], env:Env) = terms.foldLeft(0.0) {(s,t) => s + env(t)}
}

case class Product(override val args: Seq[Term[Double]]) extends Fold[Double](Constant(Times), args, Constant(0))


case class QuantifiedSum[T](override val variable: Var[T], override val formula: DoubleTerm)
        extends Quantification(Constant(Add), variable, formula, Constant(0.0)) with DoubleTerm {
  override lazy val unroll = {
    val env = new MutableEnv
    Sum(variable.values.map(value => {env += variable -> value; formula.ground(env)}).toSeq)
  }
  def upperBound = unroll.upperBound
  override def ground(env: Env) = unroll.ground(env)

}
case class BoolToDoubleCast(boolTerm: BooleanTerm) extends FunApp(Constant(CastBoolToDouble), boolTerm)
        with DoubleTerm {
  def upperBound = if (boolTerm.upperBound) 1.0 else 0.0

  override def ground(env: Env) = BoolToDoubleCast(boolTerm.ground(env))
}

case class DoubleFunApp[T](override val function: Term[T => Double], override val arg: Term[T])
        extends FunApp(function, arg) with DoubleTerm {
  def upperBound = Math.POS_INF_DOUBLE

  override def ground(env: Env) = DoubleFunApp(function.ground(env),arg.ground(env))
}

case class DoubleConstant(override val value: Double) extends BoundedConstant(value) with DoubleTerm {
  //problem Constant superclass defines ground to return Constants

  override def ground(env: Env) = this
}

object Add extends (Double => (Double => Double)) {
  def apply(arg1: Double): (Double => Double) = (arg2: Double) => arg1 + arg2

  override def toString = "Add"
}

object Times extends (Double => (Double => Double)) {
  def apply(arg1: Double): (Double => Double) = (arg2: Double) => arg1 * arg2

  override def toString = "Times"
}

object CastBoolToDouble extends (Boolean => Double) {
  def apply(bool: Boolean) = if (bool) 1.0 else 0.0

  override def toString = "B2D"
}

Hide details
Change log
r658 by sebastian.riedel on Aug 15, 2009   Diff
- refactored package structure: now each
type has its own sub-package
Go to: 	
Project members, sign in to write a code review
Older revisions
All revisions of this file
File info
Size: 3043 bytes, 92 lines
View raw file
