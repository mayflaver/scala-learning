/**
  *  Created by wangfucong on 10/27/14.
  */
trait Expression {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(l, r) => l.eval + r.eval
    case Product(l, r) => l.eval * r.eval
  }
}

case class Number(n: Int) extends Expression
case class Sum(l: Expression, r: Expression) extends Expression
case class Product(l: Expression, r: Expression) extends Expression
