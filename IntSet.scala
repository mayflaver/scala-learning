/**
  *  Created by wangfucong on 10/27/14.
  */
trait IntSet {
  def contain(x: Int): Boolean
  def add(x: Int): IntSet
}


object Empty extends IntSet {
  val element = null

  def contain(x: Int): Boolean = false

  def add(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def toString = ""
}


class NonEmpty(x: Int, left: IntSet, right: IntSet) extends IntSet {
  val element = x

  def contain(e: Int): Boolean = {
    if (e == x) true
    else if (e > x) right contain e
    else left contain e
  }

  def add(e: Int): IntSet = {
    if (x == e) this
    else if (x < e) new NonEmpty(x, left, right add e)
    else new NonEmpty(x, left add e, right)
  }

  override def toString = left.toString + " " + element.toString + " " + right.toString
}
