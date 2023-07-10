package institute.teias

import z3.scala.{Z3AST, Z3Context, Z3Sort}

case class Line(bitVector: Z3AST) {
  val context: Z3Context = bitVector.context
  def <(other: Line): Boolean = context.getBoolValue(context.mkBVUlt(bitVector, other.bitVector)).getOrElse(false)
  def <=(other: Line): Boolean = context.getBoolValue(context.mkBVUle(bitVector, other.bitVector)).getOrElse(false)
  def ===(other: Line): Boolean = context.getBoolValue(bitVector === other.bitVector).getOrElse(false)
  def !==(other: Line): Boolean = context.getBoolValue(bitVector !== other.bitVector).getOrElse(false)
}

object Line {
  def freshLine(context: Z3Context, name: String, lineBitWidth: Int): Line = {
    val sort: Z3Sort = context.mkBVSort(lineBitWidth)
    Line(context.mkFreshConst(name, sort))
  }

  def lineFromInt(context: Z3Context, line: Int, lineBitWidth: Int): Line = {
    val sort: Z3Sort = context.mkBVSort(lineBitWidth)
    Line(context.mkInt(line, sort))
  }
}
