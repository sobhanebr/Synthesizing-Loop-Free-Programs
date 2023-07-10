package institute.teias
import Components.Z3ContextExtensions

import z3.scala.{Z3AST, Z3Context}



trait Component {
  def operandArity: Int

  def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator

  def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST

  def immediateArity: Int = 0
}

case class Const(value: Option[Int]) extends Component {
  override def operandArity: Int = 0

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = {
    value match {
      case Some(v) => CONST(v)
      case None => CONST(immediates.head)
    }
  }

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    value match {
      case Some(v) => context.mkBV(v, bitWidth)
      case None => immediates.head
    }
  }

  override def immediateArity: Int = if (value.isDefined) 0 else 1
}

case object Eqz extends Component {
  override def operandArity: Int = 1

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = EQZ(operands.head)

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkITE(
      context.mkBool(operands.head.eq(context.mkBV(0, bitWidth))),
      context.mkBV(1, bitWidth),
      context.mkBV(0, bitWidth)
    )

  }
}

case object Clz extends Component {
  override def operandArity: Int = 1

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = CLZ(operands.head)

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    def clz(input: Z3AST, oneBit: Z3AST, i: Int): Z3AST = {
      if (i == bitWidth) context.mkBV(i, bitWidth)
      else context.mkITE(context.mkBool(context.mkExtract(bitWidth - 1 - i, bitWidth - 1 - i, input).eq(oneBit)),
        context.mkBV(i, bitWidth),
        clz(input, oneBit, i + 1)
      )

    }

    val oneBit = context.mkBV(1, 1)
    clz(operands.head, oneBit, 0)
  }
}

case object Ctz extends Component {
  override def operandArity: Int = 1

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = CTZ(operands.head)

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    val zero = context.mkBV(0, bitWidth)
    val one = context.mkBV(1, bitWidth)

    val lsb = context.mkExtract(0, 0, operands.head)
    val eqZero = context.mkBool(lsb.eq(zero))

    context.mkITE(eqZero, one, zero)
  }
}

case object Popcnt extends Component {
  override def operandArity: Int = 1

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = POPCNT(operands.head)

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    val operand = operands.head

    val bits = (0 until bitWidth).map { i =>
      context.mkZeroExt(bitWidth - 1, context.mkExtract(i, i, operand))
    }
    bits.reduce((a, b) => context.mkAdd(a, b))
  }
}


case object Rotl extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = ROTL(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVOr(
      context.mkBVShl(operands.head, operands(1)),
      context.mkBVLshr(operands.head, context.mkBVSub(context.mkBV(bitWidth, bitWidth), operands(1)))
    )
  }
}

case object Rotr extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = ROTR(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVOr(
      context.mkBVLshr(operands.head, operands(1)),
      context.mkBVShl(operands.head, context.mkBVSub(context.mkBV(bitWidth, bitWidth), operands(1)))
    )
  }
}

case object Add extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = ADD(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVAdd(operands.head, operands(1))
  }
}

case object Sub extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = SUB(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVSub(operands.head, operands(1))
  }
}

case object Mul extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = MUL(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVMul(operands.head, operands(1))
  }
}

case object Udiv extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = DIVU(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVUdiv(operands.head, operands(1))
  }
}

case object Sdiv extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = DIVS(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVSdiv(operands.head, operands(1))
  }
}

case object Urem extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = REMU(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVUrem(operands.head, operands(1))
  }
}

case object Srem extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = REMS(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVSrem(operands.head, operands(1))
  }
}

case object And extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = AND(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVAnd(operands.head, operands(1))
  }
}

case object Or extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = OR(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVOr(operands.head, operands(1))
  }
}

case object Xor extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = XOR(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVXor(operands.head, operands(1))
  }
}

case object Shl extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = SHL(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVShl(operands.head, operands(1))
  }
}

case object ShrU extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = SHRU(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVLshr(operands.head, operands(1))
  }
}

case object ShrS extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = SHRS(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVAshr(operands.head, operands(1))
  }
}

case object Eq extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = EQ(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkITE(context.mkBool(operands.head.eq(operands(1))), context.mkBV(1, bitWidth), context.mkBV(0, bitWidth))
  }
}

case object Ne extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = NE(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkITE(context.mkBool(operands.head.eq(operands(1))), context.mkBV(0, bitWidth), context.mkBV(1, bitWidth))
  }
}

case object Ult extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = LTU(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVUlt(operands.head, operands(1))
  }
}

case object Ule extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = LEU(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVUle(operands.head, operands(1))
  }
}

case object Ugt extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = GTU(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVUgt(operands.head, operands(1))
  }
}

case object Uge extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = GEU(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVUge(operands.head, operands(1))
  }
}

case object Slt extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = LST(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVSlt(operands.head, operands(1))
  }
}

case object Sle extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = LES(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVSle(operands.head, operands(1))
  }
}

case object Sgt extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = GTS(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVSgt(operands.head, operands(1))
  }
}

case object Sge extends Component {
  override def operandArity: Int = 2

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = GES(operands.head, operands(1))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkBVSge(operands.head, operands(1))
  }
}

case object Select extends Component {
  override def operandArity: Int = 3

  override def makeOperator(immediates: Seq[Int], operands: Seq[Id]): Operator = SELECT(operands.head, operands(1), operands(2))

  override def makeExpression(context: Z3Context, immediates: Seq[Z3AST], operands: Seq[Z3AST], bitWidth: Int): Z3AST = {
    context.mkITE(
      operands.head,
      operands(1),
      operands(2)
    )
  }
}




object Components {
  def const_(value: Option[Int]): Component = Const(value)

  def const_(value: Int): Component = Const(Some(value))

  def eqz(): Component = Eqz

  def clz(): Component = Clz

  def add(): Component = Add

  def sub(): Component = Sub

  def mul(): Component = Mul

  def udiv(): Component = Udiv

  def sdiv(): Component = Sdiv

  def urem(): Component = Urem

  def srem(): Component = Srem

  def and(): Component = And

  def or(): Component = Or

  def xor(): Component = Xor

  def ctz(): Component = Ctz

  def rotl(): Component = Rotl

  def rotr(): Component = Rotr

  def shl(): Component = Shl

  def shru(): Component = ShrU

  def shrs(): Component = ShrS

  def eq(): Component = Eq

  def ne(): Component = Ne

  def ult(): Component = Ult

  def ule(): Component = Ule

  def ugt(): Component = Ugt

  def uge(): Component = Uge

  def slt(): Component = Slt

  def sle(): Component = Sle

  def sgt(): Component = Sgt

  def sge(): Component = Sge

  def popcnt(): Component = Popcnt

  def select(): Component = Select

  implicit class Z3ContextExtensions(val z3: Z3Context) extends AnyVal {
    def mkBV(value: Int, size: Int): Z3AST = {
      z3.mkInt(value, z3.mkBVSort(size))
    }
    def mkBool(value: Boolean): Z3AST = {
      z3.mkBoolSort()
      if (value) z3.mkTrue()
      else z3.mkFalse()
    }

  }
}


