package institute.teias

sealed trait Operator {
  def arity: Int

  def immediates(f: Long => Unit): Unit

  def operands(f: Id => Unit): Unit

  def operandsMut(f: Id => Unit): Unit

  def toString: String

  def component: Component
}

// Unary operators
case class EQZ(id: Id) extends Operator {
  override val arity: Int = 1

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = f(id)

  override def operandsMut(f: Id => Unit): Unit = f(id)

  override def toString: String = s"eqz $id"

  override def component: Component = Components.eqz()
}

case class CLZ(id: Id) extends Operator {
  override val arity: Int = 1

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = f(id)

  override def operandsMut(f: Id => Unit): Unit = f(id)

  override def toString: String = s"clz $id"

  override def component: Component = Components.clz()
}

case class CTZ(id: Id) extends Operator {
  override val arity: Int = 1

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = f(id)

  override def operandsMut(f: Id => Unit): Unit = f(id)

  override def toString: String = s"ctz $id"

  override def component: Component = Components.ctz()
}

case class POPCNT(id: Id) extends Operator {
  override val arity: Int = 1

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = f(id)

  override def operandsMut(f: Id => Unit): Unit = f(id)

  override def toString: String = s"popcnt $id"

  override def component: Component = Components.popcnt()
}

// Binary relations
case class EQ(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"eq $a, $b"

  override def component: Component = Components.eq()
}

case class NE(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"ne $a, $b"

  override def component: Component = Components.ne()
}

case class LST(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"lt_s $a, $b"

  override def component: Component = Components.slt()
}

case class LTU(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"lt_u $a, $b"

  override def component: Component = Components.ult()
}

case class GTS(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"gt_s $a, $b"

  override def component: Component = Components.sgt()
}

case class GTU(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"gt_u $a, $b"

  override def component: Component = Components.ugt()
}

case class LES(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"le_s $a, $b"

  override def component: Component = Components.sle()
}

case class LEU(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"le_u $a, $b"

  override def component: Component = Components.ule()
}

case class GES(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"ge_s $a, $b"

  override def component: Component = Components.sge()
}

case class GEU(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"ge_u $a, $b"

  override def component: Component = Components.uge()
}

case class ADD(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"add $a, $b"

  override def component: Component = Components.add()
}

case class SUB(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"sub $a, $b"

  override def component: Component = Components.sub()

}

case class MUL(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"mul $a, $b"

  override def component: Component = Components.mul()
}

case class DIVS(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"div_s $a, $b"

  override def component: Component = Components.sdiv()
}

case class DIVU(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"div_u $a, $b"

  override def component: Component = Components.udiv()
}

case class REMS(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"rem_s $a, $b"

  override def component: Component = Components.srem()
}

case class REMU(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"rem_u $a, $b"

  override def component: Component = Components.urem()
}

case class AND(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"and $a, $b"

  override def component: Component = Components.and()
}

case class OR(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"or $a, $b"

  override def component: Component = Components.or()
}

case class XOR(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"xor $a, $b"

  override def component: Component = Components.xor()
}

case class SHL(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"shl $a, $b"

  override def component: Component = Components.shl()
}

case class SHRS(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"shr_s $a, $b"

  override def component: Component = Components.shrs()
}


case class SHRU(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"shr_u $a, $b"

  override def component: Component = Components.shrs()
}

case class ROTL(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"rotl $a, $b"

  override def component: Component = Components.rotl()
}

case class ROTR(a: Id, b: Id) extends Operator {
  override val arity: Int = 2

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
  }

  override def toString: String = s"rotr $a, $b"

  override def component: Component = Components.rotr()
}

// If-then-else
case class SELECT(a: Id, b: Id, c: Id) extends Operator {
  override val arity: Int = 3

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = {
    f(a)
    f(b)
    f(c)
  }

  override def operandsMut(f: Id => Unit): Unit = {
    f(a)
    f(b)
    f(c)
  }

  override def toString: String = s"select $a, $b, $c"

  override def component: Component = Components.select()
}

case object VAR extends Operator {
  override val arity: Int = 0

  override def immediates(f: Long => Unit): Unit = ()

  override def operands(f: Id => Unit): Unit = ()

  override def operandsMut(f: Id => Unit): Unit = ()

  override def toString: String = "var"

  override def component: Component = throw new IllegalStateException("Var has no components")
}

case class CONST(value: Int) extends Operator {
  override val arity: Int = 0

  override def immediates(f: Long => Unit): Unit = f(value)

  override def operands(f: Id => Unit): Unit = ()

  override def operandsMut(f: Id => Unit): Unit = ()

  override def toString: String = s"const 0x${value.toHexString}"

  override def component: Component = Components.const_(Some(value))
}

object Operator {
  implicit class DisplayOps(operator: Operator) {
    override def toString: String = operator match {
      case EQZ(id) => s"eqz $id"
      case CLZ(id) => s"clz $id"
      case CTZ(id) => s"ctz $id"
      case POPCNT(id) => s"popcnt $id"
      case EQ(a, b) => s"eq $a, $b"
      case NE(a, b) => s"ne $a, $b"
      case LST(a, b) => s"lt_s $a, $b"
      case LTU(a, b) => s"lt_u $a, $b"
      case GTS(a, b) => s"gt_s $a, $b"
      case GTU(a, b) => s"gt_u $a, $b"
      case LES(a, b) => s"le_s $a, $b"
      case LEU(a, b) => s"le_u $a, $b"
      case GES(a, b) => s"ge_s $a, $b"
      case GEU(a, b) => s"ge_u $a, $b"
      case ADD(a, b) => s"add $a, $b"
      case SUB(a, b) => s"sub $a, $b"
      case MUL(a, b) => s"mul $a, $b"
      case DIVS(a, b) => s"div_s $a, $b"
      case DIVU(a, b) => s"div_u $a, $b"
      case REMS(a, b) => s"rem_s $a, $b"
      case REMU(a, b) => s"rem_u $a, $b"
      case AND(a, b) => s"and $a, $b"
      case OR(a, b) => s"or $a, $b"
      case XOR(a, b) => s"xor $a, $b"
      case SHL(a, b) => s"shl $a, $b"
      case SHRS(a, b) => s"shr_s $a, $b"
      case SHRU(a, b) => s"shr_u $a, $b"
      case ROTL(a, b) => s"rotl $a, $b"
      case ROTR(a, b) => s"rotr $a, $b"
      case SELECT(a, b, c) => s"select $a, $b, $c"
      case VAR => "var"
      case CONST(value) => s"const 0x${value.toHexString}"
    }
  }
}
