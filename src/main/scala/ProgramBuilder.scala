package institute.teias

import scala.collection.mutable.ArrayBuffer

class ProgramBuilder {
  private val program: ArrayBuffer[Instruction] = ArrayBuffer.empty[Instruction]

  def finish(): Program = Program(program.toList)

  def var_(): Id = {
    assert(
      program.forall(_.operator == VAR),
      "All `var`s must be at the start of the program"
    )
    val result = nextId
    program += Instruction(result, VAR)
    result
  }

  def const_(c: Int): Id = {
    val result = nextId
    program += Instruction(result, CONST(c))
    result
  }

  def eqz(a: Id): Id = {
    val result = nextId
    program += Instruction(result, EQZ(a))
    result
  }

  def clz(a: Id): Id = {
    val result = nextId
    program += Instruction(result, CLZ(a))
    result
  }

  def ctz(a: Id): Id = {
    val result = nextId
    program += Instruction(result, CTZ(a))
    result
  }

  def popcnt(a: Id): Id = {
    val result = nextId
    program += Instruction(result, POPCNT(a))
    result
  }

  def eq(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, EQ(a, b))
    result
  }

  def ne(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, NE(a, b))
    result
  }

  def lt_s(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, LST(a, b))
    result
  }

  def lt_u(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, LTU(a, b))
    result
  }

  def gt_s(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, GTS(a, b))
    result
  }

  def gt_u(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, GTU(a, b))
    result
  }

  def le_s(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, LES(a, b))
    result
  }

  private def nextId: Id = Id(program.length)

  def le_u(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, LEU(a, b))
    result
  }

  def ge_s(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, GES(a, b))
    result
  }

  def ge_u(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, GEU(a, b))
    result
  }

  def add(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, ADD(a, b))
    result
  }

  def sub(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, SUB(a, b))
    result
  }

  def mul(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, MUL(a, b))
    result
  }

  def div_s(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, DIVS(a, b))
    result
  }

  def div_u(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, DIVU(a, b))
    result
  }

  def rem_s(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, REMS(a, b))
    result
  }

  def rem_u(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, REMU(a, b))
    result
  }

  def and(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, AND(a, b))
    result
  }

  def or(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, OR(a, b) )
    result
  }

  def xor(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, XOR(a, b))
    result
  }

  def shl(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, SHL(a, b))
    result
  }

  def shr_s(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, SHRS(a, b))
    result
  }

  def shr_u(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, SHRU(a, b))
    result
  }

  def rotl(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, ROTL(a, b))
    result
  }

  def rotr(a: Id, b: Id): Id = {
    val result = nextId
    program += Instruction(result, ROTR(a, b))
    result
  }

  def select(a: Id, b: Id, c: Id): Id = {
    val result = nextId
    program += Instruction(result, SELECT(a, b, c))
    result
  }
}
