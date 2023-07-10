package institute.teias
import Components.Z3ContextExtensions

import z3.scala._

trait Specification {
  def arity(): Int
  def makeExpression(context: Z3Context, inputs: Seq[Z3AST], output: Z3AST, bitWidth: Int): Z3AST
}

class ProgramSpecification(val program: Program) extends Specification {
  def arity(): Int = {
    program.instructions.takeWhile(_.operator == VAR).length
  }

  def makeExpression(context: Z3Context, inputs: Seq[Z3AST], output: Z3AST, bitWidth: Int): Z3AST = {
    assert(program.instructions.length > inputs.length)

    var vars = inputs.toList

    val operands = collection.mutable.Buffer[Z3AST]()
    for (instr <- program.instructions.drop(inputs.length)) {
      // programs cannot contain unbound constants, so specifications
      // constructed from programs will never require us to synthesize an
      // immediate.
      val immediates = Seq[Z3AST]()

      operands.clear()
      instr.operator.operands((id: Id) => operands += vars(id.value))

      val newVar = instr.operator.component.makeExpression(context, immediates, operands, bitWidth)
      vars = vars :+ newVar
    }

    context.mkBool(vars.last.equals(output))
  }
}
