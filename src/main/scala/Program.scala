package institute.teias

import Synthesizer.Result

import z3.scala.Z3Context

import scala.collection.mutable

case class Program(instructions: List[Instruction]){
  def synthesize(context: Z3Context, spec: Specification, library: Library): Result[Program] = {
    val synthesizer = Synthesizer(context, library, spec)
    synthesizer.synthesize()
  }

  def dce(): Unit = {
    val used = mutable.HashSet.newBuilder[Id]
    for (inst <- instructions.reverseIterator) {
      if (used.result().contains(inst.result)) {
        inst.operator.operands(id => used += id)
      }
    }

    instructions.filter(inst => used.result().contains(inst.result))

    val renumbering = mutable.HashMap.newBuilder[Id, Id]
    for ((inst, i) <- instructions.zipWithIndex) {
      inst.operator.operandsMut(x => renumbering += (x -> (renumbering.result()(x))))
      val old = renumbering.result().put(inst.result, Id(i))
      assert(old.isEmpty)
      inst.result = Id(i)
    }
  }
}


