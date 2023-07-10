package institute.teias

case class Assignments(
                        immediates: Seq[Int],
                        params: Seq[Int],
                        results: Seq[Int],
                        output: Int
                      ) {
  def toProgram(numInputs: Int, library: Library): Program = {
    val builder = new ProgramBuilder()
    for (_ <- 0 until numInputs) {
      builder.var_()
    }
    val program = builder.finish()

    val immediateIterator = immediates.iterator
    val paramsIterator = params.iterator.map(Id)

    val instructions = results.zip(library.components).map {
      case (n, component) =>
        val immArity = component.immediateArity
        val immediates = immediateIterator.take(immArity).toSeq

        val opArity = component.operandArity
        val operands = paramsIterator.take(opArity).toSeq
        assert(operands.forall(_.value < n))

        val operator = component.makeOperator(immediates, operands)
        val result = Id(n)
        Instruction(result, operator)
    }
    val sortedInstructions = instructions.sortBy(_.result.value)
    val truncatedInstructions = sortedInstructions.take(output + 1)

    program.copy(instructions = truncatedInstructions.toList)
  }
}
