package institute.teias

import Components.Z3ContextExtensions
import Line.lineFromInt
import Synthesizer.{IntIsPowerOfTwo, Result}

import z3.scala.dsl.z3ASTToIntOperand
import z3.scala.{Z3AST, Z3Context, Z3Model}

import java.time.Duration
import scala.collection.immutable.Range
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.language.implicitConversions
import scala.util.{Success, Try}



case class Synthesizer(
                        context: Z3Context,
                        library: Library,
                        spec: Specification
                      ) {

  private val locations: LocationVars = LocationVars(context, library, spec.arity())
  private var wellFormedProgram: Boolean = _
  private var invalidConnections: mutable.HashSet[(Int, Int)] = _
  private var notInvalidAssignments: Boolean = _
  private var shouldSynthesizeMinimalPrograms: Boolean = false
  private var timeout: Option[Timeout] = None

  val FULL_BIT_WIDTH: Int = 32

  def shouldSynthesizeMinimalPrograms(should: Boolean): Unit = {
    shouldSynthesizeMinimalPrograms = should
  }

  def setTimeout(milliseconds: Option[Int]): Unit = {
    timeout = milliseconds.map(ms => DurationTimeout(Duration.ofMillis(ms)))
  }

  def and(exprs: Seq[Boolean]): Boolean = {
    if (exprs == null || exprs.isEmpty) return true
    context.getBoolValue(context.mkAnd(exprs.map(context.mkBool(_)): _*)).getOrElse(false)
  }

  def or(exprs: Seq[Boolean]): Boolean = {
    if (exprs == null || exprs.isEmpty) return true
    context.getBoolValue(context.mkOr(exprs.map(context.mkBool(_)): _*)).getOrElse(false)
  }

  def freshImmediate(context: Z3Context, bitWidth: Int): Z3AST = {
    context.mkFreshConst("immediate", context.mkBVSort(bitWidth))
  }

  def freshParam(context: Z3Context, bitWidth: Int): Z3AST = {
    context.mkFreshConst("param", context.mkBVSort(bitWidth))
  }

  def freshResult(context: Z3Context, bitWidth: Int): Z3AST = {
    context.mkFreshConst("result", context.mkBVSort(bitWidth))
  }

  def freshInput(context: Z3Context, bitWidth: Int): Z3AST = {
    context.mkFreshConst("input", context.mkBVSort(bitWidth))
  }

  def freshOutput(context: Z3Context, bitWidth: Int): Z3AST = {
    context.mkFreshConst("output", context.mkBVSort(bitWidth))
  }

  def evalZ3AST(model: Z3Model, bv: Z3AST): Int = {
    model.evalAs[Int](bv).getOrElse(0)
  }

  def evalZ3ASTs(model: Z3Model, bvs: Seq[Z3AST]): Seq[Int] = {
    bvs.map(evalZ3AST(model, _))
  }

  def evalLine(model: Z3Model, line: Z3AST): Int = {
    evalZ3AST(model, line).toInt
  }

  def evalLines(model: Z3Model, lines: Seq[Z3AST]): Seq[Int] = {
    lines.map(evalLine(model, _))
  }

  import z3.scala._

  def connectivity(
                    inputs: Seq[Z3AST],
                    output: Z3AST,
                    params: Seq[Z3AST],
                    results: Seq[Z3AST]
                  ): Boolean = {
    val locsToVars = Seq.newBuilder[(Z3AST, Z3AST)]
    locsToVars ++= this.locations.inputs.zip(inputs)
    locsToVars ++= this.locations.params.zip(params)
    locsToVars ++= this.locations.results.zip(results)
    locsToVars += (this.locations.output -> output)

    val conn = Seq.newBuilder[Boolean]
    for {
      (l_x, x) <- locsToVars.result().zipWithIndex
      (l_y, y) <- locsToVars.result().zipWithIndex.drop(x + 1)
      if !this.isInvalidConnection(x, y)
    } {
      conn += context.getBoolValue(context.mkImplies(l_x._1, this.context.mkBool(x == y))).getOrElse(false)
    }

    and(conn.result())
  }



  private def solver: Z3Solver = {
    val solver = context.mkSolver

    timeout.foreach {
      case DurationTimeout(d) =>
        val millis = d.toMillis.toInt
        timeout = Some(InstantTimeout(java.time.Instant.now().plusMillis(d.toMillis)))
        val params = Map[String, Any](("timeout", millis))
        solver.set(params)

      case InstantTimeout(instant) =>
        val dur = java.time.Duration.between(java.time.Instant.now(), instant)
        val millis = dur.toMillis.toInt
        timeout = Some(InstantTimeout(instant))
        val params = Map[String, Any](("timeout", millis))
        solver.set(params)
    }

    solver
  }

  private def isInvalidConnection(i: Int, j: Int): Boolean = {
    assert(i < locations.inputs.length + locations.params.length + locations.results.length + 1) // +1 for output
    assert(j < locations.inputs.length + locations.params.length + locations.results.length + 1) // +1 for output
    invalidConnections.contains((i, j)) || invalidConnections.contains((j, i))
  }

  private def freshImmediates(bitWidth: Int): Seq[Z3AST] = {
    library.components.flatMap { c =>
      (0 until c.immediateArity).map(_ => freshImmediate(context, bitWidth))
    }
  }

  private def freshParamVars(bitWidth: Int): Seq[Z3AST] = {
    library.components.flatMap { c =>
      (0 until c.operandArity).map(_ => freshParam(context, bitWidth))
    }
  }

  private def freshResultVars(bitWidth: Int): Seq[Z3AST] = {
    library.components.map(_ => freshResult(context, bitWidth))
  }

  private def addInvalidAssignment(assignments: Assignments): Unit = {
    val params = and(assignments.params.zip(locations.params).map {
      case (assignment, location) =>
        location.equals( lineFromInt(context, assignment, locations.lineBitWidth).bitVector)
    })

    val results = and(assignments.results.zip(locations.results).map {
      case (assignment, location) =>
        location.equals( lineFromInt(context, assignment, locations.lineBitWidth).bitVector)
    })


    val notThisAssignment = and(List(results, params).map(ast => !ast))
    notInvalidAssignments = notInvalidAssignments && notThisAssignment
  }

  private def resetInvalidAssignments(): Unit = {
    notInvalidAssignments = true
  }

  def finiteSynthesis(
                       inputs: mutable.HashSet[Seq[Int]],
                       outputLine: Int,
                       bitWidth: Int
                     ): Result[Assignments] = {
    println(
      s"finite synthesis at bit width $bitWidth with inputs = ${inputs.map(_.map(_.toHexString))}"
    )

    val worksForInputs = mutable.ArrayBuffer[Z3AST]()

    for (input <- inputs) {
      val params = freshParamVars(bitWidth)
      val results = freshResultVars(bitWidth)
      val z3Inputs = input.map(i => context.mkBV(i, bitWidth))
      val output = freshOutput(context, bitWidth)

      val lib = context.mkBool(connectivity(z3Inputs, output, params, results))
      worksForInputs += lib

      val conn = context.mkBool(connectivity(z3Inputs, output, params, results))
      worksForInputs += conn

      val spec = this.spec.makeExpression(context, z3Inputs, output, bitWidth)
      worksForInputs += spec
    }

    val worksForInputsArray = worksForInputs.toArray

    assert(spec.arity <= outputLine)
    assert(outputLine < spec.arity + library.components.length)
    val outputOnLine = locations.output === lineFromInt(context, outputLine, bitWidth).bitVector
    val query = context.mkAnd((Array(context.mkBool(wellFormedProgram) , outputOnLine , context.mkBool(notInvalidAssignments)) ++ worksForInputsArray) : _*)
    println(s"finite synthesis query =\n$query")

    val solver = context.mkSolver()
    solver.assertCnstr(query)

    solver.check() match {
      case Some(false) => Left(SynthesisUnsatisfiable)
      case Some(true) =>
        val model = solver.getModel()
        val immediates = evalZ3ASTs(model, freshImmediates(bitWidth))
        val params = evalLines(model, locations.params)
        val results = evalLines(model, locations.results)

        val assignments = Assignments(immediates, params, results, outputLine)
        println(
          s"finite synthesis generated:\n${assignments.toProgram(spec.arity(), library)}"
        )
        Right(Success(assignments))
      case None => Left(SynthesisUnknown)
    }
  }

  private def verification(assignments: Assignments, bitWidth: Int): Result[Verification] = {
    val inputs = Seq.fill(spec.arity())(freshInput(context, bitWidth))
    val output = freshOutput(context, bitWidth)
    val program: Program = assignments.toProgram(spec.arity(), library)
    program.dce()

    val progExpr = new ProgramSpecification(program).makeExpression(context, inputs, output, bitWidth)
    val specExpr = spec.makeExpression(context, inputs, output, bitWidth)
    val notSpecExpr = context.mkNot(specExpr)
    val query = context.mkAnd(progExpr, notSpecExpr)
    println("verification query =\n" + query)

    val solver = this.solver
    solver.assertCnstr(query)

    solver.check() match {
      case Some(false) =>
        println(s"verified to work for all inputs at bit width = $bitWidth")
        Right(Success(WorksForAllInputs))
      case Some(true) =>
        val model = solver.getModel()
        addInvalidAssignment(assignments)
        val z3inputs = evalZ3ASTs(model, inputs)
        println("found a counter-example: " + z3inputs.map(_.toLong.toHexString))
        Right(Success(Counterexample(z3inputs)))
      case None => Left(SynthesisUnknown)
    }
  }


  def synthesize(): Result[Program] = {
    val inputs = initialConcreteInputs()
    inputs match {
      case Left(error) => return Left(error)
      case Right(input) => assert(input.isSuccess && input.get.nonEmpty)
    }

    val arity = spec.arity()
    assert(arity > 0)

    val longest = arity + library.components.length
    val shortest = if (shouldSynthesizeMinimalPrograms) arity + 1 else longest

    var best: Result[Program] = Left(SynthesisUnknown)
    var length = longest
    while (length >= shortest) {
      synthesizeWithLength(length, inputs.right.get.get) match {
        case Right(Success(program)) =>
          program.dce()
          assert(program.instructions.length > arity)
          length = program.instructions.length - 1
          best = Right(Success(program))
          resetInvalidAssignments()

        case Left(err) =>
          best match {
            case Left(_) => return Left(err)
            case Right(Success(program)) => return Right(Success(program))
          }
      }
    }
    best
  }

  private def synthesizeWithLength(programLength: Int, inputs: mutable.HashSet[Seq[Int]]): Result[Program] = {
    println(s"synthesizing a program of length = $programLength")

    var bitWidth = 2
    var verifyingWithMoreBits = false
    while (true) {
      finiteSynthesis(inputs, programLength - 1, bitWidth) match {
        case Right(Success(assignments)) =>
          while (true) {
            println(s"verifying at bit width = $bitWidth")
            verification(assignments, bitWidth) match {
              case Right(Success(WorksForAllInputs)) =>
                assert(bitWidth <= FULL_BIT_WIDTH)
                assert(bitWidth.isPowerOfTwo)
                if (bitWidth == FULL_BIT_WIDTH) {
                  return Right(Success(assignments.toProgram(spec.arity(), library)))
                } else {
                  bitWidth *= 2
                  verifyingWithMoreBits = true
                  // TODO: If the synthesized assignments use immediate constants,
                  //  try to extend the constants for the wider bit width in various ways.
                }

              case Right(Success(Counterexample(newInputs))) =>
                val isNew = inputs.add(newInputs)
                assert(isNew || verifyingWithMoreBits)

              case Left(error: Error) => return Left(error)
            }
          }

        case Left(error: Error) => return Left(error)
      }
    }

    Left(SynthesisUnknown)
  }

  def initialConcreteInputs(): Result[mutable.HashSet[Seq[Int]]] = {
    // Taken from Souper.
    val NUM_INITIAL_INPUTS: Int = 4

    val inputs: mutable.HashSet[Seq[Int]] = mutable.HashSet.empty[Seq[Int]]

    val inputVars: Seq[Z3AST] = (0 until this.spec.arity).map(_ => freshInput(context, FULL_BIT_WIDTH))
    val outputVar = freshOutput(context, FULL_BIT_WIDTH)
    val spec = this.spec.makeExpression(context, inputVars, outputVar, FULL_BIT_WIDTH)

    for (_ <- 0 until NUM_INITIAL_INPUTS) {
      // Make sure that we don't find the same concrete inputs that we've already found.
      val existingInputs = inputs.map { inputSet =>
        val thisInput = inputSet.zip(inputVars).map {
          case (inp, varAst) =>
            context.mkInt(inp, context.mkBVSort(FULL_BIT_WIDTH) ).equals(varAst)
        }
        and(thisInput)
      }
      val existingInputsAST = or(existingInputs.toSeq)
      val notExistingInputs = context.mkBool(!existingInputsAST)

      val query = context.mkAnd(Array(spec, notExistingInputs) : _*)
      println("initial concrete input synthesis query =\n" + query)

      val solver = context.mkSolver()
      solver.assertCnstr(query)

      solver.check() match {
        case Some(true) =>
          val model = solver.getModel()
          val newInputs = evalZ3ASTs(model, inputVars)
          inputs.add(newInputs)
        case Some(false) =>
          return Left(SynthesisUnsatisfiable)
        case None =>
          return Left(SynthesisUnknown)
      }
    }

    Right(Success(inputs))
  }


  case class LocationVars(context: Z3Context, library: Library, numInputs: Int) {
    val lineBitWidth: Int = calculateLineBitWidth(numInputs, library.components.length)
    val inputs: immutable.Seq[Z3AST] = (0 until numInputs).map(_ => freshLine(context, "input_location", lineBitWidth))
    val params: Seq[Z3AST] = library.components.flatMap(c => (0 until c.operandArity)
      .map(_ => freshLine(context, "param_location", lineBitWidth)))
    val results: Seq[Z3AST] = library.components.map(_ => freshLine(context, "result_location", lineBitWidth))
    val output: Z3AST = freshLine(context, "output_line", lineBitWidth)


    private def freshLine(context: Z3Context, name: String, lineBitWidth: Int): Z3AST = {
      context.mkFreshConst(name, context.mkBVSort(lineBitWidth))
    }

    def nextPowerOfTwo(value: Int): Int = {
      var n = value - 1
      n |= n >> 1
      n |= n >> 2
      n |= n >> 4
      n |= n >> 8
      n |= n >> 16
      n + 1
    }

    implicit def trailingZeros(value: Int): Int = Integer.numberOfTrailingZeros(value)


    private def calculateLineBitWidth(numInputs: Int, numComponents: Int): Int = {
      val maxLine = numInputs + numComponents
      val maxPow2 = nextPowerOfTwo((maxLine + 1))
      trailingZeros(maxPow2)
    }

    def lineFromUInt(context: Z3Context, line: Int): Z3AST = {
      assert(line < (1 << lineBitWidth))
      context.mkInt(line, context.mkBVSort(lineBitWidth))
    }

    def inputsRange: Range = inputs.indices

    def paramsRange: Range = inputs.length until (inputs.length + params.length)

    def resultsRange: Range = (inputs.length + params.length) until (inputs.length + params.length + results.length)

    def paramsIterator: Iterator[Line] =
      Iterator.range(inputs.length, inputs.length + params.length).map(lineBitWidth => Line(freshParam(context, lineBitWidth)))

    def resultsIterator: Iterator[Line] =
      Iterator.range(inputs.length + params.length, inputs.length + params.length + results.length).map(lineBitWidth => Line(freshParam(context, lineBitWidth)))

    def outputRange: Range = (inputs.length + params.length + results.length) until (inputs.length + params.length + results.length + 1)

    def invalidConnections(library: Library): Set[(Int, Int)] = {
      val invalidConnections = mutable.HashSet[(Int, Int)]()

      // We will never assign the output directly to an input.
      for (a <- inputsRange; b <- outputRange) {
        invalidConnections += ((a, b))
      }

      // We never assign an input's location to another input's location.
      for {
        (a, i) <- inputsRange.zipWithIndex
        b <- inputsRange.drop(i + 1)
      } {
        invalidConnections += ((a, b))
      }

      // Similarly, a well-formed program will never assign a param's location as another param;
      // it should only be one of the original inputs or the result of another component.
      for {
        (p, i) <- paramsRange.zipWithIndex
        q <- paramsRange.drop(i + 1)
      } {
        invalidConnections += ((p, q))
      }

      // Finally, a well-formed program will never have a component with its own result as a parameter.
      val params = mutable.Queue(paramsRange: _*)
      for {
        (r, c) <- resultsRange.zip(library.components)
        _ <- 0 until c.operandArity
        p = params.dequeue()
      } {
        invalidConnections += ((r, p))
      }

      invalidConnections.toSet
    }

    def wellFormedProgram(context: Z3Context, library: Library): Z3AST = {
      val wfp = mutable.ArrayBuffer[Z3AST]()

      wfp += context.mkBool(consistent)
      wfp += context.mkBool(acyclic(context, library))

      val iLen = lineFromUInt(context, inputs.length)
      val m = lineFromUInt(context, inputs.length + results.length)
      val zero: Z3AST = lineFromUInt(context, 0)

      for ((l, i) <- inputs.zip(inputsRange)) {
        val iLine = lineFromUInt(context, i)
        wfp += l === iLine
      }

      for (l <- params) {
        // 0 <= l
        wfp += context.mkBVUle(zero, l)
        // l < M
        wfp += context.mkBVUlt(l, m)

      }

      for (l <- results) {
        // |i| <= l
        wfp += context.mkBVUle(iLen, l)
        // l < m
        wfp += context.mkBVUlt(l, m)
      }

      context.mkAnd(wfp: _*)
    }

    def consistent: Boolean = {
      val cons = scala.collection.mutable.ListBuffer[Boolean]()
      for ((x, i) <- results.iterator.zip(resultsRange.iterator)) {
        for ((y, j) <- results.iterator.zip(resultsRange.iterator).drop(i + 1)) {
          cons += !x.equals(y)
        }
      }
      and(cons.toList)
    }

    def acyclic(context: Z3Context, library: Library): Boolean = {
      val acycs = new ArrayBuffer[Boolean]()
      val params = paramsIterator
      val results = resultsIterator.toList

      for {
        c <- library.components
        resultLocation <- results
        _ <- 0 until c.operandArity
      } {
        val paramLocation = params.next()
        acycs += line_lt(paramLocation, resultLocation)
      }

      and(acycs)
    }
  }

  def line_lt(lhs: Line, rhs: Line): Boolean = {
    lhs < (rhs)
  }


}


object Synthesizer{
  type Result[T] = scala.util.Either[Error, Try[T]]

  implicit class IntIsPowerOfTwo(val number: Int) extends AnyVal {
    def isPowerOfTwo: Boolean = {
      (number != 0) && ((number & (number - 1)) == 0)
    }
  }
}




