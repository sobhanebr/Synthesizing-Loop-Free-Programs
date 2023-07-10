package institute.teias
package example

import Synthesizer.Result

import z3.scala.Z3Context

import scala.collection.mutable.ListBuffer
import scala.util.Success

object Main {

  def main(args: Array[String]): Unit ={
    val context = new Z3Context()
    val stringToFunction: Map[String, (Z3Context, Options) => Result[Program]] = Map(
      "p1" -> p1,
      "p2" -> p2,
      "p3" -> p3,
      "p4" -> p4,
      "p5" -> p5,
      "p6" -> p6,
      "p7" -> p7,
      "p8" -> p8,
      "p9" -> p9,
      "p10" -> p10
    )
    val problems: List[(String, (Z3Context, Options) => Result[Program])] = benchmarks(stringToFunction.toSeq : _*)

    val opts = Options(timeout = Some(1800000), minimal = false, onlyFast = false, synthesizeConstants = true, problems = stringToFunction.keys.toList.sorted)


    for ((name, p) <- problems) {
        println(s"==================== $name ====================")
        val then = System.nanoTime()
        val programRes: Result[Program] = p(context, opts)
        val elapsed = (System.nanoTime() - then) / 1000000.0

        println(s"\nElapsed: $elapsed ms\n")
      programRes match {
        case Left(e) => println(s"Error: $e\n")
        case Right(Success(prog)) => println(s"Synthesized:\n\n$prog")
      }
    }
  }

  def benchmarks(benchmarkList: (String, (Z3Context, Options) => Result[Program])*): List[(String, (Z3Context, Options) => Result[Program])] =
    benchmarkList.toList
  def p1(context: Z3Context, opts: Options): Result[Program] = {
    val componentList = ListBuffer[Component]()
    componentList.appendAll(Library.brahmaStd().components)
    componentList += Components.and()
    componentList += Components.const_(if (opts.synthesizeConstants) None else Some(1))

    val builder = new ProgramBuilder()
    val a = builder.var_()
    val b = builder.const_(1)
    val c = builder.sub(a, b)
    builder.and(a, c)
    val spec = new ProgramSpecification(builder.finish())

    synthesize(opts, context, spec, Library(componentList))
  }


  def p2(context: Z3Context, opts: Options): Result[Program] = {
    val componentList = ListBuffer[Component]()
    componentList.appendAll(Library.brahmaStd().components)
    componentList += Components.and()
    componentList += Components.const_(if (opts.synthesizeConstants) None else Some(1))

    val builder = new ProgramBuilder()
    val a = builder.var_()
    val b = builder.const_(1)
    val c = builder.add(a, b)
    builder.and(a, c)
    val spec = new ProgramSpecification(builder.finish())

    synthesize(opts, context, spec, Library(componentList.toList))
  }

  def p3(context: Z3Context, opts: Options): Result[Program] = {
    val componentList = ListBuffer[Component]()
    componentList.appendAll(Library.brahmaStd().components)

    val builder = new ProgramBuilder()
    val a = builder.var_()
    val b = builder.const_(0)
    val c = builder.sub(b, a)
    builder.and(a, c)
    val spec = new ProgramSpecification(builder.finish())

    synthesize(opts, context, spec, Library(componentList.toList))
  }

  def p4(context: Z3Context, opts: Options): Result[Program] = {
    val componentList = ListBuffer[Component]()
    componentList.appendAll(Library.brahmaStd().components)
    componentList += Components.and()
    componentList += Components.const_(if (opts.synthesizeConstants) None else Some(1))

    val builder = new ProgramBuilder()
    val a = builder.var_()
    val b = builder.const_(1)
    val c = builder.sub(a, b)
    builder.xor(a, c)
    val spec = new ProgramSpecification(builder.finish())

    synthesize(opts, context, spec, Library(componentList.toList))
  }

  def p5(context: Z3Context, opts: Options): Result[Program] = {
    val componentList = ListBuffer[Component]()
    componentList.appendAll(Library.brahmaStd().components)
    componentList += Components.and()
    componentList += Components.const_(if (opts.synthesizeConstants) None else Some(1))

    val builder = new ProgramBuilder()
    val a = builder.var_()
    val b = builder.const_(1)
    val c = builder.sub(a, b)
    builder.or(a, c)
    val spec = new ProgramSpecification(builder.finish())

    synthesize(opts, context, spec, Library(componentList.toList))
  }

  def p6(context: Z3Context, opts: Options): Result[Program] = {
    val componentList = ListBuffer[Component]()
    componentList.appendAll(Library.brahmaStd().components)
    componentList += Components.and()
    componentList += Components.const_(if (opts.synthesizeConstants) None else Some(1))

    val builder = new ProgramBuilder()
    val a = builder.var_()
    val b = builder.const_(1)
    val c = builder.add(a, b)
    builder.or(a, c)
    val spec = new ProgramSpecification(builder.finish())

    synthesize(opts, context, spec, Library(componentList.toList))
  }

  def p7(context: Z3Context, opts: Options): Result[Program] = {
    val componentList = ListBuffer[Component]()
    componentList.appendAll(Library.brahmaStd().components)
    componentList += Components.and()
    componentList += Components.const_(if (opts.synthesizeConstants) None else Some(1))
    componentList += Components.const_(if (opts.synthesizeConstants) None else Some(Int.MaxValue))

    val builder = new ProgramBuilder()
    val x = builder.var_()
    val a = builder.const_(Int.MaxValue)
    val o1 = builder.xor(x, a)
    val b = builder.const_(1)
    val o2 = builder.add(x, b)
    builder.and(o1, o2)
    val spec = new ProgramSpecification(builder.finish())

    synthesize(opts, context, spec, Library(componentList.toList))
  }

  def p8(context: Z3Context, opts: Options): Result[Program] = {
    val componentList = ListBuffer[Component]()
    componentList.appendAll(Library.brahmaStd().components)
    componentList += Components.and()
    componentList += Components.const_(if (opts.synthesizeConstants) None else Some(1))

    val builder = new ProgramBuilder()
    val a = builder.var_()
    val b = builder.const_(1)
    val c = builder.sub(a, b)
    val d = builder.const_(Int.MaxValue)
    val e = builder.xor(a, d)
    builder.and(c, e)
    val spec = new ProgramSpecification(builder.finish())

    synthesize(opts, context, spec, Library(componentList.toList))
  }

  def p9(context: Z3Context, opts: Options): Result[Program] = {
    val componentList = ListBuffer[Component]()
    componentList.appendAll(Library.brahmaStd().components)
    componentList += Components.const_(if (opts.synthesizeConstants) None else Some(31))

    val builder = new ProgramBuilder()
    val a = builder.var_()
    val b = builder.const_(31)
    val c = builder.shr_u(a, b)
    val d = builder.xor(a, c)
    builder.sub(d, c)
    val spec = new ProgramSpecification(builder.finish())

    synthesize(opts, context, spec, Library(componentList.toList))
  }

  def p10(context: Z3Context, opts: Options): Result[Program] = {
    val library = Library.brahmaStd()

    val builder = new ProgramBuilder()
    val x = builder.var_()
    val y = builder.var_()
    val a = builder.and(x, y)
    val b = builder.xor(x, y)
    builder.le_u(b, a)
    val spec = new ProgramSpecification(builder.finish())

    synthesize(opts, context, spec, library)
  }


  case class Options(
                      timeout: Option[Int],
                      minimal: Boolean,
                      onlyFast: Boolean,
                      synthesizeConstants: Boolean,
                      problems: List[String]
                    )

  object Options {
    def shouldRunProblem(options: Options, problem: String): Boolean = {
      options.problems.isEmpty || options.problems.contains(problem)
    }
  }

  def synthesize(
                  options: Options,
                  context: Z3Context,
                  spec: Specification,
                  library: Library
                ): Result[Program] = {
    val synthesizer = Synthesizer(context, library, spec)
    synthesizer.setTimeout(options.timeout)
    synthesizer.synthesize()
  }
}
