package institute.teias

sealed trait Verification

case object WorksForAllInputs extends Verification

case class Counterexample(inputs: Seq[Int]) extends Verification
