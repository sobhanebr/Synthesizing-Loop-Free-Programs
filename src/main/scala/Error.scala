package institute.teias

import scala.util.control.NoStackTrace

sealed trait Error extends NoStackTrace
case object NoComponents extends Error
case object SynthesisUnsatisfiable extends Error
case object SynthesisUnknown extends Error

