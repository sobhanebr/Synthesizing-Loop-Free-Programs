package institute.teias

import java.time.{Duration, Instant}

sealed trait Timeout
case class DurationTimeout(duration: Duration) extends Timeout
case class InstantTimeout(instant: Instant) extends Timeout
