package institute.teias
import Components._

import scala.collection.mutable.ListBuffer

case class Library(components: Seq[Component])

object Library {
  def brahmaStd(): Library = {
    val componentList = ListBuffer[Component]()

    // 1.
    componentList += add()
    // 2.
    componentList += and()
    // 3. neg(x) = 0 - x
    componentList += const_(Some(0))
    componentList += sub()
    // 4. not(a) = xor a, MAX
    componentList += const_(Some(java.lang.Integer.MAX_VALUE))
    componentList += xor()
    // 5.
    componentList += or()
    // 6.
    componentList += shrs()
    // 7.
    componentList += shru()
    // 8.
    componentList += sub()
    // 9.
    componentList += uge()
    // 10.
    componentList += ugt()
    // 11. ule
    componentList += ule()
    // 12.
    componentList += xor()

    Library(componentList)
  }
}
