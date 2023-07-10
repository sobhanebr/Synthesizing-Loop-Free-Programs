package institute.teias

import scala.util.control.Breaks._

case class Id(value: Int) {
  override def toString: String = {
    val sb = new StringBuilder()
    var x = value
    breakable{
      do {
        val y = x % 26
        x = x / 26
        sb.insert(0, (y + 'a').toChar)
        if (x == 0) {
          break
        }
        x -= 1
      } while (true)
    }
    sb.toString()
  }
}