import scala.util.control.Breaks._

object exponentialDouble {
  def exponentialWholeNumber(n: Int): Double = {
    val binaryString = n.toBinaryString
    val exponentialMap = scala.collection.mutable.Map(0 -> 1.0, 1 -> math.E)
    var exponentOfWholeNumber = 1.0
    var ePower = 1
    n match {
      case 0 => exponentialMap(0)
      case _ => {
        (binaryString.length - 1 to 0 by -1).foreach { i =>
          if (binaryString(i) == '0') {
            ePower *= 2
            exponentialMap(ePower) = exponentialMap(ePower / 2) * exponentialMap(ePower / 2)
          }
          else if (binaryString(i) == '1') {
            ePower *= 2
            exponentialMap(ePower) = exponentialMap(ePower / 2) * exponentialMap(ePower / 2)
            exponentOfWholeNumber *= exponentialMap(ePower)
          }
        }
        exponentOfWholeNumber
      }
    }
  }

  def exponentialDecimal(n: Double): Double = {
    1 + n * (1 + n / 2 * (1 + n / 3 * (1 + n / 4 * (1 + n / 5))))
  }

  def exponentialMethodFour(n: Double): Double = {
    val wholeNumber = n.toInt
    val decimalNumber = n - wholeNumber
    val result = exponentialWholeNumber(wholeNumber) + exponentialDecimal(decimalNumber)
    result
  }}