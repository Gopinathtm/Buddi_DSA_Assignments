import scala.util.control.Breaks._
object exponentialDouble {
  def exponential(n:Double)={
    var exponentialOfOne = 0
    var nTimes = 1
    var wholeNumber = n.toInt
    breakable{
    for ( i <- 1 to n.toInt){
      if (wholeNumber == 1) {
        exponentialOfOne = 1 * (1 + 1 / 2 * (1 + 1 / 3 * (1 + 1 / 4 * (1 + 1 / 5))))
        nTimes = i
        break
      }
      else
        wholeNumber = wholeNumber/2
    }
    }
  }

}
