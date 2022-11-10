import scala.util.control.Breaks._
object exponentialDouble {
  def exponential(n:Double):Double={
    var exponentialOfWholeNumber:Double = 0.0
    var nTimes = 1
    var wholeNumber = n.toInt
    breakable{
    for ( i <- 1 to n.toInt){
      if (wholeNumber == 1) {
        exponentialOfWholeNumber = 1 + (1 + 1 / 2 * (1 + 1 / 3 * (1 + 1 / 4 * (1 + 1 / 5))))
        nTimes = i
        break
      }
      else
        wholeNumber = wholeNumber/2
    }
    }
    val dble = n - wholeNumber
    exponentialOfWholeNumber +=  1 + (dble + dble / 2 * (1 + dble / 3 * (1 + dble / 4 * (1 + dble / 5))))
    exponentialOfWholeNumber
  }

}
