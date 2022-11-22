package Week2Assignment

import java.io.{File, FileNotFoundException, IOException, PrintWriter}
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ListBuffer
import com.github.tototoshi.csv._

import scala.collection.mutable

object StringMatch {
  var fileString: String = ""

  def readTextFile(path: String): Unit = {
    try {
      val bufferedSource = Source.fromFile(path)
      fileString = bufferedSource.getLines().mkString("\n ").toLowerCase()
      bufferedSource.close()
      println("File loaded")
    }
    catch {
      case _: FileNotFoundException => println("File Not Found")
      case _: IOException => println("Got a IO Exception")
    }
  }

  def getRandomWordsFromText(InputTxt: String): Unit = {
    val wordsList = InputTxt.split(" ").map(word => word.trim())
    val FiveLetterKeyWord = new ListBuffer[String]
    val SixLetterKeyWord = new ListBuffer[String]
    val SevenLetterKeyWord = new ListBuffer[String]
    val EightLetterKeyWord = new ListBuffer[String]
    val NineLetterKeyWord = new ListBuffer[String]
    val TenLetterKeyWord = new ListBuffer[String]
    val rand = new Random()
    (0 until 7000).foreach { _ =>
      val randomIndex = rand.nextInt(2463683)
      val keyword: String = wordsList(randomIndex)
      keyword.length match {
        case 5 => FiveLetterKeyWord.append(keyword)
        case 6 => SixLetterKeyWord.append(keyword)
        case 7 => SevenLetterKeyWord.append(keyword)
        case 8 => EightLetterKeyWord.append(keyword)
        case 9 => NineLetterKeyWord.append(keyword)
        case 10 => TenLetterKeyWord.append(keyword)
        case _ => ""
      }
    }
    val keywordsList = List(FiveLetterKeyWord.take(80), SixLetterKeyWord.take(80), SevenLetterKeyWord.take(80), EightLetterKeyWord.take(80), NineLetterKeyWord.take(80), TenLetterKeyWord.take(80))
    var keywords = ""
    keywordsList.foreach(lst => lst.foreach(i => keywords = keywords.concat(s"$i\n")))
    val pw = new PrintWriter(new File("keywords.txt"))
    pw.write(keywords)
    pw.close()

  }

  def timeTaken(keywordsPath: String, completeString: String): (ListBuffer[Long], ListBuffer[Long], ListBuffer[String]) = {
    val keywordSource = Source.fromFile(keywordsPath)
    val keywordsList = keywordSource.getLines().toList
    keywordSource.close()
    val containsTimeTaken = new ListBuffer[Long]()
    val matchTimeTaken = new ListBuffer[Long]()
    val keywordList = new ListBuffer[String]()
    keywordsList.foreach { eachWord =>
      var start = System.nanoTime()
      var result = completeString.contains(eachWord)
      var end = System.nanoTime()
      containsTimeTaken.append(end - start)
      start = System.nanoTime()
      result = completeString.matches(eachWord)
      end = System.nanoTime()
      matchTimeTaken.append(end - start)
      keywordList.append(eachWord)
    }
    (containsTimeTaken, matchTimeTaken, keywordList)
  }

  def randomStringFromCharList(length: Int): String = {
    val chars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
    val sb = new mutable.StringBuilder
    for (i <- 1 to length) {
      val randomNum = util.Random.nextInt(chars.length)
      sb.append(chars(randomNum))
    }
    sb.toString
  }


  def main(args: Array[String]): Unit = {
    readTextFile("samples.txt")
    //        getRandomWordsFromText(fileString)
    (0 to 6).foreach { experiment =>
      val (containsTime, matchTime, keywordList) = timeTaken("keywords.txt", fileString)
      val writer = CSVWriter.open("newOut.csv", append = true)
      (0 to 599).foreach(idx => writer.writeRow(List(keywordList(idx).length, keywordList(idx), containsTime(idx), matchTime(idx))))
      writer.close()
      println(experiment + " file written")
    }

  }

}
