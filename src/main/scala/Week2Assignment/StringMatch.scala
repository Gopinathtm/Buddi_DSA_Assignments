package Week2Assignment

import java.io.{File, FileNotFoundException, IOException, PrintWriter}
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ListBuffer

object StringMatch {
  var fileString :String = ""

  def readTextFile(path: String): Unit = {
    try {
      val bufferedSource = Source.fromFile(path)
      fileString = bufferedSource.getLines().mkString(" ").replaceAll("[’”—_'-+-.^:;,\"]","")
      bufferedSource.close()
      println("File loaded")
    }
    catch {
      case _: FileNotFoundException => println("File Not Found")
      case _: IOException => println("Got a IO Exception")
    }
  }

  def getRandomWordsFromText(InputTxt: String):Unit= {
    val wordsList = InputTxt.split(" ").map( word => word.trim())
    val FiveLetterKeyWord = new ListBuffer[String]
    val SixLetterKeyWord = new ListBuffer[String]
    val SevenLetterKeyWord = new ListBuffer[String]
    val EightLetterKeyWord = new ListBuffer[String]
    val NineLetterKeyWord = new ListBuffer[String]
    val TenLetterKeyWord = new ListBuffer[String]
    val rand = new Random()
    (0 until 3000).foreach { _ =>
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
    val keywordsList = List(FiveLetterKeyWord.take(20), SixLetterKeyWord.take(10), SevenLetterKeyWord.take(10), EightLetterKeyWord.take(10), NineLetterKeyWord.take(10), TenLetterKeyWord.take(20))
    var keywords = ""
    keywordsList.foreach(lst => lst.foreach(i => keywords = keywords.concat( s"$i\n")))
    val pw = new PrintWriter(new File("keywords.txt" ))
    pw.write(keywords)
    pw.close()

  }

  def main(args: Array[String]): Unit = {
    readTextFile("bible.txt")
    getRandomWordsFromText(fileString)
  }

}
