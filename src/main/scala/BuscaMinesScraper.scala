import org.jsoup.Jsoup

import java.io.{File, PrintWriter}
import scala.collection.JavaConverters._
import scala.util.matching.Regex

object BuscaMinesScraper extends App{
  var successes = 0;
  // Function to scrape a webpage and extract the puzzle sections
  def extractPuzzleData(url: String): String = {
    try {
      val doc = Jsoup.connect(url).get()
      val scriptTag = doc.select("script#data").html()
      scriptTag
    }
  }

  // Function to scrape all puzzles from the main index page
  def scrapeAllPuzzles(): Unit = {
    val indexUrl = "https://www.janko.at/Raetsel/Minesweeper/index.htm"


    // Fetch the main index page
    val indexDoc = Jsoup.connect(indexUrl).get()

    // Extract all puzzle URLs from the index page (assuming links to puzzles end with ".htm")
    val puzzleLinks = indexDoc.select("a[href]").asScala
      .map(_.attr("href"))
      .filter(_.matches(".*\\.a\\.htm")).toSet

    // Iterate through each puzzle URL and extract the data
    for (puzzleLink <- puzzleLinks) {
      val fullPuzzleUrl = "https://www.janko.at/Raetsel/Minesweeper/" + puzzleLink
      extractPuzzleData(fullPuzzleUrl) match {
        case result => parseResult(fullPuzzleUrl, result);
        case _ => System.err.println("Error");
      }
    }
  }

  def parseResult(url: String,str: String) = {
    println(str)
    println(url)
    var rows = 0
    var cols = 0
    var problemSection = false
    var solutionSection = false
    var problemData = ""
    var solutionData = ""
    val baseFileName = url.split("/").last.replace(".a.htm", ".txt")

    val lines = str.split("\n")

    for (line <- lines) {
      line match {
        case l if l.startsWith("size") =>
          rows = l.split(" ")(1).toInt
          cols = rows // If it's "size", rows and columns are the same
        case l if l.startsWith("rows") =>
          rows = l.split(" ")(1).toInt
        case l if l.startsWith("cols") =>
          cols = l.split(" ")(1).toInt

        case l if l.startsWith("[problem]") =>
          problemSection = true
        case l if l.startsWith("[solution]") =>
          problemSection = false
          solutionSection = true
        case l if l.startsWith("[moves]") =>
          solutionSection = false
          problemSection = false
        case l if l.startsWith("[end]") =>
          solutionSection = false
          problemSection = false

        case l if problemSection =>
          problemData += l + "\n"
        case l if solutionSection =>
          solutionData += l + "\n"
        case _ =>
      }
    }

    val testFileName = s"testsBuscaMines/tests/$baseFileName"
    val solutionFileName = s"testsBuscaMines/sol/$baseFileName"

    println(problemData)
    println(solutionData)

    val testWriter = new PrintWriter(new File(testFileName))
    try {
      testWriter.println(s"$rows $cols -1")
      testWriter.print(problemData.trim.replace('.','-')) // Write problem matrix
    } finally {
      testWriter.close()
    }

    //que són els punts?
    val solutionWriter = new PrintWriter(new File(solutionFileName))
    try {
      solutionWriter.print(solutionData.trim.replace('-','X').replace('x','o').replace('.','X')) // Write solution matrix
    } finally {
      solutionWriter.close()
    }
  }

  // Run the scraper
  scrapeAllPuzzles()
}
