import java.io.File
import scala.Console.{GREEN, RED, RESET}
import scala.io.Source
import scala.util.{Random, Using}

object BuscaMinesTester extends App {
  val testFolderPath = "testsBuscaMines/tests/"
  val solFolderPath = "testsBuscaMines/sol/"

  def getListOfFiles(dir: String): List[String] = {
    val file = new File(dir)
    file.listFiles.filter(_.isFile).map(_.getPath).toList
  }

  val testFiles = getListOfFiles(testFolderPath)
  var numberOfTestThatSucceded = 0;

  val numberOfSolutions = getListOfFiles(solFolderPath).filter(_.endsWith(".txt")).length

  testFiles.foreach { testFile =>
    val fileName = testFile.split('\\').last
    val solFile = solFolderPath+fileName
    println(testFile + " " + solFile)

    val solFileDocument = new File(solFile)
    if(solFileDocument.exists){
      // Load the test file
      val testContent = Using(Source.fromFile(testFile)) { source =>
        source.getLines().toArray
      }.getOrElse {
        System.err.println(s"Error reading file: $testFile")
        Array.empty[String]
      }

      // Load the expected solution
      val expectedSolution = Using(Source.fromFile(solFile)) { source =>
        source.getLines().mkString("\n")
      }.getOrElse {
        System.err.println(s"Error reading solution file: $solFile")
        ""
      }

      val result = BuscaMines.solveMineSweeper(testContent)
      if(expectedSolution==result._2){
        numberOfTestThatSucceded += 1
        println(numberOfTestThatSucceded)
      }
      else{
        System.err.println("Test failed at the mineSweeper")
        // Iterate over both expectedSolution and result._2 to find differences
        val maxLength = math.max(expectedSolution.length, result._2.length)
        val highlightedResult = result._2.zipAll(expectedSolution, ' ', ' ').zipWithIndex.map {
          case ((resultChar, expectedChar), index) =>
            if (resultChar != expectedChar) {
              s"${RED}$resultChar${RESET}" // Highlight differing characters in red
            } else {
              resultChar.toString // Print matching characters normally
            }
        }.mkString

        // Print the highlighted result
        println(highlightedResult)
      }
    }
    else{
      System.err.println(s"File does not exist $solFile")
    }
  }
  if(numberOfTestThatSucceded==numberOfSolutions){
    println(s"${GREEN}ALL THE TESTS PASSED SUCCESSFULLY${RESET}") // Highlight differing characters in red)
  }
  else{
    val failedTests = numberOfSolutions-numberOfTestThatSucceded
    println(s"${RED} $failedTests FAILED TO PASS ${RESET}")
  }

}

object BuscaMines extends App{
  def solveMineSweeper(test: Array[String]) = {
    val e = new ScalAT("BuscaMines")

    val buscamines = buscaMinesToArray(test)
    val n = buscamines._1
    val m = buscamines._2
    val mines = buscamines._3
    val tauler: Array[Array[Int]] = e.newVar2DArray(n, m)
    val mineSweeper = buscamines._4
    for(i <- 0 until n){
      for(j <- 0 until m){
        addConstraint(i,j);
      }
    }

    def addConstraint(row: Int, col: Int): Unit = {
      val centerElement = mineSweeper(row)(col)
      centerElement match {
        case "-" => // do nothing (cas en el que no es sap res)
        case "x" | "X" => e.addClause(-tauler(row)(col) :: List()) //marcar casella mineSweeper(row)(col) == false
        case n => addConstraint3x3(centerElement,row,col) //exactlyN //get the 3x3 list and then add an exactlyK (K == n)
      }
    }

    def addConstraint3x3(centerElement: String, row: Int, col: Int) = {
      val clausula = tauler(row)(col)
      e.addClause(-clausula :: List())
      val elements = getSurroundingElements(row,col).flatten
      e.addEK(elements,centerElement.toInt)
    }

    def getSurroundingElements(row: Int, col: Int) = {
      val edgeElements = scala.collection.mutable.ListBuffer[List[Int]]()
      for (di <- -1 to 1) {
        val edgeElementsRow = scala.collection.mutable.ListBuffer[Int]()
        for (dj <- -1 to 1) {
          val rowD = row + di
          val colD = col + dj
          if(rowD>=0 && rowD<n && colD >= 0 && colD < m && !(rowD == row && colD == col))
            edgeElementsRow += tauler(rowD)(colD)
        }
        edgeElements += edgeElementsRow.toList
      }
      edgeElements.toList
    }

    def getMinesPositions = tauler
      .map(_.map((i: Int) => if (e.getValue(i)) "o " else "X "))
      .map(_.mkString("").trim)
      .mkString("\n")

    if (mines != -1) e.addEK(tauler.flatten.toList, mines)
    val result=e.solve()
    if (result.satisfiable) (result,getMinesPositions)
    else (result,"not satisfied")
  }

  def buscaMinesToArray(buscaMinesString: Array[String]) /*Array[Array[String]]*/ = {
    val firstLine = buscaMinesString(0).split(" ")
    val n = firstLine(0).toInt // rows
    val m = firstLine(1).toInt // columns
    val mines = firstLine(2).toInt // number of mines (or -1)

    val mapaBuscaMines = buscaMinesString.tail
    val result = mapaBuscaMines.map(fila => fila.split(" "))
    (n,m,mines,result)
  }

}
