import scala.math.ceil

def log2(i: Int): Double = Math.log(i) / Math.log(2)

def numberOfVars(x: Int): Int = {
  if(x<=0)
    return 0;
  ceil(log2(x)).toInt
}

def addAMOLog(x: List[Int]): Unit = {
  val l = x.toArray
  val nVars = l.length;
  val numberOfNewClauses = numberOfVars(nVars);
  for (j <- 0 until numberOfNewClauses) {
    //println("newVar: " + j)
    println
    for (i <- 0 until nVars) {
      if(i.toBinaryString.reverse.padTo(numberOfNewClauses, '0').reverse.mkString.charAt(j)=='0'){
        print(i.toBinaryString.reverse.padTo(numberOfNewClauses, '0').reverse.mkString)
        println("\t 0")
      }
      else{
        print(i.toBinaryString.reverse.padTo(numberOfNewClauses, '0').reverse.mkString)
        println("\t 1")
      }
    }
  }
}


addAMOLog(List(0,1,2,3,4,5,6,7,8))

val x = List(1,2,3,4,6)
val maxVars = Math.pow(numberOfVars(x.length),2);
