import java.util.Dictionary

object ValueLabsCoding {
  def main(args: Array[String]): Unit = {
    if(args.length == 2){
      // No need to generate the inputFile string we can directly execute the program
      readTheInputFilesAndSolve(args)
    } else {
      println("you need to pass the Dictionary and inputFile File")
    }
  }

  def solve(input: String,
            dictionary: List[String]): Int = {
    dictionary map {
      eachWord =>
       //println("each word is ", eachWord)
        var iter = 0
        val wordLength = eachWord.length
        val subStringInDict = eachWord.substring(1, wordLength - 1).sorted
        var cond = false
        var a = 0;
        while (iter < input.length - wordLength  && !cond) {
          val endIndex = iter + wordLength - 1;
          if (input.charAt(iter) == eachWord.charAt(0) && input.charAt(endIndex) == eachWord.charAt(wordLength - 1)) {
            if (wordLength == 2) {
              cond = true
              a = a+1
            } else {
              val subStringInInput = input.substring(iter + 1, iter+wordLength - 1).sorted
              if (subStringInInput == subStringInDict) {
                cond = true
                a = a+1
              }
            }
          }
          iter = iter + 1;
        }
        a
    } sum
  }

  def readTheInputFilesAndSolve(args: Array[String]) = {
    import scala.io.Source
    val inputPath = args(0)
    val dictionaryInputPath = args(1)
    val dictionarySource = Source.fromFile(dictionaryInputPath)
    val inputSource = Source.fromFile(inputPath)
    val dictionaryList  = dictionarySource.getLines().map(_.toString).toList
    var i:Int=1;
    for (line <- inputSource.getLines) {
      val output = solve(line, dictionaryList)
      println(s"case #$i: "+ output)
      i = i+1
    }
  }
}