import java.io.PrintWriter

object Solution {
  sealed trait FL
  case object F extends FL
  case object L extends FL
  /*
   * Complete the findSuffix function below.
   */
  def findSuffix(collections: Array[String], queryString: String): Int = {
    /*
     * Write your code here.
     */
    val firstOpt = binSearch(queryString, collections, 0, collections.length-1, F)
    val lastOpt = binSearch(queryString, collections, 0, collections.length-1, L)

    (firstOpt, lastOpt) match {
      case (Some(f), Some(l)) => l - f + 1
      case (_, _) => 0
    }
  }

  def binSearch(el: String, xs: Array[String], s: Int, e: Int, firstOrLast: FL): Option[Int] = {
    if (s > e) {
      None
    } else if (s == e) {
      if (xs(s) == el) Some(s) else None
    } else {
      val pivotIndex = (s + e)/2
      val pivot = xs(pivotIndex)

      val (newS, newE) = if (el < pivot) {
        (s, pivotIndex - 1)
      } else if (el > pivot) {
        (pivotIndex + 1, e)
      } else {
        firstOrLast match {
          case F => (s, pivotIndex)
          case L => (pivotIndex, e)
        }
      }

      binSearch(el, xs, newS, newE, firstOrLast)
    }
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(System.out)

    val stringsCount = stdin.readLine.trim.toInt

    val strings = Array.ofDim[String](stringsCount)

    for (stringsItr <- 0 until stringsCount) {
      val stringsItem = stdin.readLine
      strings(stringsItr) = stringsItem}

    val q = stdin.readLine.trim.toInt

    for (qItr <- 1 to q) {
      val queryString = stdin.readLine

      val res = findSuffix(strings.sortWith(_ < _), queryString)

      printWriter.println(res)
    }

    printWriter.close()
  }
}

