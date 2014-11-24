package so.modernized.pathological

import so.modernized.pathological.Pathologic._

/**
 * Hello world!
 *
 */
object App {
  def main(args:Array[String]) {
    def splitLine(line:String) = {
      val Array(k,v) = line.split("\t")
      k -> v.toDouble
    }

    val res = read(args(0)) linewise splitLine
  }
}
