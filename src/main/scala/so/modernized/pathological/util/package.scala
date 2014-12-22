package so.modernized.pathological

import java.io.{BufferedReader, File}
import scala.reflect._
import scala.collection.mutable
import scala.util.matching.Regex

/**
 * @author John Sullivan
 */
package object util {

  implicit class FileExtras(f:File) {
    /** if file is a directory returns all files in all subdirectories, otherwise returns itself in a seq */
    def getFiles:Seq[File] = {

      def helper(dir:File):Seq[File] = if(dir.isDirectory) {
        dir.listFiles flatMap helper
      } else {
        Seq(dir)
      }
      helper(f)
    }
  }

  implicit class BufferedReaderExtras(rdr:BufferedReader) {

    /** Returns an iterator over the lines of the buffered reader's contents.
      * Consumes the reader and auto-closes. */
    def toIterator:Iterator[String] = new Iterator[String] {
      private var nextLine = rdr.readLine()

      def next() = {
        val res = nextLine
        nextLine = rdr.readLine()
        if(nextLine == null) {
          rdr.close()
        }
        res
      }

      def hasNext = nextLine != null
    }
  }

  implicit class RegexExtras(r:Regex) {
    def matches(str:String):Boolean = r.findFirstIn(str).isDefined
  }

  implicit class ClassExtras[T](co:Class[T]) {

    def parsePrimitive(str:String):Option[T] = co match {
      case c if c == classOf[Boolean] => Some(str.toBoolean.asInstanceOf[T])
      case c if c == classOf[Byte] => Some(str.toByte.asInstanceOf[T])
      case c if c == classOf[Short] => Some(str.toShort.asInstanceOf[T])
      case c if c == classOf[Int] => Some(str.toInt.asInstanceOf[T])
      case c if c == classOf[Long] => Some(str.toLong.asInstanceOf[T])
      case c if c == classOf[Float] => Some(str.toFloat.asInstanceOf[T])
      case c if c == classOf[Double] => Some(str.toDouble.asInstanceOf[T])
      case c if c == classOf[String] => Some(str.asInstanceOf[T])
      case _ => None
    }
  }
}
