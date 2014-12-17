package so.modernized.pathological

import java.io.{BufferedReader, File}
import scala.reflect._

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


  implicit class ClassTagExtras[T](ct:ClassTag[T]) {
    private val strTag = classTag[String]

    // todo Check for parse failures
    def parseString(str:String):Option[T] = ct match {
      case ClassTag.Boolean => Some(str.toBoolean.asInstanceOf[T])
      case ClassTag.Byte => Some(str.toByte.asInstanceOf[T])
      case ClassTag.Short => Some(str.toShort.asInstanceOf[T])
      case ClassTag.Int => Some(str.toInt.asInstanceOf[T])
      case ClassTag.Long => Some(str.toLong.asInstanceOf[T])
      case ClassTag.Float => Some(str.toFloat.asInstanceOf[T])
      case ClassTag.Double => Some(str.toDouble.asInstanceOf[T])
      case _ if ct == strTag => Some(str.asInstanceOf[T])
      case _ => None
    }
  }
}
