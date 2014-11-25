package so.modernized.pathological

import java.io.{BufferedReader, File}

/**
 * @author John Sullivan
 */
package object util {

  implicit class FileExtras(f:File) {
    /** if file is a directory returns all files in all subdirectories that match pred, otherwise returns itself in a seq */
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
}
