package so.modernized

import java.io.BufferedReader

/**
 * @author John Sullivan
 */
package object pathological {

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
