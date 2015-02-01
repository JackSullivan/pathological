package so.modernized.and.pathological

import java.io._
import java.net.URL
import scala.reflect.{ClassTag, classTag}

/**
 * @author John Sullivan
 */
object Pathological {

  //def read(pathlike:String):Readable
  def read(file:File):Readable = if(file.isDirectory) {
    new MultiFileReader(file.listFiles().toIterator.map(f => new BufferedReader(new FileReader(f))))
  } else {
    new SingleFileReadable(new BufferedReader(new FileReader(file)))
  }
  def read(url:URL):Readable = read(url.openStream())
  def read(is:InputStream):Readable = new SingleFileReadable(new BufferedReader(new InputStreamReader(is)))

  private[pathological] def bufferToIterator(rdr:BufferedReader):Iterator[String] = new Iterator[String] {
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

  implicit class SplitIterExtras(iter:Iterator[Seq[String]]) {
    def into[A](implicit at:ClassTag[A], formatter:Formatter):Iterator[A] = iter.map { case Seq(a) => // todo something more robust
      formatter.format[A].apply(a)
    }
    def into[A,B](implicit at:ClassTag[A], bt:ClassTag[B], formatter:Formatter):Iterator[(A,B)] = iter.map { case Seq(a,b) =>
      formatter.format[A].apply(a) -> formatter.format[B].apply(b)
    }
  }

}

trait Formatter {
  def format[A:ClassTag]:PartialFunction[String, A]
}

object DefaultFormatter extends Formatter {
  def formatImpl[A:ClassTag]:PartialFunction[(String, ClassTag[A]), A] = {
    case (s, ct) if ct == ClassTag.Boolean =>  s.toBoolean.asInstanceOf[A]
    case (s, ct) if ct == ClassTag.Byte =>     s.toByte.asInstanceOf[A]
    case (s, ct) if ct == ClassTag.Char =>     s.toCharArray.head.asInstanceOf[A] //todo check nonsingle case
    case (s, ct) if ct == ClassTag.Double =>   s.toDouble.asInstanceOf[A]
    case (s, ct) if ct == ClassTag.Float =>    s.toFloat.asInstanceOf[A]
    case (s, ct) if ct == ClassTag.Int =>      s.toInt.asInstanceOf[A]
    case (s, ct) if ct == ClassTag.Long =>     s.toLong.asInstanceOf[A]
    case (s, ct) if ct == ClassTag.Short =>    s.toShort.asInstanceOf[A]
  }


  def format[A:ClassTag] = new PartialFunction[String, A] {
    private val ct = classTag[A]
    def isDefinedAt(x: String) = formatImpl.isDefinedAt(x -> ct)

    def apply(v1: String) = formatImpl.apply(v1 -> ct)
  }
}

sealed trait Readable {
  def linewise:Splitable
  def filewise:Splitable
}

protected[pathological] class Splitable(private val iter:Iterator[String]) {

  def splitingOn(delim:String) = iter.map(_ split delim)

  def asTSV = splitingOn("\t")
  def asCSV = splitingOn(",") //todo account for all the weird stuff people do with csvs
}

protected[pathological] class MultiFileReader(brs:Iterator[BufferedReader]) extends Readable {
  import Pathological._
  lazy val linewise = new Splitable(brs flatMap bufferToIterator)
  lazy val filewise = new Splitable(brs.map(br => bufferToIterator(br).mkString("\n")))
}

protected[pathological] class SingleFileReadable(br:BufferedReader) extends Readable {
  import Pathological._
  lazy val linewise = new Splitable(bufferToIterator(br))
  lazy val filewise = new Splitable(new Iterator[String] {
    private val fileString = bufferToIterator(br).mkString("\n") // todo fix for multiple line endings
    private var _hasNext = true
    def next() = {
      _hasNext = false
      fileString
    }

    def hasNext = _hasNext
  })
}


object Foo {
  implicit class StringIterExtras(iter:Iterator[String]) {
    def into[A](implicit at:ClassTag[A]):Iterator[A] = {
      val mapper = at match {
        case ClassTag.Boolean => s:String => s.toBoolean
        case ClassTag.Byte =>    s:String => s.toByte
        case ClassTag.Char =>    s:String => s.toCharArray.head //todo check nonsingle case
        case ClassTag.Double =>  s:String => s.toDouble
        case ClassTag.Float =>   s:String => s.toFloat
        case ClassTag.Int =>     s:String => s.toInt
        case ClassTag.Long =>    s:String => s.toLong
        case ClassTag.Short =>   s:String => s.toShort
        //case tag if tag == classOf[Array[Boolean]] => s:String => new Array[Boolean](1)
      }
      iter.map(s => mapper(s).asInstanceOf[A])
    }
    //def into[A,B]:Iterator[(A,B)]
    //def intoCaseClass[A <: Product]:Iterator[A]

  }

}
