package so.modernized.pathological

import java.nio.file.{Files, Paths, Path}
import java.io.{FileReader, BufferedReader, File}
import scala.collection.JavaConverters._

/**
 * @author John Sullivan
 */
object Pathologic {

  private def cleanString(s:String):String = if(s.startsWith("./")) s.substring(2) else s

  implicit def string2Path(str:String):Path = Paths.get(cleanString(str))
  implicit def file2Path(file:File):Path = file.toPath
  implicit def string2PathExtras(str:String):PathExtras = string2Path(str)
  implicit def file2PathExtras(file:File):PathExtras = file2Path(file)

  def ~~ = Paths.get(System.getProperty("user.home"))
  //def ~ : Path= Paths get System.getProperty("user.home")
  def pwd: PathExtras = Paths get ""

  implicit class PathsExtras(ps:Iterable[Path]) {
    def collapse:Path = ps.reduce(_ / _)
  }

  implicit class PathExtras(p:Path) extends Iterable[Path] {
    def /(otherString:String) = p resolve otherString
    def /(other:Path):Path = p resolve other
    def /(others:Iterable[String]):Path = others.map(string2Path).reduce(_ / _)

/*
    private def glob2Regex(g:String):Regex =
      g.toCharArray.foldLeft(new StringBuilder) { case(sb, c) =>
        c match {
          case '*' => sb ++= """[^/]*""" // match any number of non-slash characters
          case '?' => sb ++= """[^/]""" // match a single non-slash character
          // alternation and ranges work the same way in regex and glob
          case otw => sb + otw
        }
      }.toString().r
  */
    //def glob(g:String):Option[Path] =  glob2Regex(g).findFirstIn(p.toAbsolutePath.toString).map(Paths.get)
    def after(s:String):Path = Paths.get((s + ".*").r.findFirstIn(p.toAbsolutePath.toString).get)

    def dot(suf:String) = p resolveSibling p.last.toString + "." + (if(suf.startsWith(".")) suf.substring(1) else suf)

    def iterator = p.iterator().asScala

    def realize = {
      val (dirs, nameList) = p.splitAt(p.size - 1)
      Files.createDirectories(dirs.collapse)
      Files.createFile(nameList.head)
    }
  }

  def read(filename:String):ReadablePathoid = ReadablePathoid fromFile filename
}

object ReadablePathoid {

  // todo check readability and writeability
  def fromFile(filename:String):ReadablePathoid = new File(filename) match {
    case f:File if f.exists() && f.isFile => new FilePathoid(f)
    case f:File if f.exists() && f.isDirectory => new FilesPathoid(f.listFiles()) //todo this isn't recursive, should it be?
  }
}

sealed trait ReadablePathoid {
  //def filewise[A](f:(File => A)):A

  def linewise[A](f:(String => A)):Iterator[A]
}
private class FilePathoid(file:File) extends ReadablePathoid {
  private val rdr = new BufferedReader(new FileReader(file))

  def linewise[A](f:(String => A)):Iterator[A] = rdr.toIterator.map(f)
}
private class FilesPathoid(files:Iterable[File]) extends ReadablePathoid {
  def linewise[A](f:(String => A)):Iterator[A] = files.toIterator.flatMap(file => new BufferedReader(new FileReader(file)).toIterator).map(f)
}
