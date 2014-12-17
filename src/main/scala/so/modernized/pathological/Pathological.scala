package so.modernized.pathological

import java.nio.file._
import scala.collection.mutable
import java.nio.file.attribute.BasicFileAttributes
import so.modernized.pathological.util._
import java.nio.charset.Charset
import scala.reflect._

/**
 * @author John Sullivan
 */
object Pathological {

  private class GlobResolver(globPath:Path) extends SimpleFileVisitor[Path] {
    val matcher = FileSystems.getDefault.getPathMatcher("glob:" + globPath.toString)
    val matches = mutable.ArrayBuffer[Path]()

    override def visitFile(file: Path, attrs: BasicFileAttributes) = {
      if(matcher matches file) matches += file
      super.visitFile(file, attrs)
    }
  }

  def expandGlob(p:Path):Iterable[Path] = {
    val resolver = new GlobResolver(p)
    Files.walkFileTree(p.toAbsolutePath.getRoot, resolver)
    resolver.matches
  }

  //todo this should always return a unique list of files that exist
  def expandPath(p:Path):Iterable[Path] = {
      if(p.toString.contains("*") || p.toString.contains("?")) expandGlob(p).flatMap(expandPath)
      else if(p.toFile.isDirectory) p.toFile.getFiles.map(_.toPath)
      else if(p.toFile.exists()) Iterable(p)
      else Iterable.empty[Path]
    }

  def read(filepath:String):ReadablePathoid = {
    val paths = expandPath(Paths get filepath)
    assert(paths.size != 0)
    assert(paths.forall(_.toFile.canRead))
    new ReadablePathoid(paths)
  }
}

class ReadablePathoid(paths:Iterable[Path]) {

  private lazy val lineIter = paths.iterator.flatMap(path => Files.newBufferedReader(path, Charset.defaultCharset()).toIterator)
  private val splitter = """\s+"""

  def intoMap[Key, Value](implicit keyTag:ClassTag[Key], valTag:ClassTag[Value]):Map[Key, Value] = lineIter.flatMap { line =>
    line split splitter match {
      case Array(k,v) => keyTag.parseString(k) -> valTag.parseString(v) match {
        case (Some(key), Some(value)) => Some(key -> value)
        case (Some(key), None) =>
          println("WARNING: Failed to parse value %s into type %s".format(v, valTag.runtimeClass.getName))
          None
        case (None, Some(value)) =>
          println("WARNING: Failed to parse key %s into type %s".format(k, keyTag.runtimeClass.getName))
          None
        case (None, None) =>
          println("WARNING: Failed to parse key %s into typo %s and value %s into type %s".format(k, keyTag.runtimeClass.getName, v, valTag.runtimeClass.getName))
          None
      }
      case arr =>
        println("WARNING: Expected to split line into 2 but instead split into %d giving %s".format(arr.size, arr.mkString("|")))
        None
    }
  }.toMap

  def linewise[A](f:(String => Option[A])):Iterator[Option[A]] = lineIter map f

  def linewiseWithWarnings[A](f:String => A):Iterator[A] =
    paths.iterator.flatMap{path =>
      Files.newBufferedReader(path, Charset.defaultCharset()).toIterator.flatMap{line =>
        f(line) match {
          case Some(result) => Some(result.asInstanceOf[A])
          case None =>
            println("WARNING: Failed to parse input line: %s".format(line))
            None
        }
      }
    }
}
