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

  def read1(filepath:String):ReadablePathoid = {
    val paths = expandPath(Paths get filepath)
    assert(paths.size != 0)
    assert(paths.forall(_.toFile.canRead))
    new ReadablePathoid(paths)
  }

  /** Takes a string of glob style paths or a comma-separated string of same
    * and returns a [[so.modernized.ReadablePathoid]] for further processing */
  def read(fileLike:String) = new ReadablePathoid(fileLike.split(",").flatMap(f => expandPath(Paths get f)).map(expandGlob))
}

class ReadablePathoid(paths:Iterable[Path]) {

  private lazy val lineIter = paths.iterator.flatMap(path => Files.newBufferedReader(path, Charset.defaultCharset()).toIterator)
  private val splitter = """\s+"""

  def intoMap[Key, Value](implicit keyTag:ClassTag[Key], valTag:ClassTag[Value]):Map[Key, Value] = lineIter.flatMap { line =>
    line split splitter match {
      case Array(k,v) => keyTag.runtimeClass.parsePrimitive(k) -> valTag.runtimeClass.parsePrimitive(v) match {
        case (Some(key), Some(value)) => Some(key.asInstanceOf[Key] -> value.asInstanceOf[Value])
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


  def linewiseIntoClass[Target:ClassTag]:Iterator[Target] = {
    // todo pick the constructor better
    val con = classTag[Target].getClass.getConstructors.head
    val params = con.getParameterTypes
    lineIter.map { line =>
      val arr = line split splitter
      assert(arr.size == params.size)
      con.newInstance((params zip arr).map{case (c,s) => c.parsePrimitive(s).get.asInstanceOf[Object]} :_*).asInstanceOf[Target]
    }
  }

  def linewiseInto[T1:ClassTag]:Iterator[T1]
  def linewiseInto[T1:ClassTag, T2:ClassTag]:Iterator[(T1, T2)]
  def linewiseInto[T1:ClassTag, T2:ClassTag, T3:ClassTag]:Iterator[(T1, T2, T3)]
  def linewiseInto[T1:ClassTag, T2:ClassTag, T3:ClassTag, T4:ClassTag]:Iterator[(T1, T2, T3, T4)]
}
