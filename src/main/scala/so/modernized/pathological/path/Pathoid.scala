package so.modernized.pathological.path

import java.nio.file._
import scala.collection.mutable
import java.nio.file.attribute.BasicFileAttributes
import java.io.File
import scala.collection.JavaConverters._
import so.modernized.pathological.util._

/**
 * @author John Sullivan
 */
object Pathoid {

  private class GlobResolver(globPath:Path) extends SimpleFileVisitor[Path] {
    val matcher = FileSystems.getDefault.getPathMatcher("glob:" + globPath.toString)
    val matches = mutable.ArrayBuffer[Path]()

    override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = if(Files isReadable dir) {
      FileVisitResult.CONTINUE
    } else {
      FileVisitResult.SKIP_SUBTREE
    }

    override def visitFile(file: Path, attrs: BasicFileAttributes) = {
      println("visiting :%s as %s".format(file, file.toAbsolutePath))
      if(matcher matches file) matches += file
      super.visitFile(file, attrs)
    }
  }

  def ~~ = new PathoidBuilder(Paths.get(System.getProperty("user.home")))
  def pwd = new PathoidBuilder(Paths get "")
  def ** = new PathoidBuilder(Paths get "**")
  private class PathoidBuilder(var p:Path) {
    def this(str:String) = this(Paths get str)

    def /(otherString:String) = {
      p = p resolve otherString
      this
    }
    def /(other:Path) = {
      p = p resolve other
      this
    }
    def /(otherBuilder:PathoidBuilder) = {
      p = p resolve otherBuilder.p
      this
    }
    def /(others:Iterable[String])= {
      p = p resolve others.map(o => new PathoidBuilder(o)).reduce(_ / _)
      this
    }


    private def resolveGlob:Iterable[Path] = {
      val resolver = new GlobResolver(p)
      Files.walkFileTree(p.getRoot, resolver)
      resolver.matches
    }

    private def getSubPaths:Iterable[Path] = {
      def helper(dir:File):Seq[File] = if(dir.isDirectory) {
        dir.listFiles flatMap helper
      } else {
        Seq(dir.toPath)
      }
      helper(p, List.empty[Path])
    }

    def buildExisting:Iterable[Path] = {
      if(p.toString.contains("*") || p.toString.contains("?")) resolveGlob
      else if(p.toFile.isDirectory) getSubPaths
      else if(p.toFile.exists()) Iterable(p)
      else Iterable.empty[Path]
    }
  }

}

class PathSegment(protected[path] val p:Path) {
  def /(next:String) = new PathSegment(p resolve next)
  def /(otherPath:Path) = new PathSegment(p resolve otherPath)
  def /(otherSegment:PathSegment) = new PathSegment(p resolve otherSegment.p)

  override def toString = p.toAbsolutePath.toString

}

object PathSegmentImplicits {

  implicit def path2segment(p:Path) = new PathSegment(p)
  implicit def segment2path(s:PathSegment) = s.p

  private def ensureDot(str:String) = if(str startsWith ".") str else "." + str

  private class GlobResolver(globPath:Path) extends SimpleFileVisitor[Path] {
    val matcher = FileSystems.getDefault.getPathMatcher("glob:" + globPath.toString)
    val matches = mutable.ArrayBuffer[Path]()

    override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = if(Files isReadable dir) {
      FileVisitResult.CONTINUE
    } else {
      FileVisitResult.SKIP_SUBTREE
    }

    override def visitFile(file: Path, attrs: BasicFileAttributes) = {
      if(matcher matches file) matches += file
      super.visitFile(file, attrs)
    }
  }

  def resolveGlob(p:Path):Iterable[Path] = {
    val containsGlob = """(?<!\\)\*|(?<!\\)\?""".r // match * or ? unless it is preceded by \
    def rePathify(ps:Iterable[Path]):Path = Paths get ps.map(_.toString).mkString(FileSystems.getDefault.getSeparator)
    val (roots, _) = p.iterator().asScala.toList.span(containsGlob.matches)
    val resolver = new GlobResolver(p)
    Files.walkFileTree(rePathify(roots), resolver)
    resolver.matches
  }

  def ~~ = new PathSegment(Paths get System.getProperty("user.home"))
  def pwd = new PathSegment(Paths get "")
  def ** = new PathSegment(Paths get "**")
  def *(ext:String) = new PathSegment(Paths get ("*" + ensureDot(ext)))

}
