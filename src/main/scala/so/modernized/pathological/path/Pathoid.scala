package so.modernized.pathological.path

import java.nio.file._
import scala.collection.mutable
import java.nio.file.attribute.BasicFileAttributes
import java.io.File

/**
 * @author John Sullivan
 */
object Pathoid {

  private class GlobResolver(globPath:Path) extends SimpleFileVisitor[Path] {
    val matcher = FileSystems.getDefault.getPathMatcher("glob:" + globPath.toString)
    val matches = mutable.ArrayBuffer[Path]()

    override def visitFile(file: Path, attrs: BasicFileAttributes) = {
      if(matcher matches file) matches += file
      super.visitFile(file, attrs)
    }
  }

  def ~~ = new PathoidBuilder(Paths.get(System.getProperty("user.home")))
  def pwd = new PathoidBuilder(Paths get "")
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

  def read

}

sealed trait Pathoid
private class ReadableFilePathoid extends Pathoid
private class ReadableFilesPathoid extends Pathoid
