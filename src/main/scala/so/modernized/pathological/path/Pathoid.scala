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
    val (roots, _) = p.iterator().asScala.toList.span(pathlet => containsGlob matches pathlet.toString)
    val resolver = new GlobResolver(p)
    Files.walkFileTree(rePathify(roots), resolver)
    resolver.matches
  }

}
