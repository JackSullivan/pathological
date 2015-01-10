package so.modernized.pathological.path

import java.nio.file.Path

/**
 * @author John Sullivan
 */
protected[path] class PathSegment(protected[path] val p:Path) {
  def /(next:String) = new PathSegment(p resolve next)
  def /(otherPath:Path) = new PathSegment(p resolve otherPath)
  def /(otherSegment:PathSegment) = new PathSegment(p resolve otherSegment.p)

  override def toString = p.toAbsolutePath.toString

}

