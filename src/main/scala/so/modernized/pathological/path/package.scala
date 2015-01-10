package so.modernized.pathological

import java.nio.file.Paths

/**
 * @author John Sullivan
 */
package object path {

  def ~~ = new PathSegment(Paths get System.getProperty("user.home"))
  def pwd = new PathSegment(Paths get "")
  def ** = new PathSegment(Paths get "**")
  def *(ext:String) = new PathSegment(Paths get ("*" + ensureDot(ext)))
}
