package so.modernized.pathological

import org.scalatest.{FlatSpec, Matchers}
import java.nio.file.{Paths, Files}

/**
 * @author John Sullivan
 */
class PathologicalTests extends FlatSpec with Matchers {

  def fileFixtures() {
    Files.createDirectories(Paths get "foo/bar/baz")
    (1 to 3) foreach { idx =>
      Files.createFile(Paths get "foo/bar/baz/%s.a".format(idx))
      Files.createFile(Paths get "foo/bar/baz/%s.b".format(idx))
    }
    Files.createFile(Paths get "foo/bar/inbar.a")
  }

  def deleteFixture() {
    Files.delete(Paths get "foo/bar/inbar.a")
    (1 to 3) foreach { idx =>
      Files.delete(Paths get "foo/bar/baz/%s.a".format(idx))
      Files.delete(Paths get "foo/bar/baz/%s.b".format(idx))
    }
    Files.delete(Paths get "foo/bar/baz")
    Files.delete(Paths get "foo/bar")
    Files.delete(Paths get "foo")
  }

  "expandGlob" should "expand for **" in {
    //fileFixtures()

    //println(Pathological.expandGlob(Paths get "foo/**/1.a"))

    //deleteFixture()
  }

}
