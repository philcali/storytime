package storytime
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.io.File

class MetaTest extends FlatSpec with ShouldMatchers with ImplicitKeys {
  val paginated = StoryMetaKey[Boolean]("paginated", required = true)
  val clean = StoryMetaKey[File]("clean")

  "Meta" should "be created fluently" in {
    (paginated := false) should be === Meta[Boolean](paginated, false)
  }

  it should "obtain its value easily" in {
    val settings: Seq[Meta[_]] = Seq (
      clean := new File(".")
    )

    val paging = settings.getOrElse[Boolean]("paginated", false)

    paging should be === false 

    val file: File = settings.get("clean").get

    file.isDirectory should be === true
  }
}
