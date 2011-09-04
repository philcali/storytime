package storytime
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.io.File

class MetaTest extends FlatSpec with ShouldMatchers with Keys {
  val clean = StoryMetaKey[File]("clean")

  "Meta" should "be created fluently" in {
    (paginate := false) should be === Meta[Boolean](paginate, false)
  }

  it should "obtain its value easily" in {
    val settings: Seq[Meta[_]] = Seq (
      clean := new File(".")
    )

    val paging = settings.getOrElse[Boolean]("paginate", false)

    paging should be === false 

    val file: File = settings.get("clean").get

    file.isDirectory should be === true
  }
}
