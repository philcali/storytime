package storytime
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.io.File

import StoryKeys._

class MetaTest extends FlatSpec with ShouldMatchers {
  val clean = StoryMetaKey[File]("clean")

  "Meta" should "be created fluently" in {
    (paginate := false) should be === Meta[Boolean](paginate, false)
  }

  it should "obtain its value easily" in {
    val settings = StoryMode(Seq (
      clean := new File(".")
    ))

    val paging = settings.getOrElse(paginate, false)

    paging should be === false 

    val file = settings.get(clean).get

    file.isDirectory should be === true
  }

  it should "handle seq settings fluently" in {
    import StoryMode._

    val expectedMacros = Seq (
      macro("test") { (dis, blocks) => 
        <div class = "test">
          { dis.toXHTML(blocks) }
        </div>
      },
      textMacro("other") { rest => <q> { rest } </q> }
    )

    val mode: StoryMode = Seq(
      clean := new File("."),

      macros += expectedMacros(0)
    )

    val otherSettings = Seq(
      clean := new File("core"),

      macros ++= Seq(expectedMacros(1))
    )

    val expectedMode = StoryMode(Seq(
      clean := new File("core"),
      macros := expectedMacros
    ))

    val evaluatedMode = mode ++ otherSettings

    expectedMode should be === evaluatedMode

    evaluatedMode.get(clean).get should be === new File("core")
  }
}
