package storytime
package test

import org.scalatest.{FlatSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

import java.io.File

class Generator extends FlatSpec with ShouldMatchers with BeforeAndAfterAll {
  val destination = new File(".")

  val story = 
"""
meta = Map(
  "paginate" -> true
)

macros = Seq(
  macro("test") { (dis, blocks) =>
    <div class="test">
      { dis.toXHTML(blocks) }
    </div>
  }
)
"""

  val input = new File("test.story")

  override def afterAll(config: Map[String, Any]) {
    val testScala = new File(destination, "src/main/scala/TestTemplate.scala")
    List(testScala, input) foreach (f => if (f.exists) f.delete)
  }

  "Test generator" should "generate the correct source" in {
    val generator = new StoryCodeGenerator { 
      val key = "test"
      val source = story 
    }

    val source = generator.generate()

    source.contains("TestTemplate") should be === true
  }

  "File Reader" should "generate source from a file" in {
    import java.io.FileWriter

    val writer = new FileWriter(input)
    writer.write(story)
    writer.close()

    val testgenerator = new StoryFileReader {
      val key = "test"
      val file = input
    }

    val source = testgenerator.generate()

    source.contains("TestTemplate") should be === true
  }

  "File Writer" should "generate a file from a source" in {
    val generator = new FileGeneration {
      val key = "test"
      val source = story
      val path = destination
    }

    generator.write()

    new File(destination, "/src/main/scala/TestTemplate.scala") should be ('exists)
  }
}
