package storytime
package test

import org.scalatest.{FlatSpec, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers

import java.io.File

class Generator extends FlatSpec with ShouldMatchers with BeforeAndAfterAll {
  val destination = new File(".", "test")

  val story = 
"""
import custom._
import CustomKeys._

index := "index.html"

paginate := true

macros += macro("test") { (dis, blocks) =>
  <div class="test">
    { dis.toXHTML(blocks) }
  </div>
}
"""

  val inputed = new File("test.story")

  override def afterAll(config: Map[String, Any]) {

    def recurse(f: File) {
      if (f.isDirectory)
        f.listFiles.filter(!_.getName.startsWith(".")).foreach(recurse)
      
      f.delete
    }

    List(destination, inputed) foreach (recurse)
  }

  "Test generator" should "generate the correct source" in {
    val generator = new StoryCodeGenerator { 
      val key = "test"
      val source = story 
    }

    val template = generator.generate()

    template.source.contains("TestTemplate") should be === true
  }

  "File Reader" should "generate source from a file" in {
    import java.io.FileWriter

    val writer = new FileWriter(inputed)
    writer.write(story)
    writer.close()

    val testgenerator = new StoryFileReader {
      val key = "test"
      def input = inputed
    }

    val template = testgenerator.generate()

    template.source.contains("TestTemplate") should be === true
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

  "A SBT template" should "find the project refs" in {
    val imports = List("import custom._", "import CustomKeys._")

    val template = sbtTemplate(imports, story)

    template.dependencies should be === List("custom", "CustomKeys")
  }
}
