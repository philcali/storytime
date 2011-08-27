package storytime

import com.tristanhunt.knockoff.{Block, Discounter}
import java.io.File

trait StoryKey {
  val key: String
}

trait StoryPreprocessor extends StoryKey {
  lazy val reg = ("""\[!\s?""" + key + """\s?\]""").r

  def preprocess(contents: String): String
  def process(contents: String) = 
    reg.replaceAllIn(contents, preprocess(contents))
}

trait StoryHandler extends StoryKey {
  def handle(dicounter: Discounter, blocks: Seq[Block]): xml.Node
}

trait StoryTemplate extends StoryKey with StoryBoard {

  case class StoryArgument(name: String, description: String = "")

  def resource(name: String) = { 
    this.getClass.getClassLoader.getResourceAsStream(key + "/" + name)  
  }

  def arguments = Seq (
    StoryArgument("paginate", "Splits up a project into multiple files"),
    StoryArgument("embed", "Tries to embed resources into converted output"),
    StoryArgument("sep", "This is used to determine the page separators"),
    StoryArgument("title", "Title of the book"),
    StoryArgument("resources", "Relative or absolute path(s) to conversion assests"),
    StoryArgument("output", "Relative or absolute path to an output folder."),
    StoryArgument("templates", "Load template macros and processors in scope")
  )

  def template(data: TemplateData): xml.Node

  def apply(book: StoryBook) = {
    val meta = book.mode.meta

    val pagination = meta.getOrElse("paginate", "false").toString.toBoolean
    val embed = meta.getOrElse("embed", "false").toString.toBoolean

    val title = meta.getOrElse("title", "Storytime").toString
    val loc = meta.getOrElse("output", "converted").toString

    val res = meta.getOrElse("resources", List()).asInstanceOf[List[String]].map{f => 
      new File(f.toString)
    }

    val data = TemplateData(meta, res, _: Seq[StoryPage], title, embed)

    if (pagination) {
      book.pages.foreach { page => 
        val file = "%s/page%d.html".format(loc, page.number)
        output(template(data(Seq(page))), file)
      }
    } else {
      output(template(data(book.pages)), "%s/index.html".format(loc))
    }
  }

  def output(converted: xml.Node, location: String) {
    val result = "<!DOCTYPE html>\n" + converted.toString 
    
    val writer = new java.io.FileWriter(location)
    writer.write(result)
    writer.close()
  }
}

case class TemplateData(
  meta: Map[String, Any],
  resources: Seq[File], 
  pages: Seq[StoryPage], 
  title: String = "Storytime", 
  embed: Boolean = false
)

trait StoryBoard extends StoryKey {
  // Override for setting values in templates
  def story(): StoryMode
}

object UndefinedHandler extends StoryHandler {
  val key = "undefined"
  
  def handle(discounter: Discounter, blocks: Seq[Block]) = 
    discounter.toXHTML(blocks)
}
