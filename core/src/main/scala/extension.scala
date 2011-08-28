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

  val paginate = StoryMetaKey[Boolean]("paginate", 
    "Splits up a project into multiple files")

  val embed = StoryMetaKey[Boolean]("embed", 
    "Tries to embed resources into converted output")
  
  val separator = StoryMetaKey[String]("separator", 
    "This is used to determine the page separators")

  val title = StoryMetaKey[String]("title", "Title of the book")

  val resources = StoryMetaKey[Seq[String]]("resources", 
    "Relative or absolute path(s) to conversion assests")

  val output = StoryMetaKey[String]("output", 
    "Relative or absolute path to an output folder.")

  val templates = StoryMetaKey[Seq[String]]("templates", 
    "Load template macros and processors in scope")

  def resource(name: String) = { 
    this.getClass.getClassLoader.getResourceAsStream(key + "/" + name)  
  }

  def template(data: TemplateData): xml.Node

  def apply(book: StoryBook) = {
    val meta = book.mode.meta

    val pagination: Boolean = meta.getOrElse("paginate", false)
    val embeded: Boolean = meta.getOrElse("embed", false)

    val titled: String = meta.getOrElse("title", "Storytime")
    val loc: String = meta.getOrElse("output", "converted")

    // TODO: Incorporate regexes
    val res = meta.getOrElse[Seq[String]]("resources", List()).map{ f => 
      new File(f)
    }

    val data = TemplateData(meta, res, _: Seq[StoryPage], titled, embeded)

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
  meta: StoryKeys, 
  resources: Seq[File], 
  pages: Seq[StoryPage], 
  title: String = "Storytime", 
  embed: Boolean = false
)

trait StoryBoard extends StoryKey with ImplicitKeys {
  // Override for setting values in templates
  def story(): StoryMode
}

object UndefinedHandler extends StoryHandler {
  val key = "undefined"
  
  def handle(discounter: Discounter, blocks: Seq[Block]) = 
    discounter.toXHTML(blocks)
}
