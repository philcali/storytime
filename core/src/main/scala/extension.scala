package storytime

import com.tristanhunt.knockoff.{Block, Discounter}
import java.io.{
  File,
  InputStream,
  OutputStream,
  FileOutputStream
}

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
  def restToXml(dis: Discounter, blocks: Seq[Block]) = dis.toXHTML(blocks)

  def restToText(dis: Discounter, blocks: Seq[Block]) = dis.toText(blocks)

  def handle(dicounter: Discounter, blocks: Seq[Block]): xml.Node
}

trait StoryTemplate extends StoryKey with StoryBoard {

  def loadResource(name: String) = { 
    val stream = this.getClass.getClassLoader.getResourceAsStream(key + "/" + name)  
    if (stream == null) {
      import java.io.FileInputStream

      try {
        Some(new FileInputStream(name))
      } catch {
        case e: java.io.FileNotFoundException => None
      }
    } else {
      Some(stream)
    }
  }

  def outsource(filename: String) = {
    new File(filename.split("/").dropRight(1).mkString("/")).mkdirs
    new FileOutputStream(filename)
  }

  def copy(in: InputStream, out: OutputStream) {
    val buf = new Array[Byte](1024)
    in read(buf, 0, 1024) match {
      case n if n >= 0 => out.write(buf, 0, n); copy(in, out)
      case _ => in.close; out.close
    }
  }

  def template(data: TemplateData): xml.Node

  def apply(book: StoryBook) = {
    val meta = book.mode

    val pagination: Boolean = meta.getOrElse("paginate", false)
    val embeded: Boolean = meta.getOrElse("embed", false)

    val titled: String = meta.getOrElse("title", "Storytime")
    val loc: String = meta.getOrElse("output", "converted")

    val location = new File(loc)
    if (!location.exists) {
      location.mkdirs()
    }

    // TODO: Incorporate regexes
    val res = meta.getOrElse[Seq[String]]("resources", Nil)

    res.foreach(r => loadResource(r).map { loaded =>
      copy(loaded, outsource(loc + "/" + r))
    })

    val data = TemplateData(meta, _: Seq[StoryPage], titled, embeded)

    if (pagination) {
      book.pages.foreach { page => 
        val file = "%s/page%d.html".format(loc, page.number)
        outputConverted(template(data(Seq(page))), file)
      }
    } else {
      outputConverted(template(data(book.pages)), "%s/index.html".format(loc))
    }
  }

  def outputConverted(converted: xml.Node, location: String) {
    val result = "<!DOCTYPE html>\n" + converted.toString 
    
    val writer = new java.io.FileWriter(location)
    writer.write(result)
    writer.close()
  }
}

case class TemplateData(
  mode: StoryMode, 
  pages: Seq[StoryPage], 
  title: String = "Storytime", 
  embed: Boolean = false
)

trait StoryBoard extends StoryKey with Keys {
  // Override for setting values in templates
  def story(): StoryMode
}

object UndefinedHandler extends StoryHandler {
  val key = "undefined"
  
  def handle(discounter: Discounter, blocks: Seq[Block]) = 
    discounter.toXHTML(blocks)
}
