package storytime

import com.tristanhunt.knockoff.{Block, Discounter}
import java.io.File

import StoryKeys._

import StoryLoader.{ copyFile, copyBytes, loadResource }

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

trait StoryTemplate extends StoryKey with StoryBoard { parent =>

  case class TemplateData(mode: StoryMode, pages: Seq[StoryPage]) {
    val title = mode.getOrElse(StoryKeys.title, "Storytime")

    val embedded = mode.getOrElse(embed, false)

    val base = mode.getOrElse(output, "converted")

    def headerResources = {
      val res = mode.getOrElse(resources, Nil)

      val mimeMap = new javax.activation.MimetypesFileTypeMap

      val loaded = res.map { r => 
        // is Loadable
        loadResource(key, r).map { in =>

          val mime = if (r.endsWith("js")) 
            "text/javascript"
          else if (r.endsWith("css"))
            "text/css"
          else mimeMap.getContentType(r)

          val resource = Resource(r, mime, embedded)

          val bytes = if (embedded) {
            Some(copyBytes(in))
          } else {
            copyFile(in, base + "/" + r)
            None
          }

          headerToHTML(resource, bytes)
        }
      }

      loaded.filter(_.map(_ => true).orElse(Some(false)).get).map(_.get)
    }

    def headerToHTML(resource: Resource, contents: Option[Array[Byte]]) = resource match {
      case Resource(name, mime, embeded) =>
        val stripScript = """</script>""".r

        val utf = contents.map { new String(_, "UTF8") }

        mime match {
          case "text/css" =>
            if (embeded)
              <style type={mime}>{xml.Unparsed(utf.get)}</style>
            else
              <link rel="stylesheet" type={mime} href={name}/>
          case _ => 
            if (embeded)
              <script type={mime}>
                {xml.Unparsed(stripScript.replaceAllIn(utf.get, ""))}
              </script>
            else
              <script type={mime} src={name}></script>
        }
    }
  }

  def apply(book: StoryBook) = {
    val meta = book.mode

    val pagination = meta.getOrElse(paginate, false)

    val loc = meta.getOrElse(output, "converted")

    val location = new File(loc)

    if (!location.exists) {
      location.mkdirs()
    }

    val data = TemplateData(meta, _: Seq[StoryPage])

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

    val bytes = result.getBytes("UTF8")   

    val out = new java.io.FileOutputStream(location) 
    out.write(bytes, 0, bytes.length)
    out.close()
  }

  def template(data: TemplateData): xml.Node
}

case class Resource(name: String, contentType: String, embedded: Boolean)

trait StoryBoard extends StoryKey {
  // Override for setting values in templates
  def story(): StoryMode
}

object UndefinedHandler extends StoryHandler {
  val key = "undefined"
  
  def handle(discounter: Discounter, blocks: Seq[Block]) = 
    discounter.toXHTML(blocks)
}
