package storytime

import com.tristanhunt.knockoff.{Block, Discounter}
import java.io.{
  File,
  InputStream,
  OutputStream,
  ByteArrayOutputStream,
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

  def headerToHTML(resource: Resource) = resource match {
    case Resource(name, mime, contents, embeded) =>
      val stripScript = """</script>""".r

      val utf = new String(contents, "UTF8")

      mime match {
        case "text/css" =>
          if (embeded)
            <style type={mime}>{xml.Unparsed(utf)}</style>
          else
            <link rel="stylesheet" type={mime} href={name}/>
        case _ => 
          if (embeded)
            <script type={mime}>
              {xml.Unparsed(stripScript.replaceAllIn(utf, ""))}
            </script>
          else
            <script type={mime} src={name}></script>
      }
  }

  def separateResources(res: Seq[Resource]) = {
    res.partition(_.contentType match {
      case "text/javascript" | "text/css" => true
      // consider all other resources (images and what not) non-header
      case _ => false
    })
  }

  def gatherResources(res: Seq[String], embeded: Boolean, loc: String) = {
    val mimeMap = new javax.activation.MimetypesFileTypeMap

    val loaded = res.map { r => 
      loadResource(r).map { in =>
        val base = loc + "/" + r

        val (out, toBytes) = if (embeded) { 
          val bytes = new ByteArrayOutputStream 
          (bytes, () => bytes.toByteArray)
        } else {
          (outsource(base), () => new Array[Byte](0))
        }

        copy(in, out)

        val mime = if (r.endsWith("js")) 
          "text/javascript"
        else if (r.endsWith("css"))
          "text/css"
        else mimeMap.getContentType(r)

        Resource(r, mime, toBytes(), embeded)
      }
    }

    loaded.filter(_.map(_ => true).orElse(Some(false)).get).map(_.get)
  }

  def template(data: TemplateData): xml.Node

  def apply(book: StoryBook) = {
    val meta = book.mode

    val pagination = meta.getOrElse(paginate, false)
    val embeded = meta.getOrElse(embed, false)

    val titled = meta.getOrElse(title, "Storytime")
    val loc = meta.getOrElse(output, "converted")

    val location = new File(loc)
    if (!location.exists) {
      location.mkdirs()
    }

    val res = gatherResources(meta.getOrElse(resources, Nil), embeded, loc)

    val data = TemplateData(meta, _: Seq[StoryPage], titled, res)

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

    val out = new FileOutputStream(location) 
    out.write(bytes, 0, bytes.length)
    out.close()
  }
}

case class Resource(
  name: String,
  contentType: String,
  contents: Array[Byte],
  embed: Boolean
)

case class TemplateData(
  mode: StoryMode,
  pages: Seq[StoryPage],
  title: String = "Storytime",
  resources: Seq[Resource]
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
