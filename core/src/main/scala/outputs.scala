package storytime

trait FileOutput extends HtmlOutput {
  import java.io._
  
  val output: String

  def outsource(name: String) = {
    val filename = output + "/" + name
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

  override def apply(converted: Seq[xml.Node]) = {
    resources foreach (res => copy(resource(res), outsource(res)))
   
    val result = super.apply(converted)
 
    val writer = new FileWriter(output + "/index.html")
    writer.write(result)
    writer.close

    "Success"
  }
}

trait HtmlOutput extends StoryTemplate {
  def resources: List[String]

  def resource(name: String) = 
    this.getClass.getClassLoader.getResourceAsStream(key + "/" + name)

  def apply(converted: Seq[xml.Node]) = {
    "<!DOCTYPE>\n" + template(converted).toString 
  }
}

// TODO: create a Resource module for dynamic resources
object DefaultTemplate extends StoryTemplate with FileOutput {
  val key = "default"
  val output = "converted"

  def resources = List(
    "assests/slides.js", 
    "assests/prettify.js", 
    "assests/styles.css"
  )

  def template(articles: Seq[xml.Node]) = {
    <html>
      <head>
        <title>StoryBoard</title>

        <meta charset="utf-8"/>
        <script type="text/javascript" src="assests/slides.js"></script>
      </head>
      <body style="display: none">
        <section class="slides layout-regular template-default">
          {articles.map (article =>
            <article>
              {article}
            </article>
          )}
        </section>
      </body>
    </html>
  }
}
