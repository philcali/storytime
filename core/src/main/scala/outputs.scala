package storytime

trait FileOutput {
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

}

object DefaultTemplate extends StoryTemplate {
  val key = "default"

  def story() = StoryMode (
    meta = Seq(
      paginate := false,
      resources := Seq(
        "assests/slides.js", 
        "assests/prettify.js", 
        "assests/styles.css"
      ),
      output := "converted"
    ),
 
    macros = Seq(new BuildHandler)
  )

  def template(data: TemplateData) = {
    <html>
      <head>
        <title>{ data.title }</title>

        <meta charset="utf-8"/>
        <script type="text/javascript" src="assests/slides.js"></script>
      </head>
      <body style="display: none">
        <section class="slides layout-regular template-default">
          {data.pages.map (page =>
            <article>
              {page}
            </article>
          )}
        </section>
      </body>
    </html>
  }
}
