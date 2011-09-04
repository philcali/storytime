package storytime
package default

import StoryKeys._

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
      output := "converted",
      macros := Seq(new BuildHandler)
    )
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
              {page.contents}
            </article>
          )}
        </section>
      </body>
    </html>
  }
}
