package storytime
package default

import StoryKeys._

object DefaultTemplate extends StoryTemplate {
  val key = "default"

  def story() = Seq(
    paginate := false,
    resources ++= Seq(
      "assests/prettify.js",
      "assests/styles.css",
      "assests/slides.js"
    ),
    output := "converted",
    macros += new BuildHandler
  )

  def template(data: TemplateData) = {
    val (headerResources, otherResources) = separateResources(data.resources)

    <html>
      <head>
        <title>{ data.title }</title>

        <meta charset="utf-8"/>
        { headerResources.map(headerToHTML) }
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
