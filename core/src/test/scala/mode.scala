package storytime
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import StoryKeys._
import StoryMode._

object TestTemplate extends StoryTemplate {
  val key = "test"

  def story() = StoryMode (Seq(
    paginate := false,
    embed := false,

    macros += macro("test") { (dis, blocks) =>
      <div class="test">
        { dis.toXHTML(blocks) }
      </div>
    },

    preprocessors += preprocessor("boilerplate") { _ =>
      "My name is Philip Cali"
    }
  ))

  def template(data: TemplateData) = {
    <html>
      <body>
        <h1>Here is the meta</h1>
        <ul>
          { data.mode.meta.map { p =>
              val (k, v) = (p.meta.key, p.value)
              <li><strong>{k}</strong>: {v.toString} </li>
          }}
        </ul>
        <section>
          {data.pages.map( page =>
            <article>
              {page}
            </article>
          )}
        </section>
      </body>
    </html>
  }
}

class StoryTest extends FlatSpec with ShouldMatchers {
  val testMarkdown =
"""
# Welcome

@boilerplate

I hope you find this as interesting as I do...

@page

@test
This is stuff in here **bolded** or _italicized_
@end

It might even be stuff wrapped in a test div!
"""

  "Converter" should "handle the appropriate mode" in {
    val book = Converter(TestTemplate.story).convert(testMarkdown)

    book.pages.length should be === 2

    book.pages(0).title should be === Some("Welcome")
    book.pages(1).title should be === None

    book.pages(1).contents.toString.contains("class=\"test\"") should be === true
  }

  "StoryLoader" should "find the Test template" in {
    val clazz = StoryLoader.loadClass("test").get

    TestTemplate.story.get(paginate) should be === clazz.mode.get(paginate)
  }

}
