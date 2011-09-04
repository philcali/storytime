package storytime
package test

import org.scalatest.{ FlatSpec, BeforeAndAfterAll }
import org.scalatest.matchers.ShouldMatchers

import StoryKeys._

class TemplateTest extends FlatSpec with ShouldMatchers with BeforeAndAfterAll {

  val markdown =
"""
# Story time

---

### Better hop to it

More about this and that

---

### Navigation

Use the arrow keys to move forward or backward

---

### Custom Macro's

The following code will be hidden:

[!build]
```scala
object Hello extends App {
  println("hello world!")
}
```
[!end]


[!quote]
Glamor Glamor! More power to you!
[!end]

[!author]
Philip Cali
[!end]
"""

  "DefaultTemplate" should "be loadable via StoryLoader" in {
    val template = StoryLoader.loadClass("default")

    template.get.key should be === "default"
  }

  it should "be launchable after loaded" in {
    import StoryMode._

    val template = StoryLoader.loadClass("default").get

    val oldmode = template.mode
    val mode = oldmode.copy(meta = oldmode.meta.inited ++ Seq(
      separator := "---",
      paginate := true
    ), macros = oldmode.macros ++ Seq(

      textMacro("quote") { rest =>
        <q>
          { rest }
        </q>
      },

      textMacro("author") { rest =>
        <div class="author">
          { rest } 
        </div>
      }
    ))

    template(Converter(mode).convert(markdown))

    new java.io.File("converted/index.html") should be ('exists)
    new java.io.File("converted/assests") should be ('exists)
  }
}
