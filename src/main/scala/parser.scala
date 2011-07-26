package storytime

import com.tristanhunt.knockoff._
import pamflet.{ FencedDiscounter, FencedChunkParser}
import util.parsing.input.{ Position }

import scala.io.Source.{fromFile => open}

trait StoryDiscounter extends FencedDiscounter {
  override def newChunkParser = 
    new ChunkParser with StoryChunkParser
}

trait StoryChunkParser extends FencedChunkParser {
  val keyId = """\[!\s?(.*)\s?\]\n""".r

  override def chunk = storykey | super.chunk 

  def unendingTextLine = 
    """(?!\[\!\s?end\s?\])[^\n]+\n""".r ^^ { TextChunk(_) }

  def storykey = keyId ~ rep1(unendingTextLine | emptyLine) <~ opt("[!end]") ^^ { 
    case keyId(k) ~ contents => StoryChunk(k.trim, foldedString(contents)) 
  }

  private def foldedString( texts : List[ Chunk ] ) : String =
    ( "" /: texts )( (current, text) => current + text.content )
}

case class StoryChunk(key: String, content: String) extends Chunk {

  def appendNewBlock (list: collection.mutable.ListBuffer[Block],
                      remaining: List[ (Chunk, Seq[Span], Position) ],
                      spans: Seq[Span], position: Position,
                      discounter: Discounter) {

    val blocks = discounter.knockoff(content)
    list += StoryBlock(key, blocks, position)
  }
}

object StoryMode {
 val preprocessors = collection.mutable.ListBuffer[StoryPreprocessor]()
}

case class StoryBlock (
  val key: String,
  val blocks: Seq[Block],
  val position: Position
) extends Block

trait StoryPreprocessor {
  val key: String

  lazy val reg = ("""\[!\s?""" + key + """\s?\]""").r

  def process(contents: String): String
}

object Slides extends StoryPreprocessor {
  val key = "slide"
  
  def create(contents: String) = reg.split(contents)
  def process(contents: String) = reg.replaceAllIn(contents, "")
}

object Converter extends StoryDiscounter {
  def apply(mdLocation: String) = {
    val contents = open(mdLocation).getLines.mkString("\n")

    val preprocessed = 
      StoryMode.preprocessors.foldLeft(contents) { (in, pre) =>
        pre.process(in)
      }

    val slides = Slides create preprocessed map (knockoff(_)) 

    slides foreach (println)

    slides map (toXHTML(_))
  }
}

object Output {
  import java.io._

  val theme = "default"
  val output = "converted"

  val templated = """\#\{\s?(\w+)\s?\}\#""".r

  def resource(name: String) = 
    this.getClass.getClassLoader.getResourceAsStream(theme + "/" + name)

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

  def toArticles(contents: Seq[xml.Node]) = {
    <html>
      <head>
        <title>Presentation</title>

        <meta charset="utf-8"/>
        <script type="text/javascript" src="assests/slides.js"></script>
      </head>
      <body style="display: none">
        <section class="slides layout-regular template-default">
          {contents.map (article =>
            <article>
              {article}
            </article>
          )}
        </section>
      </body>
    </html>
  }

  def apply(converted: Seq[xml.Node]) = {
    val resources = List(
      "assests/slides.js", 
      "assests/prettify.js", 
      "assests/styles.css"
    )

    resources foreach (res => copy(resource(res), outsource(res)))
 
    val tpl = "<!DOCTYPE>\n" + toArticles(converted).toString 
 
    val writer = new FileWriter(output + "/index.html")
    writer.write(tpl)
    writer.close
  }
}
