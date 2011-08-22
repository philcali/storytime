package storytime

import com.tristanhunt.knockoff._
import pamflet.{ FencedDiscounter, FencedChunkParser}
import util.parsing.input.{ Position }

import scala.io.Source.{fromFile => open}

trait StoryDiscounter extends FencedDiscounter {
  override def newChunkParser = 
    new ChunkParser with StoryChunkParser

  override def blockToXHTML: Block => xml.Node = block => block match {
    case StoryBlock(key, children, _) => 
      StoryMode.handlers.find(_.key == key)
                        .getOrElse(UndefinedHandler).handle(this, children)
    case _ => super.blockToXHTML(block)
  }
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

  // Default Macros
  val handlers = collection.mutable.ListBuffer[StoryHandler]()

  def macro(k: String)(handler: (Discounter, Seq[Block]) => xml.Node) = {
    new StoryHandler {
      val key = k

      def handle(discounter: Discounter, blocks: Seq[Block]) = 
        handler(discounter, blocks)
    }
  }

  def preprocessor(k: String)(pro: (String) => String) = {
    new StoryPreprocessor {
      val key = k

      def preprocess(contents: String) = pro(contents)
    }
  }
}

object UndefinedHandler extends StoryHandler {
  val key = "undefined"
  
  def handle(discounter: Discounter, blocks: Seq[Block]) = 
    discounter.toXHTML(blocks)
}

class BuildHandler extends StoryHandler {
  val key = "build"

  def handle(discounter: Discounter, blocks: Seq[Block]) = blocks.headOption match {
    case Some(UnorderedList(items)) =>
      <ul class = "build">
        { discounter.toXHTML(items) }
      </ul>
    case Some(OrderedList(items)) =>
      <ol class = "build">
        { discounter.toXHTML(items) }
      </ol>
    case Some(_) => 
      <div class = "build">
        { discounter.toXHTML(blocks) }
      </div>
    case _ => discounter.toXHTML(blocks)
  } 
}

trait StoryHandler extends StoryKey {
  def handle(dicounter: Discounter, blocks: Seq[Block]): xml.Node
}

case class StoryBlock (
  val key: String,
  val blocks: Seq[Block],
  val position: Position
) extends Block with StoryKey

trait StoryKey {
  val key: String
}

trait StoryPreprocessor extends StoryKey {
  lazy val reg = ("""\[!\s?""" + key + """\s?\]""").r

  def preprocess(contents: String): String
  def process(contents: String) = 
    reg.replaceAllIn(contents, preprocess(contents))
}

object Pages extends StoryPreprocessor {
  val key = "page"
  
  def create(contents: String) = reg.split(contents)
  def preprocess(contents: String) = ""
}

// TODO: create Meta
object Converter extends StoryDiscounter {
  def apply(mdLocation: String) = {
    val contents = open(mdLocation).getLines.mkString("\n")

    val preprocessed = 
      StoryMode.preprocessors.foldLeft(contents) { (in, pre) =>
        pre.process(in)
      }

    val pages = Pages create preprocessed map (knockoff(_)) 

    pages map (toXHTML(_))
  }
}

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

trait StoryTemplate extends StoryKey {
  def template(data: Seq[xml.Node]): xml.Node
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
