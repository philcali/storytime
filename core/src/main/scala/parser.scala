package storytime

import com.tristanhunt.knockoff._
import DefaultDiscounter.{ toText }
import pamflet.{ FencedDiscounter, FencedChunkParser}
import util.parsing.input.{ Position }

import StoryKeys.{ macros, embed, output, defaultTemplate }

import StoryLoader.{ loadResource, copyBytes, copyFile }

import org.apache.commons.codec.binary.Base64.{
  encodeBase64
}

import java.io.File

class StoryPage (blocks: Seq[Block], val contents: xml.Node, val number: Int) {
  lazy val title = {
    blocks.find(_.isInstanceOf[Header]).map(h => toText(Seq(h)).trim)
  }
}

trait StoryDiscounter extends FencedDiscounter { self =>
  val mode: StoryMode

  lazy val embedded = mode.getOrElse(embed, false)

  lazy val location = mode.getOrElse(output, "converted")

  lazy val template = mode.getOrElse(defaultTemplate, "default")

  override def newChunkParser = 
    new ChunkParser with StoryChunkParser

  override def blockToXHTML: Block => xml.Node = block => block match {
    case StoryBlock(key, children, _) => 
      mode.getOrElse(macros, Nil).find(_.key == key)
                      .getOrElse(UndefinedHandler).handle(self, children)
    case _ => super.blockToXHTML(block)
  }

  override def imageLinkToXHTML: (Seq[Span], String, Option[String]) => xml.Node = {
    (spans, url, title) =>
      // Test for relativity
      val in = loadResource(template, url)
      
      val outUrl = if (in.isDefined && embedded) {
        // base 64 encode image data
        val bytes = copyBytes(in.get)

        val extension = url.substring(url.lastIndexOf("."), url.length)

        val data = new String(encodeBase64(bytes), "UTF8")

        "data:image/%s;base64,%s".format(extension, data)
      } else {
        val out = new File(location).getAbsolutePath
        val relative = new File(".").getAbsolutePath
 
        if (in.isDefined && out != relative) 
          copyFile(in.get, location + "/" +url)

        url
      }

      <img src={ outUrl } title={ title.getOrElse(null) }
             alt={ spans.map( spanToXHTML(_) ) } ></img>
  }

  override def linkToXHTML: ( Seq[Span], String, Option[String] ) => xml.Node = {
    (spans, url, title) =>
      loadResource(template, url).map { in =>
        val path = location + "/" + url
        val file = new File(path)

        if (!file.exists) copyFile(in, path)
      }

      <a href={ escapeURL(url) } title={ title.getOrElse(null) }>{
        spans.map( spanToXHTML(_) )
      }</a> 
  }
}

trait StoryChunkParser extends FencedChunkParser {
  val keyId = """@(.*)\s?\n""".r

  override def chunk = storykey | super.chunk 

  def unendingTextLine = 
    """(?!\@end\s?)[^\n]+\n""".r ^^ { TextChunk(_) }

  def storykey = keyId ~ rep1(unendingTextLine | emptyLine) <~ opt("@end") ^^ { 
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

case class StoryBlock (
  val key: String,
  val blocks: Seq[Block],
  val position: Position
) extends Block with StoryKey

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
