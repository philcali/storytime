package storytime

import com.tristanhunt.knockoff._
import pamflet.{ FencedDiscounter, FencedChunkParser}
import util.parsing.input.{ Position }

trait StoryDiscounter extends FencedDiscounter {
  override def newChunkParser = 
    new ChunkParser with StoryChunkParser

  override def blockToXHTML: Block => xml.Node = block => block match {
    case StoryBlock(key, children, _) => 
      StoryMode.macros.find(_.key == key)
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
