package storytime

import com.tristanhunt.knockoff.{Block, Discounter}

trait StoryKey {
  val key: String
}

trait StoryPreprocessor extends StoryKey {
  lazy val reg = ("""\[!\s?""" + key + """\s?\]""").r

  def preprocess(contents: String): String
  def process(contents: String) = 
    reg.replaceAllIn(contents, preprocess(contents))
}

trait StoryHandler extends StoryKey {
  def handle(dicounter: Discounter, blocks: Seq[Block]): xml.Node
}

trait StoryTemplate extends StoryKey {
  def template(data: Seq[xml.Node]): xml.Node
}

object UndefinedHandler extends StoryHandler {
  val key = "undefined"
  
  def handle(discounter: Discounter, blocks: Seq[Block]) = 
    discounter.toXHTML(blocks)
}
