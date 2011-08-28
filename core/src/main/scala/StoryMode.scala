package storytime

import com.tristanhunt.knockoff.{Discounter, Block}

object StoryMode {
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

case class StoryMode (
  meta: StoryKeys = StoryKeys(Nil),
  macros: Seq[StoryHandler] = Nil,
  preprocessors: Seq[StoryPreprocessor] = Nil
)
