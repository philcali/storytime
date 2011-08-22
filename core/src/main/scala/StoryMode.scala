package storytime

import com.tristanhunt.knockoff.{Discounter, Block}

object StoryMode {
  val meta = collection.mutable.HashMap[String, Any]()

  val preprocessors = collection.mutable.ListBuffer[StoryPreprocessor]()

  // Default Macros
  val macros = collection.mutable.ListBuffer[StoryHandler]()

  // meta ++= Seq (
  //   "pagination" -> "true"
  // )

  // macros += macro("cool-dude") { (dis, blocks) => 
  //    <div class="cool-dude">
  //      dis.toXHTML(blocks)
  //    </div>
  // }
  def macro(k: String)(handler: (Discounter, Seq[Block]) => xml.Node) = {
    new StoryHandler {
      val key = k

      def handle(discounter: Discounter, blocks: Seq[Block]) = 
        handler(discounter, blocks)
    }
  }

  // preprocessors += preprocessor("boilerplate") { _ =>
  //    "My name is Philip Cali"
  // }
  def preprocessor(k: String)(pro: (String) => String) = {
    new StoryPreprocessor {
      val key = k

      def preprocess(contents: String) = pro(contents)
    }
  }
}

