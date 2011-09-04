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

  def xmlMacro(k: String)(rest: xml.Node => xml.Node) = {
    new StoryHandler {
      val key = k
      
      def handle(discounter: Discounter, blocks: Seq[Block]) =
        rest(restToXml(discounter, blocks))
    }
  }

  def textMacro(k: String)(rest: String => xml.Node) = {
    new StoryHandler {
      val key = k
      
      def handle(discounter: Discounter, blocks: Seq[Block]) =
        rest(restToText(discounter, blocks))
    }
  }

  def preprocessor(k: String)(pro: (String) => String) = {
    new StoryPreprocessor {
      val key = k

      def preprocess(contents: String) = pro(contents)
    }
  }
}

case class StoryMode(meta: Seq[Meta[_]]) {
  lazy val keys = meta.map(_.meta)

  def get[A](key: StoryMetaKey[A]): Option[A] = get(key.key)

  def get[A](key: String): Option[A] = 
    meta.find(_.meta.key == key).map(_.value.asInstanceOf[A])

  def getOrElse[A](key: StoryMetaKey[A], default: A): A =
    getOrElse(key.key, default)

  def getOrElse[A](key: String, default: A): A =
    get[A](key).getOrElse(default)

  def ++ (otherSettings: Seq[Meta[_]]): StoryMode = {

    val adjusted = otherSettings.map {
      case m: OverwriteMeta[_] => m
      case m: AddSeqMeta[_] =>
        m.process(this.getOrElse(m.meta, Nil))
    }

    val adjustedKeys = adjusted.map(_.meta)

    val diff = keys diff adjustedKeys

    val originalMeta = diff.map(m => meta.find(_.meta == m).get)

    StoryMode(originalMeta ++ adjusted)
  }

  def ++ (otherMode: StoryMode): StoryMode = ++ (otherMode.meta)
}
