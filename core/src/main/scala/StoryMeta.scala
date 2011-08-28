package storytime

case class StoryMetaKey[A](
  key: String, 
  description: String = "",
  required: Boolean = false) extends StoryKey {
  def := (value: A) = Meta[A](this, value)
}

case class Meta[A](meta: StoryMetaKey[A], value: A)

trait ImplicitKeys {
  implicit def seq2StoryKeys(inited: Seq[Meta[_]]) =
    StoryKeys(inited)
}

case class StoryKeys(inited: Seq[Meta[_]]) {
  def get[A](key: String) = 
    inited.find(_.meta.key == key).map(_.value.asInstanceOf[A])

  def getOrElse[A](key: String, default: A) =
    get[A](key).getOrElse(default)
}
