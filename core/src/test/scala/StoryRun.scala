package storytime
package test

object StoryRun {
  def main(args: Array[String]) {
    StoryLoader.loadedJars.foreach(println)
  }
}
