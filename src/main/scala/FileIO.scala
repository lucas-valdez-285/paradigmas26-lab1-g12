import scala.io.Source


object FileIO {


  type Subscription = (String, String)


  def readSubscriptions(): List[Subscription] = {
    List(
      ("scala", "https://www.reddit.com/r/scala/.json?count=10"),
      ("learnprogramming", "https://www.reddit.com/r/learnprogramming/.json?count=10")
    )
  }


  def downloadFeed(url: String): Option[String] = {
    try {
      val source = Source.fromURL(url)
      val content = source.mkString
      source.close()
      Some(content)
    } catch {
      case _: Exception => None
    }
  }
}
