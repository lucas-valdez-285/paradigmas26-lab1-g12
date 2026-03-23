import PostParser._

object Main extends App {

  val subscriptions: List[FileIO.Subscription] = FileIO.readSubscriptions("subscriptions.json")


  val allPosts: List[Post] = subscriptions.flatMap {
    case (name, url) =>
      FileIO.downloadFeed(url) match {
        case Some(json) => PostParser.parsePosts(json, name)
        case None => List()
      }
  }

  allPosts.foreach(println)
}
