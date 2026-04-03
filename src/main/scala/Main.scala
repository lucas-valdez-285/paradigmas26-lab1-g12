import PostParser._

object Main extends App {

  val subscriptions: List[FileIO.Subscription] = FileIO.readSubscriptions("subscriptions.json") match {
    case Some(subs) => subs
    case None => Nil
  }

  val allPosts: List[Post] = subscriptions.flatMap {
    case (name, url) =>
      FileIO.downloadFeed(url) match {
        case Some(json) => PostParser.parsePosts(json, name) match{
          case Some(post) => post
          case None => Nil
        }
        case None => List()
      }
  }

  println(TextProcessing.reportBySubreddit(allPosts))
}
