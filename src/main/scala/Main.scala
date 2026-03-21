object Main {
  def main(args: Array[String]): Unit = {
    val header = s"Reddit Post Parser\n${"=" * 40}"
    println(header)

    val subscriptionsOpt = FileIO.readSubscriptions("subscriptions.json")

    val allPosts: List[(String, String)] = subscriptionsOpt match {
      case Some(subscriptions: List[(String, String)]) =>
        subscriptions.flatMap { case (name, url) =>
          println(s"Fetching posts from: $name")

          FileIO.downloadFeed(url) match {
            case Some(posts) =>
              Some((name, posts))
            case None =>
              println(s"Error descargando feed: $name")
              None
          }
        }

      case None =>
        println("Error leyendo suscripciones o JSON mal formado")
        List()
    }

    val output = allPosts
      .map { case (name, posts) =>
        Formatters.formatSubscription(name, posts)
      }
      .mkString("\n")

    println(output)
  }
}