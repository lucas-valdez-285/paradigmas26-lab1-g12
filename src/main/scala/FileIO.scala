import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._

object FileIO {
  type Subscription = (String, String)

  def readSubscriptions(path: String): List[Subscription] = {
    try {
      val source = Source.fromFile(path)
      val content = source.mkString
      source.close()

      implicit val formats: DefaultFormats.type = DefaultFormats
      val json = parse(content)

      val subscriptions = json.children.map { sub =>
        val name = (sub \ "name").extract[String]
        val url = (sub \ "url").extract[String]
        (name, url)
      }
      subscriptions
    } catch {
      case _: Exception => Nil
    }
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