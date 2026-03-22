import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatter


object Formatters {


  def formatDateFromUTC(timestamp: Long): String = {
    val instant = Instant.ofEpochSecond(timestamp)
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    formatter.format(instant.atZone(ZoneId.systemDefault()))
  }
}


import org.json4s._
import org.json4s.jackson.JsonMethods._

object PostParser {

  type Post = (String, String, String, String)

  def parsePosts(json: String, subreddit: String): List[Post] = {
    implicit val formats: DefaultFormats.type = DefaultFormats


    val parsed = parse(json)


    val children = (parsed \ "data" \ "children")


    children.children.flatMap { post =>
      val data = post \ "data"


      val titleOpt = (data \ "title").extractOpt[String]
      val textOpt = (data \ "selftext").extractOpt[String]
      val dateOpt = (data \ "created_utc").extractOpt[Double]


      (titleOpt, textOpt, dateOpt) match {
        case (Some(title), Some(text), Some(date)) =>
          val formattedDate = Formatters.formatDateFromUTC(date.toLong)
          Some((subreddit, title, text, formattedDate))


        case _ => None
      }
    }
  }
}