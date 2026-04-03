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

  type Post = (String, String, String, String, Int, String)

  def parsePosts(json: String, subreddit: String): Option[List[Post]] = {
    implicit val formats: DefaultFormats.type = DefaultFormats


    val parsed = parse(json)


    val children = (parsed \ "data" \ "children")


    Some(children.children.flatMap { post =>
      val data = post \ "data"


      val titleOpt = (data \ "title").extractOpt[String]
      val textOpt = (data \ "selftext").extractOpt[String]
      val dateOpt = (data \ "created_utc").extractOpt[Double]
      val scoreOpt = (data \ "score").extractOpt[Int]
      val urlOpt = (data \ "url").extractOpt[String]
      
      (titleOpt, textOpt, dateOpt, scoreOpt, urlOpt) match {
        case (Some(title), Some(text), Some(date), Some(score), Some(url))
            if title.trim.nonEmpty && text.trim.nonEmpty =>

            val formattedDate = Formatters.formatDateFromUTC(date.toLong)
            List((subreddit, title, text, formattedDate, score, url))

        case _ => Nil
      }
    })
  }
}

object TextProcessing {

  val stopwords: Set[String] = Set(
    "the", "about", "above", "after", "again", "against", "all", "am", "an",
    "and", "any", "are", "aren't", "as", "at", "be", "because", "been",
    "before", "being", "below", "between", "both", "but", "by", "can't",
    "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't",
    "doing", "don't", "down", "during", "each", "few", "for", "from", "further",
    "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd",
    "he'll", "he's", "her", "here", "here's", "hers", "herself", "him",
    "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if",
    "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me",
    "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off",
    "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves",
    "out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's",
    "should", "shouldn't", "so", "some", "such", "than", "that", "that's",
    "their", "theirs", "them", "themselves", "then", "there", "there's",
    "these", "they", "they'd", "they'll", "they've", "this", "those",
    "through", "to", "too", "under", "until", "up", "very", "was", "wasn't",
    "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what",
    "what's", "when", "when's", "where", "where's", "which", "while", "who",
    "who's", "whom", "why", "why's", "with", "won't", "would",
    "wouldn't", "you", "you'd", "you'll", "you're", "you've", "your", "yours",
    "yourself", "yourselves", "re"
  )

  private def tokenize(text: String): List[String] =
    text.split("[^\\p{L}\\p{N}']+").toList.filter(_.nonEmpty)

  private def normalize(word: String): String = word.toLowerCase

  private def isCapitalized(word: String): Boolean =
    word.headOption.exists(_.isUpper)

  private def relevantWordsFromText(text: String): List[String] =
    tokenize(text)
      .filter(isCapitalized)
      .map(normalize)
      .filterNot(stopwords.contains)

  private def countWords(words: List[String]): List[(String, Int)] =
    words
      .groupBy(identity)
      .toList
      .map { case (word, occurrences) => word -> occurrences.size }
      .sortBy { case (word, count) => (-count, word) }

  def wordFrequencies(posts: List[PostParser.Post]): List[(String, Int)] =
    countWords(
      posts.flatMap { case (_, _, selftext, _, _, _) =>
        relevantWordsFromText(selftext)
      }
    )

  def wordFrequenciesBySubreddit(
      posts: List[PostParser.Post]
  ): List[(String, List[(String, Int)])] =
    posts
      .groupBy(_._1)
      .toList
      .sortBy(_._1)
      .map { case (subreddit, subredditPosts) =>
        subreddit -> wordFrequencies(subredditPosts)
      }

  def totalScore(posts: List[PostParser.Post]): Int =
    posts.foldLeft(0) { case (accumulator, (_, _, _, _, score, _)) =>
      accumulator + score
    }

  def postsBySubreddit(posts: List[PostParser.Post]): List[(String, List[PostParser.Post])] =
    posts
      .groupBy(_._1)
      .toList
      .sortBy(_._1)

  def formatPost(post: PostParser.Post): String = {
    val (_, title, _, date, _, url) = post
    s"- $title | $date | $url"
  }

  def reportBySubreddit(posts: List[PostParser.Post]): String = {
    postsBySubreddit(posts).map { case (subreddit, subredditPosts) =>
      val scoreTotal = totalScore(subredditPosts)
      val topWords = wordFrequencies(subredditPosts).take(5)
      val topPosts = subredditPosts.take(5)

      val wordsSection = if (topWords.isEmpty) {
        "- Sin palabras capitalizadas no stopwords"
      } else {
        topWords.map { case (word, count) => s"- $word: $count" }.mkString("\n")
      }

      val postsSection = if (topPosts.isEmpty) {
        "- Sin posts disponibles"
      } else {
        topPosts.map(formatPost).mkString("\n")
      }

      s"## $subreddit\n- Score total: $scoreTotal\n\n### Palabras frecuentes\n$wordsSection\n\n### Primeros 5 posts\n$postsSection"
    }.mkString("\n\n")
  }
}