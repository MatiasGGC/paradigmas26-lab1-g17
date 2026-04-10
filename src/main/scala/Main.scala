object Main {

  type Subscription = (String, String) 
  type Post = (Option[String], Option[String], Option[String], Option[String], Option[Int], Option[String]) 
  
  val defaultSubscriptionsPath = "subscriptions.json"

  def info(msg: String): Unit =
    println(s"[info] $msg")

  def error(msg: String): Unit =
    Console.err.println(s"[error] $msg")     // lo mismo que println() pero para errores

  def loadPosts(subscription: Subscription): Option[(String, List[Post])] = {
    val (name, url) = subscription

    FileIO.downloadFeed(url) match {
      case None =>
        error(s"Could not download feed for '$name' ($url).")
        None

      case Some(rawJson) =>
        Formatters.formatSubscription(url, Some(rawJson)) match {
          case None =>
            error(s"Could not parse feed for '$name' ($url).")
            None

          case Some(posts) =>
            val filteredPosts = PostFilter.filtrarPost(Some(posts)).getOrElse(Nil)

            if (filteredPosts.isEmpty) {
              info(s"Subscription '$name' produced no valid posts after filtering.")
            }

            Some(name -> filteredPosts)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    val subscriptionsPath = args.headOption.getOrElse(defaultSubscriptionsPath)

    println("Reddit Post Parser")
    println("=" * 40)

    FileIO.readSubscriptions(subscriptionsPath) match {
      case None =>
        error(s"Could not read '$subscriptionsPath', or the file contains no valid subscriptions.")

      case Some(Nil) =>
        error(s"'$subscriptionsPath' is empty or every entry is invalid.")

      case Some(subscriptions) =>
        val allPostsBySubreddit: Map[String, List[Post]] =
          subscriptions.flatMap(loadPosts).toMap

        if (allPostsBySubreddit.isEmpty) {
          error("No feeds could be downloaded or parsed successfully.")
        } else {
          Informe.printReport(subscriptions, allPostsBySubreddit)
        }
    }
  }
}
