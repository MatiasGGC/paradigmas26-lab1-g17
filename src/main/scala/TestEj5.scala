
object TestEj5 {

  def main(args: Array[String]): Unit = {

    val subscriptions = FileIO.readSubscriptions("subscriptions.json")

    val allPosts =
      subscriptions.map { case (name, url) =>
        println(s"\nFetching posts from: $url")

        val json = FileIO.downloadFeed(url)

        val posts = Formatters.formatSubscription(url, json)

        println(s"Posts obtenidos: ${posts.length}")

        (name, posts)
      }

    // 
    allPosts.foreach { case (subreddit, posts) =>
      println(s"\n===== $subreddit =====")

      val freqs = Ejercicio5.wordFrequencies(posts)

      println("\nTop palabras:")
      freqs.take(10).foreach { case (word, count) =>
        println(s"$word -> $count")
      }
    }
  }
}