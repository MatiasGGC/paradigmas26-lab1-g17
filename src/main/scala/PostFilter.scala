import Formatters.Post

object PostFilter{ //usamos esto para agrupar funciones relacionadas sin necesidad de crear instancias //subreddit, title, selftext, created

    val esEspacio: Char => Boolean =
    c => c == ' ' || c == '\n' || c == '\t'

    val quitarEspacios: String => String =
        texto => texto.filter(c => !esEspacio(c))

    val tieneContenidoReal: Option[String] => Boolean =
        textoOpt => textoOpt.exists(texto => quitarEspacios(texto).nonEmpty)

    val esValido: Post => Boolean = 
        post => tieneContenidoReal(post._2) && tieneContenidoReal(post._3)

    val filtrarPost: Option[List[Post]] => Option[List[Post]] =
        postsOpt => postsOpt.map(posts => posts.filter(esValido))

}
