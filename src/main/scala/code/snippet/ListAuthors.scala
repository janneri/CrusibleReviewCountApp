package code.snippet

import code.util.CrusibleClient
import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import Helpers._
import util._
import Helpers._

class ListAuthors {
  
    val dateformat = new java.text.SimpleDateFormat("dd.mm.yyyy")
  
    def listAuthors = {
      
      val reviewsPerAuthor = CrusibleClient.getReviewsPerAuthor
      
      "#authorCount" #> reviewsPerAuthor.length & 
      ".author *" #> reviewsPerAuthor.map(author =>
          ".name *" #> author._1 &
          ".reviewCount *" #> author._2
      )
    }
}