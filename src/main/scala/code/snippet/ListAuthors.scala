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
  
    def listAuthors = {
      
      val reviewsPerAuthor = CrusibleClient.getReviewsPerAuthor
      
      "#authorCount" #> reviewsPerAuthor.length & 
      ".author *" #> reviewsPerAuthor.zipWithIndex.map(author =>
          ".rank *" #> author._2 &
          ".name *" #> author._1._1 &
          ".reviewCount *" #> author._1._2
      )
    }
}