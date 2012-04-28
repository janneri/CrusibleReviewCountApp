package code.snippet

import code.util.CrusibleClient
import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import Helpers._
import util._
import Helpers._

class ListReviews {
  
    val dateformat = new java.text.SimpleDateFormat("dd.MM.yyyy")
  
    def listReviews = {

      ".review *" #> CrusibleClient.getReviews.map(review =>
          ".name" #> review.name &
          ".createDate" #> dateformat.format(review.createDate) &
          ".projectKey" #> review.projectKey
      )
    }
}