package code.util

import org.apache.http.client.ResponseHandler
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.BasicResponseHandler
import org.apache.http.impl.client.DefaultHttpClient
import net.liftweb.json._
import java.sql.DriverManager
import java.sql.Connection
import net.liftweb.mapper._
import net.liftweb.common._
import code.model.JsonClasses._
import scala.xml.XML
import net.liftweb.http.SessionVar

object CrusibleClient {

    object reviews extends SessionVar[Box[List[Review]]](Empty)
    
    private var authtoken: Box[String] = None
    
  	implicit val formats = new DefaultFormats {
  		override def dateFormatter = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.mmmZ")
  	}
   
    def main(args: Array[String]) {
        val reviews = getAllReviewData
        println("Fetched %d reviews".format(reviews.length))
    }

    private def getAuthToken: String = {
      authtoken openOr loginAndGetToken(sysprop("username"), sysprop("password"))
    }
    
    def getReviews: List[Review] = reviews openOr getReviewsForYear(2012)

    private def getReviewsForYear(year: Int): List[Review] = {
      getReviewData(yearFilter(year) _)
    }
    
    private def yearFilter(year: Int)(review: Review) = {
      review.createDate.getYear() == year && 
      notExerciseAndContainsCommentsFilter(review)
    }

    private def getAllReviewData: List[Review] = {
      getReviewData(notExerciseAndContainsCommentsFilter)
    }
    
    private def notExerciseAndContainsCommentsFilter(review: Review) = {
      review.projectKey != "EXERCISES" && 
      containsComments(review.permaId.id, getAuthToken)
    }

    private def getReviewData(filter: Review => Boolean): List[Review] = {
        
        val json = JsonParser parse getMetricsAsJson
        val review = json.extract[ReviewData]
        
        val reviewList = review.reviewData
            .filter(filter(_))
            .sortBy(_.createDate)
            
        reviews.set(Full(reviewList))
        
        reviews openOr List()
    }
    
    def getReviewsPerAuthor: List[(String, Int)] = {
        val reviewsPerAuthor = 
          for ( (creator, reviews) <- getReviews.groupBy(_.creator) )
       	    yield (creator.displayName, reviews.length)
      
      reviewsPerAuthor.toList.sortBy(_._2).reverse
    }   
        
    private def getMetricsAsJson: String = {
        // states: sDraft,Approval, Review, Summarize, Closed, Dead, Rejected, Unknown
        call("http://review.solita.fi/rest-service/reviews-v1.json?FEAUTH=" + 
            getAuthToken + "&state=Draft,Approval,Review,Summarize,Closed,Unknown");
    }
    
    private def sysprop(key: String): String = {
      sys.props.get(key).get
    }
    
    private def loginAndGetToken(username: String, pass: String): String = {
        val responseBody = call("http://review.solita.fi/rest-service/auth-v1/login?userName="
            +username+"&password="+pass);
    
        val xml = XML.loadString(responseBody)
        val token = xml \\ "token"
    	
        token.text
    }
    
    private def containsComments(reviewKey: String, authtoken: String) = {
      val responseBody = call("http://review.solita.fi/rest-service/reviews-v1/"+
          reviewKey+"/comments.json?FEAUTH=" + authtoken);
      
      responseBody.contains("versionedLineCommentData")
    }
    
    private def call(url: String): String = {
        val httpclient = new DefaultHttpClient
        try {
            val request = new HttpGet(url)

            val responseHandler = new BasicResponseHandler
            val responseBody = httpclient.execute(request, responseHandler)

            responseBody
      
        } finally {
        httpclient.getConnectionManager.shutdown
        }
    }

}