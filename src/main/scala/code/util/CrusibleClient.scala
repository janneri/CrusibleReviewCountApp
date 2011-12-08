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

    def getSelectedTrainingSession = reviews.is
    
	implicit val formats = new DefaultFormats {
		override def dateFormatter = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.mmmZ")
	}
   
    def main(args: Array[String]) {
        val reviews = getReviewData
        println("Fetched %d reviews".format(reviews.length))
    }
  
    def getReviews: List[Review] = {
      reviews openOr getReviewData
    }
    
    def getReviewData: List[Review] = {
        
        val authtoken = loginAndGetToken(sysprop("username"), sysprop("password"))

        val json = JsonParser.parse(getMetricsAsJson(authtoken))
        val review = json.extract[ReviewData]
        
        val reviewList = review.reviewData.filter(_.projectKey != "EXERCISES").sortBy(_.createDate)
        reviews.set(Full(reviewList))
        
        reviews openOr List()
    }
    
    def getReviewsPerAuthor: List[(String, Int)] = {
        val reviewsPerAuthor = 
          for ( (creator, reviews) <- getReviews.groupBy(_.creator) )
       	    yield (creator.displayName, reviews.length)
      
      reviewsPerAuthor.toList.sortBy(_._2).reverse
    }   
    
    def call(url: String): String = {
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
    
    def getMetricsAsJson(authtoken: String): String = {
        // states: sDraft,Approval, Review, Summarize, Closed, Dead, Rejected, Unknown
        call("http://review.solita.fi/rest-service/reviews-v1.json?FEAUTH=" + authtoken + "&state=Draft,Approval,Review,Summarize,Closed,Unknown");
    }
    
    def sysprop(key: String): String = {
      sys.props.get(key).get
    }
    
    def loginAndGetToken(username: String, pass: String): String = {
        val responseBody = call("http://review.solita.fi/rest-service/auth-v1/login?userName="+username+"&password="+pass);
    
        val xml = XML.loadString(responseBody)
        val token = xml \\ "token"
    	
        token.text
    }
    
    def getReviewDetails(authtoken: String, reviewKey: String) = {
      // todo
      val responseBody = call("http://review.solita.fi/rest-service/"+reviewKey+"/reviewitems.json?FEAUTH=" + authtoken);
    }
    
}