package code.snippet

import scala.xml.NodeSeq
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JsCmds._
import net.liftweb.widgets.flot._
import scala.actors._
import scala.actors.Actor._
import net.liftweb.common.{Box, Full}
import net.liftweb.util._
import net.liftweb.http._
import SHtml._
import _root_.scala.xml.Text
import code.util.CrusibleClient
import org.joda.time.DateTime
import java.util.Calendar
import scala.util.Random

class Graphs {

  def randomColor(): String = {
      def randomChar: String = {
        val legalValues = List("A","B","C","D","E","F") ++ (0 to 9)
        legalValues(Random.nextInt(legalValues.size - 1)).toString()
      }
      
      (for (i <- 1 to 6) yield randomChar).reduce(_+_)
  }

  def equalYearMonth(d1: DateTime, d2: DateTime) = 
    d1.getYear() == d2.getYear && d1.getMonthOfYear() == d2.getMonthOfYear()  

      
  def getMonthlyStatsChartUrl(width: String, height: String, reviewDates: List[DateTime]) = {
    def d(y: Int, m: Int, d: Int) = new DateTime(y, m, d, 0, 0, 0, 0)
    
    def statsForMonth(y: Int, m: Int, reviewDates: List[DateTime]) = {
      val month = d(y, m, 1)
      (y, m, (reviewDates.filter(date => equalYearMonth(month, date))).size)
    }
    
    def getDataForYear(year: Int, yearlyStats: List[(Int, Int, Int)]): String = {
      
      def getReviewCount(month: Int): Int = 
        yearlyStats.filter(row => (row._1,row._2) == (year, month)) match {
        case Nil => 0
        case stats => stats.head._3
      }
        
      val stats = for (month <- 1 to 12) yield getReviewCount(month)
      (1 to 12).mkString(",") ++ "|" ++ stats.mkString(",")
    }
    
    def reviewsPerYear(year: Int, reviewDates: List[DateTime]): Int = 
      (reviewDates.filter(_.getYear() == year)).size
    
    
    def betweenOrEqual(start: DateTime, end: DateTime, date: DateTime) = {
      (start.isBefore(date) || start.isEqual(date)) &&
      (date.isBefore(end) || date.isEqual(end))
    }
    
    val sortedReviewDates = reviewDates.sort((d1, d2) => d1.isBefore(d2))
    
    val firstdate = sortedReviewDates.head 
    val firstyear = firstdate.getYear()
    val lastdate = sortedReviewDates.last
    val lastyear = lastdate.getYear()
    
    val stats = for (year <- firstyear to lastyear;
                     month <- 1 to 12
                     if betweenOrEqual(firstdate, lastdate, d(year, month, 1)))
      yield statsForMonth(year, month, sortedReviewDates)
    
    println(stats)
    
    val maxReviewCount = (stats maxBy { _._3 })._3
    val distinctYearCount = lastyear - firstyear + 1
    val xScale = "1,12"
    val yScale = "0,"+maxReviewCount
    
    val baseurl = "http://chart.apis.google.com/chart?" 
    val header = "chtt=Reviews"
    val charttype = "&cht=lxy"
    val xlabels = "&chxl=0:|Jan|Feb|Mar|Apr|May|June|July|Aug|Sep|Oct|Nov|Dec"
    val ranges = "&chxr=0,"+xScale+"|1,"+yScale
    val axes = "&chxt=x,y"
    val size = "&chs="+width+"x"+height
    val colors = "&chco="+(for(i <- 1 to distinctYearCount) yield randomColor).mkString(",")
    val scaleForText = "&chds="+(for(i <- 1 to distinctYearCount) yield xScale+","+yScale).mkString(",")
    val data = "&chd=t:"+(for(i <- firstyear to lastyear) yield getDataForYear(i, stats.toList)).mkString("|") 
    val legends = "&chdl="+(for(i <- firstyear to lastyear) yield i + ", tot " + reviewsPerYear(i, reviewDates) ).mkString("|") 
    val legendpos = "&chdlp=b" // bottom
    val margins = "&chma=5,5,5,25|0,2"  
    //chma=<left_margin>,<right_margin>,<top_margin>,<bottom_margin>|<opt_legend_width>,<opt_legend_height>
      
    baseurl + header + charttype + xlabels + ranges + axes + size + colors + 
            scaleForText + data + legends + legendpos + margins            
  }

//    def getReviewsPerMonth: List[(String, Int)]  = {
//      val reviewDates = CrusibleClient.getReviewData.map(data => data.createDate)
//      
//      val yearmonths = reviewDates.map(date => (date.getYear(), date.getMonth()))
//      val groupedYearMonths = yearmonths.groupBy {pair => pair}
//      val monthlyCounts = groupedYearMonths.map(x => (x._1, x._2.length))
//      
//      val formatted = monthlyCounts.toList.map(pair => (pair._1._1 + "/" + pair._1._2, pair._2))
//      formatted.sort( (pair1, pair2) => pair1._1 < pair2._1 )
//    }
  
  	def render = {
      val width = "400"
      val height = "225"

      val googleUrl = getMonthlyStatsChartUrl(width, height,
          CrusibleClient.getReviews.map(data => new DateTime(data.createDate)))
      
      <img width={width} height={height} src={googleUrl} />
    }
	
}