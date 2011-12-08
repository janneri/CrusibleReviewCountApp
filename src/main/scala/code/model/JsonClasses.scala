package code.model

object JsonClasses {
  
  case class ReviewData(reviewData: List[Review])
  case class Creator(displayName: String)
  case class PermaId(id: String)
  
  case class Review(name: String, description: String, createDate: java.util.Date, creator: Creator, projectKey: String, permaId: PermaId)

}