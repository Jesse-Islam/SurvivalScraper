scrapeSearch <- function(searches,LCCI,collectedInfo) {
  subsetted<-data.frame(
    Url = character(0),
    Title = character(0),
    Description = character(0))
  for(i in 1:length(LCCI$Description))
  {
    chosen<-any(sapply(searches, function(x) grepl(x, survivalTaskViews[i,])))
    if(chosen==TRUE){

      pull<-collectedInfo[i,]
      pull$Description<-gsub("[\r\n\t]", " ", collectedInfo[i,3])
      pull$Description<-gsub("\\s+", " ", collectedInfo[i,3])
      subsetted<-rbind(subsetted,pull)  
    }
    if(chosen==FALSE){
      chosen2<-any(sapply(searches, function(x) grepl(x, survivalTaskViews[i,])))
      if(chosen2==TRUE){
        pull<-collectedInfo[i,]
        pull$Description<-gsub("[\r\n\t]", " ", collectedInfo[i,3])
        pull$Description<-gsub("\\s+", " ", collectedInfo[i,3])
        subsetted<-rbind(subsetted,pull)  
      }
    }
  }
  return(subsetted)
}


survivalTaskViews<-readRDS("survivalTaskViews.RDS")
lcstv<-data.frame(lapply(survivalTaskViews, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))
  

searches<-c("non-proportional","nonproportional","regular","lasso","ridge","competing","leftcensored","left-censored","multistate","multi-state","linear regression","penalized")
picked<-scrapeSearch(searches,lcstv,survivalTaskViews)
write.csv(picked,"survivalTaskViewsOfInterest.csv", row.names = FALSE)
