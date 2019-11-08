#Loading the rvest package
library(rvest)
library(readr)
library(stringr)


scrapeSearch <- function(searchTerm,collectedInfo) {
  subsetted<-data.frame(
    Url = character(0),
    Title = character(0),
    Description = character(0))
  for(i in 1:length(collectedInfo$Description))
  {
    chosen<-grepl("non", collectedInfo[i,3])
    if(chosen==TRUE){
      subsetted<-rbind(subsetted,collectedInfo[i,])  
    }
  }
  return(subsetted)
}


survpkgs <- read_csv("survpkgs.csv", col_names = FALSE)

for(i in 1:length(survpkgs$X1))
{
  survpkgs$X1[i]<-gsub('<li class=""><a href="..', '', survpkgs$X1[i])
  survpkgs$X1[i]<-gsub("(.html).*","\\1",survpkgs$X1[i])
  survpkgs$X1[i]<-paste("https://cran.r-project.org/web",survpkgs$X1[i],sep="")
}


collectedInfo <- data.frame(
  Url = character(259),
  Title = character(259),
  Description = character(259))
collectedInfo[] <- lapply(collectedInfo, as.character)
for(i in 1:length(survpkgs$X1))
{
#Specifying the url for desired website to be scraped
collectedInfo$Url[i]<-as.character(survpkgs$X1[i])
#Reading the HTML code from the website
webpage <- read_html(collectedInfo$Url[i])

header<-html_nodes(webpage,"h2")
collectedInfo$Title[i] <- html_text(header)
descr<-html_nodes(webpage,"p")
collectedInfo$Description[i]<- html_text(descr[1])


}

saveRDS(collectedInfo,file="survivalTaskViews.RDS")
nonprop2<-scrapeSearch("non")

nonprop<-data.frame(
  Url = character(0),
  Title = character(0),
  Description = character(0))
for(i in 1:length(collectedInfo$Description))
{
chosen<-grepl("non", collectedInfo[i,3])
if(chosen==TRUE){
nonprop<-rbind(nonprop,collectedInfo[i,])  
}
}

regu<-data.frame(
  Url = character(0),
  Title = character(0),
  Description = character(0))
for(i in 1:length(collectedInfo$Description))
{
  chosen<-grepl("regular", collectedInfo[i,3])
  if(chosen==TRUE){
    regu<-rbind(nonprop,collectedInfo[i,])  
  }
}

