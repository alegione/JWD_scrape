library(tidyverse)
library(rvest)
library(googlesheets)
library(stringr)

JWDdatabase <- gs_title(x = "JWD_articles")

VolumeNo <- 1
VolumeCurrent <- 54

remove(dat)
dat <- rbind(c("Volume", "Issue", "TweetAuthor", "Title", "Link", "Tweet", "AuthorListConcat"))

for (VolumeNo in 1:VolumeCurrent) {
  if (VolumeNo == 1) {
    issueNo <- 3
  } else {
    issueNo <- 1
  }
  while (issueNo < 5) {
    paste("Accessing Volume", VolumeNo, "issue", issueNo)
    Address <- paste0("https://www.jwildlifedis.org/toc/jwdi/", VolumeNo,"/", issueNo)
    html <- read_html(x = Address)
    issue <- html %>%
    html_nodes(css = ".articleEntry")

    for (i in seq(issue)){
      Title <- issue[i] %>% html_nodes(css = "div.art_title") %>% html_text()
      AuthorList <- issue[i] %>% html_nodes(css = ".hlFld-ContribAuthor") %>% html_text() 
      AuthorListConcat <- AuthorList %>% paste0(collapse = ", ")
      Link <- paste0("https://www.jwildlifedis.org", issue[i] %>% html_nodes(css = ".ref") %>% html_attr(name = "href") %>% head(1))
      if (length(AuthorList)>2) {
        TweetAuthor <- AuthorList %>% head(1) %>% paste("et al.", sep = ", ")
      } else if (length(AuthorList)>1) {
        TweetAuthor <- AuthorList %>% paste0(collapse = " & ")
      } else {
        TweetAuthor <- AuthorList
      }
      
      
      Tweet <- paste0("JWD research: ", TweetAuthor, ": ", Title, " (", Link, ")")
      dat <- rbind(dat, c(VolumeNo, issueNo, TweetAuthor, Title, Link, Tweet, AuthorListConcat))
    }
    issueNo <- issueNo + 1
  }
}

issueDetails <- html %>%
  html_nodes(xpath = '//*[@id="articleToolsHeading"]') %>%
  html_text() %>% gsub(pattern = "\n", replacement = "") %>%
  str_squish()

gs_ws_new(ss = JWDdatabase, ws_title = issueDetails, verbose = TRUE, input = dat)
