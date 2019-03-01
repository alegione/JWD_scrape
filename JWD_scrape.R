library(tidyverse)
library(rvest)
library(googlesheets)
library(stringr)

JWDdatabase <- gs_title(x = "JWD_articles")

html <- read_html(x = "https://www.jwildlifedis.org/toc/jwdi/55/1")



issue <- html %>%
  html_nodes(css = ".articleEntry")


#table <- c("Title", "AuthorList", "First Author", "Link"))
remove(dat)
dat <- rbind(c("TweetAuthor", "Title", "Link", "Tweet", "AuthorListConcat"))

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
  dat <- rbind(dat, c(TweetAuthor, Title, Link, Tweet, AuthorListConcat))
}

issueDetails <- html %>%
  html_nodes(xpath = '//*[@id="articleToolsHeading"]') %>%
  html_text() %>% gsub(pattern = "\n", replacement = "") %>%
  str_squish()

gs_ws_new(ss = JWDdatabase, ws_title = issueDetails, verbose = TRUE, input = dat)
