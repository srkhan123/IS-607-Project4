
#Using the following link : http://www.r-bloggers.com/search/web%20scraping; 
#For each of the reference blog entries on the first page, you should pull out the 
# 1) title, 
# 2) date, and 
# 3) author, 
# and store these in an R data frame. 

#install as needed - will require the following packages:
library(RCurl)
library(XML)
library(stringr)
library(rjson)
library(rvest)
library(reshape)
library(plyr)

#------------------------------------------------------------------------------------------------------
#create Function blogScraper which takes url as input and scrapes Blog Title, Blog Date and Blog Author
  
    blogScraper <- function(url){
    getPage = htmlParse(url)
    
    #reads contecnt of page
    root<- xmlRoot(getPage)
    
    # get the html body node
    body = xmlChildren(root)$body
    blogTitle   = xpathSApply(body, "//h1", xmlValue)
      if (is.null(blogTitle))    blogTitle <- NA
    blogdate    = xpathSApply(body, "//div[@class='date']", xmlValue)
      if (is.null(blogdate))    blogdate <- NA
    blogauthor  = xpathSApply(body, "//a[@rel='author']",xmlValue)
      if (is.null(blogauthor))    blogauthor <- NA
    return(c(url, blogTitle, blogdate, blogauthor))
    
    Sys.sleep(1)
  }
#----------------------------------------------------------------------
# create Function getPageURLs which takes the base url and finds the last page and conctenates the base url with page extension urls to complete it

  getPageURLs <-function(url){
    #url <-"http://www.r-bloggers.com/search/web%20scraping"
    getPage = htmlParse(url)
    
    #reads contecnt of page
    root<- xmlRoot(getPage)
    
    # get the html body node
    body = xmlChildren(root)$body
    
    #LastPage number
    max_url <- as.numeric(xpathSApply(body, "//a[@title='Last Page']" , xmlValue))
    
    #start with page 1 increment by 1 until last page is reached
    add_url <- str_c('/page/', seq(1, max_url, 1))
    add_url
    
    urls_list <- as.list(str_c(url, add_url))
    
    return (urls_list)
  }
#-----------------------------------------------------------------------

  
#Blog base url
url <- "http://www.r-bloggers.com/search/web%20scraping"

#First get all the links to pages
url_list <-  getPageURLs(url)
url_list
class(url_list) #list

#captures all links on the web scraping blog pages(1 to 17)
urls <- unlist(llply(url_list, getHTMLLinks))
urls

#identifies all the links on the url with -scrap in the link and removes duplicate links
urls <- unique(unlist(urls[str_detect(urls, "-scrap")]))
urls

#put the filenames in a list
urls <- as.list(urls)
urls

#apply same function blogScraper to all the web scraping URLs, to get title date and author
a <- sapply(urls, blogScraper)
a

#transpose all entries to have a row for each record
a <- t(a)

class(a)
a

#force Matrix into a dataframe
a <- as.data.frame(a)
class(a)
a

#name the dataframe columns appropriately
a <- rename(a, c("V1"="Blog URL", "V2"="Blog Title", "V3" = "Blog Date", "V4" = "Blog Author"))
class(a)
a

# view data
class(a)
View(a)


------------------------------------------------------------------------------



