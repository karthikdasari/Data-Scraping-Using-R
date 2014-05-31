# This function generates the list of url's in a page. There are 86 pages for all the games.
#Each page has 100 games in it, except the last one. Input to this function
# is the url of the page e.g. http://www.metacritic.com/browse/games/release-date/available/ios/date?view=detailed&page=26

readpage <- function(page){
        library(XML)
        #print(page)
        page1 <- htmlParse(page)
        page2 <- xmlRoot(page1)
        h <- xpathSApply(page2, "//div[@class = 'basic_stat product_title']", xmlValue)
        #print(h)
        l <- length(h) - 1
        print(l)
        urllist <- matrix(nrow= l, ncol =1)
        for (i in 1:l)
        {      
                a <- h[i]
                b <- gsub("\n", "",a)
                c <- gsub("^\\s+|\\s+$", "", b)
                d <- gsub(" :", ":",c, fixed = TRUE) 
                e <- gsub("~ ", "",d ,fixed = TRUE)
                f <- gsub(" &", "",e, fixed = TRUE)
                g <- gsub("-", " ",f)
                j <- gsub("[^[:alnum:])_+! ]","",g)
                k <- gsub(" ", "-",j)
                l <- tolower(k)
                urllist[i,1] <- paste("http://www.metacritic.com/game/ios/",l, sep = "")
        }
       return(urllist)
}
