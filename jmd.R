# This function returns a data frame with the required data points like meta score, no of critics, 
# user score etc. Input to the fuction is the link to the url of a particular game
jmd <- function (url){
        library("XML")
        doc <- htmlTreeParse(url, useInternal = TRUE)
        rootNode <- xmlRoot(doc)
        n <- xpathSApply(rootNode, "//li[@class = 'summary_detail publisher']", xmlValue)
        n1 <- gsub("\n", "", n)
        n2 <- gsub("\t", "", n1)
        n3 <- gsub("\\s","",n2)
        developer <- gsub("Publisher:", "",n3, fixed = TRUE)
        if(length(as.numeric(developer)) == 0) {developer <- NA}
        d <- xpathSApply(rootNode, "//li[@class = 'summary_detail release_data']", xmlValue)
        d1 <- gsub("[//s \t \n : ]", "",d)
        d2 <- gsub("ReleaeDate", "",d1)
        date <- as.Date(d2,format='%B %d, %Y')
        t <- xpathSApply(rootNode, "//div[@class = 'product_title']", xmlValue)
        t1 <- gsub("\n", "", t)
        t2 <- gsub("\t", "",t1)
        t3 <- gsub("^\\s+|\\s+$", "", t2)
        name <- gsub("iOS", "", t3)
        g <- xpathSApply(rootNode, "//li[@class = 'summary_detail product_genre']", xmlValue)
        g1 <- gsub("\n", "",g)
        g2 <- gsub("\\s", "",g1)
        genre <- gsub("Genre(s):", "",g2, fixed = TRUE)
        if(length(as.numeric(xpathSApply(rootNode, "//span[@itemprop = 'ratingValue']", xmlValue))) == 0){metascore <- 0} else{metascore <- as.numeric(xpathSApply(rootNode, "//span[@itemprop = 'ratingValue']", xmlValue))}
        p <- as.numeric(xpathSApply(rootNode, "//div[@class = 'metascore_w user large game positive']", xmlValue))
        n <- as.numeric(xpathSApply(rootNode, "//div[@class = 'metascore_w user large game negative']", xmlValue))
        m <- as.numeric(xpathSApply(rootNode, "//div[@class = 'metascore_w user large game mixed']", xmlValue))
        if(length(p) != 0){u <- p}else{
                if(length(n) != 0){ u <- n}else{
                        if(length(m) != 0) {u <- m}else{
                                u <- 0
                        }
                }
        }
        userscore <- u[1]
        s <- as.numeric(xpathSApply(rootNode, "//span[@class = 'count']", xmlValue))
        #print(s)
        positivecritics <- s[4]
        mixedcritics <- s[5]
        negativecritics <- s[6]
        critics <- s[4] + s[5] + s [6]
        if (length(s) == 9)
        {positiveusers <- s[7] 
        mixedusers <- s[8] 
        negativeusers <- s[9] }else {
                positiveusers <- 0
                negativeusers <- 0
                mixedusers <- 0
        }
        users <- positiveusers + negativeusers + mixedusers
        ao <- xpathSApply(rootNode, "//li[@class = 'summary_detail product_platforms']", xmlValue)
        if(class(ao) == "character"){
                ao1 <- gsub("\n", "", ao)
                ao2 <- gsub("Also On:", "", ao1, fixed = TRUE)
                alsoon <- gsub("\\s", "", ao2)
        }else {alsoon <- NA}
        DataCollectedOn <- Sys.Date()
k <- data.frame(name,developer,genre,date,metascore,positivecritics,mixedcritics,negativecritics,critics,userscore,users,positiveusers,negativeusers,mixedusers,alsoon,DataCollectedOn)
return(k)
}
        
