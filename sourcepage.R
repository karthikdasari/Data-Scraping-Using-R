# This function returns a CSV format file of urls's of all the 86 pages. 
# Input to this fuction is the basic url prompt i.e.
# http://www.metacritic.com/browse/games/release-date/available/ios/date?view=detailed&page=
  
sourcepage <- function(input){
        k <- matrix(nrow = 86, ncol =1)
        for (i in 1:86){
                j <- i-1
                k[i] <- paste(input,j, sep = "")
        }
     k <- as.data.frame(k)
     write.csv(k, "/Users/karthikdasari/desktop/sourcepage.csv") 
     # Specify the path and address of destination file here
}
