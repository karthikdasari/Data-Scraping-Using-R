# This function takes the urllist file and calls for function "jmd" to get the required data from
# all the games and saves the data to a csv file.
readdata <- function(){
        data <- read.csv("urllistn.csv", colClasses = "character")
        df <- jmd(data[1,2])
        for (i in 2:8522){              # 8522 - total no of games from the urllist
                input <- data[i,2]      # it may change as new games get added
                dfN <- jmd(input)
                df <- rbind(df, dfN)
                print(i)
                
        }
        write.csv(df, "/Users/karthikdasari/desktop/JMD/Data/datan42.csv")
        # Specify path and destination file address here.
        
# It is advisable to run the function repeatedly for every 200 or 500 url's. The function stops
# if url form the list directs to an invalid page. The url has to be checked and the function
# has to be run again
}        
