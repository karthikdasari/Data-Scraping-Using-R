# This function returns the list of all the url's in 86 pages and saves the data in a CSV file.
# The function calls for the file sourcepage.csv.Check the working directory for running this function
readurl <- function(){
        data <- read.csv("sourcepage.csv", colClasses = "character")
        matrix <- readpage(data[1,2])
        for (i in 2:86){
                input <- data[i,2]
                matrixN <- readpage(input)
                matrix <- rbind(matrix, matrixN)
                print(i)       
        }
        write.csv(matrix, "/Users/karthikdasari/desktop/JMD/urllist.csv")
        # Specify the path and destination file address here
        
        
}
