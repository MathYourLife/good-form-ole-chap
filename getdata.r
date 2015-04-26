#' Pull a file down from the interwebs if it hasn't been already
#'
#' @param url Path to the source data
#' @param destfile Relative or absolute system path to where the data
#'                is to be saved
getdata <- function(url, destfile){
    print(paste('getting', destfile, sep=' '))
    method <- 'wget'
    if (!file.exists(destfile)) {
        # Specify the method of download
        download.file(url, destfile, method)
    } else {
        print(paste(destfile, 'is already fetched', sep=' '))
    }
}

url <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
destfile <- 'data/pml-training.csv'
getdata(url, destfile)

url <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
destfile <- 'data/pml-testing.csv'
getdata(url, destfile)
