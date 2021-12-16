# This script imports some example data for the multi-class classification.
# (it can be modified by the user)

library(readxl)
## Loading Data
url_src <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00192/BreastTissue.xls"
p1f <- tempfile()
download.file(url_src, p1f, mode="wb")
myData1 <- read_excel(path=p1f, sheet = 2,col_types=(c("numeric","text",rep("numeric",9))))


## Preprocessing
# We remove the first column that just holds the row number
myData1 <- myData1[,-c(1)]
myData1[,1] <- as.factor(as.character(unlist(myData1$Class)))
myData1 <- data.frame(myData1)

# we rename the columns whose names hold special characters
colnames(myData1)[7] <- "A.over.DA"
colnames(myData1)[8] <- "Max.IP"

# we place the classes in the last column
myData1 <- myData1[, c(2:ncol(myData1),1)]


# We preprocess the data by normalizing and center around the mean value for
# each attribute
myData <- myData1