
library(arulesSequences)
library(arulesViz)
library(arules)
library(plyr)
library(dplyr)
library(ggplot2)

## Setting THe Directory for the WOrking Folder

movie_master = read.csv("/Users/abhilashakumari/Downloads/Web.csv")
movies = read.csv("/Users/abhilashakumari/Downloads/u_item.csv")
## Check For Duplicate Movies in the Merged List -- No Duplicate movie Name Found
dupmovies <- movies$movie.name[duplicated(movies$movie.name)] ##changed MovieName to  movie.name 


Traindata = subset(movie_master,UserID < 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID))
Testdata = subset(movie_master,UserID >= 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID))
#trans <- as(split(Traindata[,2], Traindata[,1]), "transactions")

#Writing the Training and Test Data to Excel
write.csv(Traindata,file="Traindata_Movies.csv",row.names = FALSE)
write.csv(Traindata,file="Testdata_Movies.csv",row.names = FALSE)

Traindata_Movies <- read.transactions(file="Traindata_Movies.csv",rm.duplicates = TRUE,format="single",sep=",", cols=c("UserID","MovieID"))
#Testdata_Movies <- read.transactions(file="Testdata_Movies.csv",rm.duplicates = TRUE,format="single",sep=",",cols=c("UserID","MovieID"))
inspect(Traindata_Movies)

# Rule build based on 20% Support and 50%Confidence
rule1 <- apriori(Traindata_Movies, parameter = list(supp=0.2, conf=0.5,minlen = 2))  #-- 834
rule1<-sort(rule1, decreasing=TRUE,by="confidence")
summary(rule1)
inspect(rule1)