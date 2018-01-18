##########################################
# Building Demo Rules for Movie Lens Database 
# Project : Movie Lens Database 
# Team : Team Dosa
# Date : 29-10-2017
##########################################
#install.packages("plyr", dependencies =TRUE)
#install.packages("dplyr", dependencies =TRUE)
#install.packages("arulesSequences")
#install.packages("arules")
#install.packages("arulesViz")

# library(arulesSequences)
# library(arulesViz)
# library(arules)
# library(plyr)
# library(dplyr)
# library(ggplot2)

## Setting THe Directory for the WOrking Folder
setwd("/Users/abhilashakumari/Downloads")
#movie_master = read.csv("/Users/abhilashakumari/Downloads/Web.csv")
#movies = read.csv("/Users/abhilashakumari/Downloads/u_item.csv")
## Check For Duplicate Movies in the Merged List -- No Duplicate movie Name Found
#dupmovies <- movies$MovieName[duplicated(movies$MovieName)]


Traindata = subset(movie_master,UserID < 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID))
Testdata = subset(movie_master,UserID >= 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID))

typeof(Traindata)
#Writing the Training and Test Data to Excel
#write.csv(Traindata,file="Traindata_Movies.csv",row.names = FALSE)
#write.csv(Traindata,file="Testdata_Movies.csv",row.names = FALSE)

Traindata_Movies <- read.transactions(file="/Users/abhilashakumari/Downloads/Traindata_DemoGraphic.csv",rm.duplicates = TRUE,format="single",sep=",", cols=c("UserID","MovieID"))
#Testdata_Movies <- read.transactions(file="Testdata_Movies.csv",rm.duplicates = TRUE,format="single",sep=",",cols=c("UserID","MovieID"))

Traindata_Demo_Genre = subset(movie_master,UserID < 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID,Occupation.Details, Gender, Age, Genres1))
Testdata_Demo_Genre = subset(movie_master,UserID >= 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID, Occupation.Details, Gender, Age, Genres1))

#write.csv(Traindata_Demo_Genre,file="Traindata_Demo_Genre.csv",row.names = FALSE)
#write.csv(Testdata_Demo_Genre,file="Traindata_Demo_Genre.csv",row.names = FALSE)

Demo_Genre_Train <- data.frame(TId = rep(Traindata_Demo_Genre$UserID, times = 5), items = c(paste0("Movieid=", Traindata_Demo_Genre$MovieID), paste0("Occupation=", Traindata_Demo_Genre$Occupation.Details),paste0("Gender=",Traindata_Demo_Genre$Gender), paste0("Age =",Traindata_Demo_Genre$Age),paste0("Genre =",Traindata_Demo_Genre$Genres1)))
Demo_Genre_Test <- data.frame(TId = rep(Testdata_Demo_Genre$UserID, times = 5), items = c(paste0("Movieid=", Testdata_Demo_Genre$MovieID), paste0("Occupation=", Testdata_Demo_Genre$Occupation.Details),paste0("Gendre=",Testdata_Demo_Genre$Gender), paste0("Age =",Testdata_Demo_Genre$Age), paste0("Genre =",Testdata_Demo_Genre$Genres1)))

Demo_Genre_Train <- unique(Demo_Genre_Train)
Demo_Genre_Test <- unique(Demo_Genre_Test)
trans <- as(split(Demo_Genre_Train$items, Demo_Genre_Train[, "TId"] ), "transactions")
#inspect(trans)
#write.csv(Demographic_Train,file="Hybrid_Demo.csv",row.names = FALSE)



# Rule build based on 20% Support and 70%Confidence
rulesDemoGenre <- apriori(trans, parameter = list(supp=0.50, conf=0.5,minlen = 2, maxlen =4)) 
rulesDemo<-sort(rulesDemoGenre, decreasing=TRUE,by="lift")
rulesDemoGenreDF = as(rulesDemo,"data.frame")
#summary(rulesDemoGenre)
#inspect(rulesDemoGenre)
#plot(rulesDemoGenre)


filterrules <- function(rulesDemoGenreDF) {
  pattern=paste("=> {Movieid=",sep="")
  firingrules=rulesDemoGenreDF[grep(pattern,rulesDemoGenreDF$rules,fixed=TRUE),]
  #gsub(" ","",toString(sub("\\}","",sub(".*=> \\{","",firingrules))))
}



# Testing the Data

Testdata_Test <- Demo_Genre_Test[c(1,2)]
colnames(Testdata_Test) <- c("basketID","items")



#Defining Functions  -- Removing Duplicate items from the list
uniqueitems <- function(itemstrg) {
  Y<-unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
  Y<-Y[Y!= ""]
  return(Y)
}


# Execute ruleset using item as rule antecedent

makepreds <- function(items,rulesDemoGenreDF) {
  antecedent=paste("{",items,"} =>",sep="")
  firingrules=rulesDemoGenreDF[grep(antecedent,rulesDemoGenreDF$rules,fixed=TRUE),1]
  gsub(" ","",toString(sub("\\}","",sub(".*=> \\{","",firingrules))))
}


# count how many predictions are in the basket of items already seen by that user 

checkpreds <- function(preds,baskID) {
  
  plist=preds[[1]]
  blist=baskets[baskets$basketID == baskID,"items"][[1]]
  cnt=0
  for(p in plist) {
    if(p %in% blist) cnt=cnt+1
  }
  cnt
}


# count all predictions made
countpreds <- function(predlist) {
  len=length(predlist)
  if(len>0 && (predlist[[1]]=="")) 0 
  else len
}


#execute rules against test data
Testdata_Test$preds = apply(Testdata_Test,1,function(X) makepreds(X["items"], rulesDemoGenreDF))
head(Testdata_Test$preds,100)

# extract unique predictions for each test user
userpreds = as.data.frame(aggregate(preds ~ basketID, data = Testdata_Test, paste, collapse=","))
userpreds$preds = apply(userpreds,1,function(X) uniqueitems(X["preds"]))
head(userpreds,50)

# extract unique items bought (or rated highly) for each test user
baskets = as.data.frame(aggregate(items ~ basketID, data = Testdata_Test, paste, collapse=","))
baskets$items = apply(baskets,1,function(X) uniqueitems(X["items"]))
head(Testdata,10)
#count how many unique predictions made are correct, i.e. have previously been bought (or rated highly) by the user
correctpreds = sum(apply(userpreds,1,function(X) checkpreds(X["preds"],X["basketID"])))

# count total number of unique predictions made

totalpreds <- sum(apply(userpreds,1,function(X) countpreds(X["preds"][[1]])))

precision = correctpreds*100/totalpreds
Recall = correctpreds*100/nrow(Testdata_Test)

Total_Persons_predicted <-sum(apply(userpreds,1,function(X) ifelse(countpreds(X["preds"][[1]])>0,1,0)))

cat("precision=", precision, "recall=",Recall,"corr=",correctpreds,"total=",totalpreds, "Total_Persons_Predicted=",Total_Persons_predicted)

#########################################################################################################################################