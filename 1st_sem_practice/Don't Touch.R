##########################################
# Building Demo Rules for Movie Lens Database 
# Project : Movie Lens Database 
# Team : Team Dosa
# Date : 29-10-2017
##########################################
# install.packages("plyr", dependencies =TRUE)
# install.packages("dplyr", dependencies =TRUE)
# install.packages("arulesSequences")
# install.packages("arules")
# install.packages("arulesViz")
# 
# library(arulesSequences)
# library(arulesViz)
# library(arules)
# library(plyr)
# library(dplyr)
# library(ggplot2)

## Setting THe Directory for the WOrking Folder
movie_master = read.csv("/Users/abhilashakumari/Downloads/Web.csv")
movies = read.csv("/Users/abhilashakumari/Downloads/u_item.csv")
## Check For Duplicate Movies in the Merged List -- No Duplicate movie Name Found
dupmovies <- movies$movie.name[duplicated(movies$movie.name)] ##changed movie.name to MovieName
#Check For Duplicate Movies in the Merged List -- No Duplicate movie Name Found
dupmovies <- movies$MovieName[duplicated(movies$MovieName)]


Traindata = subset(movie_master,UserID < 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID))
Testdata = subset(movie_master,UserID >= 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID))
#trans <- as(split(Traindata[,2], Traindata[,1]), "transactions")

#Writing the Training and Test Data to Excel
write.csv(Traindata,file="Traindata_Movies.csv",row.names = FALSE)
write.csv(Traindata,file="Testdata_Movies.csv",row.names = FALSE)

Traindata_Movies <- read.transactions(file="Traindata_Movies.csv",rm.duplicates = TRUE,format="single",sep=",", cols=c("UserID","MovieID"))
#Testdata_Movies <- read.transactions(file="Testdata_Movies.csv",rm.duplicates = TRUE,format="single",sep=",",cols=c("UserID","MovieID"))


# Rule build based on 20% Support and 50%Confidence
rule1 <- apriori(Traindata_Movies, parameter = list(supp=0.2, conf=0.5,minlen = 2)) -- 834
rule1<-sort(rule1, decreasing=TRUE,by="confidence")
summary(rule1)
inspect(rule1)

# Rule build based on 20% Support and 40%Confidence
rule2 <- apriori(Traindata_Movies, parameter = list(supp=0.2, conf=0.4,minlen = 2)) -- 916
rule2<-sort(rule2, decreasing=TRUE,by="confidence")
summary(rule2)
inspect(rule2)

# Rule build based on 20% Support and 60%Confidence
rule3 <- apriori(Traindata_Movies, parameter = list(supp=0.2, conf=0.6,minlen = 2)) -- 607
rule3<-sort(rule3, decreasing=TRUE,by="confidence")
summary(rule3)
inspect(rule3)


# Rule build based on 20% Support and 70%Confidence
rule4 <- apriori(Traindata_Movies, parameter = list(supp=0.2, conf=0.85,minlen = 2)) 
rule4<-sort(rule4, decreasing=TRUE,by="confidence")
summary(rule4)
inspect(rule4)
plot(rule4)

# Rule build based on 20% Support and 70%Confidence
rule5 <- apriori(Traindata_Movies, parameter = list(supp=0.3, conf=0.7,minlen = 2)) -- 9 
rule5<-sort(rule5, decreasing=TRUE,by="confidence")
summary(rule4)
inspect(rule4)

#plot(rules)
#plot(rules, measure=c("support", "lift"), shading="confidence")
#plot(rules, shading="order", control=list(main = "Two-key plot"))
#plot(rules, measure=c("support", "lift"), shading="confidence", interactive=TRUE)

# Writing All the Reports 
rule4DF = as(rule4,"data.frame")
write.csv(rule4DF,file="rule4DF.csv",row.names = FALSE)


# Testing the Data

Testdata_Test <- Testdata[c(1,3)]
colnames(Testdata_Test) <- c("basketID","items")



#Defining Functions  -- Removing Duplicate items from the list
uniqueitems <- function(itemstrg) {
  Y<-unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
  Y<-Y[Y!= ""]
  return(Y)
}


# Execute ruleset using item as rule antecedent

makepreds <- function(items,rule4DF) {
  antecedent=paste("{",items,"} =>",sep="")
  firingrules=rule4DF[grep(antecedent,rule4DF$rules,fixed=TRUE),1]
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
Testdata_Test$preds = apply(Testdata_Test,1,function(X) makepreds(X["items"], rule4DF))
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
Recall = Correctpreds*100/

Total_Persons_predicted <-sum(apply(userpreds,1,function(X) ifelse(countpreds(X["preds"][[1]])>0,1,0)))

cat("precision=", precision, "corr=",correctpreds,"total=",totalpreds, "Total_Persons_Predicted=",Total_Persons_predicted)
##########################################################################################################################################

######################################################

##2. Considering Other Genres and Movie for Prediction##
## Considering only Genres
######################################################


Traindata_Genres_Movies = subset(movie_master,UserID < 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID,Genres1))
Testdata_Genres_Movies = subset(movie_master,UserID >= 4851 & Ratings >=1 ,select= c(UserID,Title,MovieID, Genres1))

write.csv(Traindata_Genres_Movies,file="Traindata_Genres_Movies.csv",row.names = FALSE)
write.csv(Testdata_Genre_Movies,file="Testdata_Genre_Movies.csv",row.names = FALSE)

Movie_genre_Train <-  data.frame(TId = rep(Traindata_Genres_Movies$UserID, times = 2), items = c(paste0("Movieid=", Traindata_Genres_Movies$MovieID), paste0("Genre=", Traindata_Genres_Movies$Genres1)))
Movie_genre_Test <- data.frame(TId = rep(Testdata_Genres_Movies$UserID, times = 2), items = c(paste0("Movieid=", Testdata_Genres_Movies$MovieID), paste0("Genre=", Testdata_Genres_Movies$Genres1)))

Movie_genre_Train <- unique(Movie_genre_Train)
Movie_genre_Test <- unique(Movie_genre_Test)

trans_genre <- as(split(Movie_genre_Train$items, df3_recoded[, "TId"] ), "transactions")
rule_modified <- apriori(trans_genre, parameter = list(supp=0.2, conf=0.5,minlen = 2)) 
rulesGenreDF = as(rule_modified,"data.frame")
rule_modified<-sort(rule_modified, decreasing=TRUE,by="confidence")
summary(rule_modified)
inspect(rule_modified)
plot(rule_modified)


# Creating Function to Filter Out RUles 

filterrules <- function(rulesGenreDF) {
  pattern=paste("=> {Movieid=",sep="")
  firingrules=rulesGenreDF[grep(pattern,rulesGenreDF$rules,fixed=TRUE),]
  #gsub(" ","",toString(sub("\\}","",sub(".*=> \\{","",firingrules))))
}

Final_Rules_Genre = data.frame(filterrules(rulesGenreDF))

# Writing All the Reports 
FinalRulesDF = as(Final_Rules_Genre,"data.frame")
write.csv(FinalRulesDF,file="FinalRulesDF.csv",row.names = FALSE)


# Testing the Data

Testdata_Test <- Movie_genre_Test[c(1,2)]
colnames(Testdata_Test) <- c("basketID","items")



#Defining Functions  -- Removing Duplicate items from the list
uniqueitems <- function(itemstrg) {
  Y<-unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
  Y<-Y[Y!= ""]
  return(Y)
}


# Execute ruleset using item as rule antecedent

makepreds <- function(items,FinalRulesDF) {
  antecedent=paste("{",items,"} =>",sep="")
  firingrules=FinalRulesDF[grep(antecedent,FinalRulesDF$rules,fixed=TRUE),1]
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
Testdata_Test$preds = apply(Testdata_Test,1,function(X) makepreds(X["items"], FinalRulesDF))
head(Testdata_Test$preds,100)

# extract unique predictions for each test user
userpreds = as.data.frame(aggregate(preds ~ basketID, data = Testdata_Test, paste, collapse=","))
userpreds$preds = apply(userpreds,1,function(X) uniqueitems(X["preds"]))
head(userpreds,50)

# extract unique items bought (or rated highly) for each test user
baskets = as.data.frame(aggregate(items ~ basketID, data = Testdata_Test, paste, collapse=","))
baskets$items = apply(baskets,1,function(X) uniqueitems(X["items"]))

#count how many unique predictions made are correct, i.e. have previously been bought (or rated highly) by the user
correctpreds = sum(apply(userpreds,1,function(X) checkpreds(X["preds"],X["basketID"])))

# count total number of unique predictions made

totalpreds <- sum(apply(userpreds,1,function(X) countpreds(X["preds"][[1]])))

precision = correctpreds*100/totalpreds

Total_Persons_predicted <-sum(apply(userpreds,1,function(X) ifelse(countpreds(X["preds"][[1]])>0,1,0)))

cat("precision=", precision, "corr=",correctpreds,"total=",totalpreds, "Total_Persons_Predicted=",Total_Persons_predicted)

###########################################################################################################################################

####################################################

## 3.Considering Demographic Details and Movies##

####################################################

Traindata_Genre = subset(movie_master,UserID < 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID,Occupation.Details, Gender, Age))
Testdata_Genre = subset(movie_master,UserID >= 4851 & Ratings >=1 ,select= c(UserID,Title,MovieID, Occupation.Details, Gender, Age))

write.csv(Traindata_Genre,file="Traindata_Genre.csv",row.names = FALSE)
write.csv(Testdata_Genre,file="Testdata_Genre.csv",row.names = FALSE)



df3_recoded <- data.frame(TId = rep(Traindata_Genre$UserID, times = 4), items = c(paste0("Movieid=", Traindata_Genre$MovieID), paste0("Occupation=", Traindata_Genre$Occupation.Details),paste0("Gendre=",Traindata_Genre$Gender), paste0("Age =",Traindata_Genre$Age)))
df3_recoded_test <- data.frame(TId = rep(Testdata_Genre$UserID, times = 4), items = c(paste0("Movieid=", Testdata_Genre$MovieID), paste0("Occupation=", Testdata_Genre$Occupation.Details),paste0("Gendre=",Testdata_Genre$Gender), paste0("Age =",Testdata_Genre$Age)))

df3_recoded <- unique(df3_recoded)
df3_recoded_test <- unique(df3_recoded_test)
trans <- as(split(df3_recoded$items, df3_recoded[, "TId"] ), "transactions")
inspect(trans)
write.csv(df3_recoded,file="Hybrid.csv",row.names = FALSE)


Traindata_Genre <- read.transactions(file="Hybrid.csv",rm.duplicates = TRUE,format="single",sep=",", cols=c(1,2))
inspect( subset( rule_modified, subset = rhs %pin% "MovieID=" ) )
library(caret)
recall <- sensitivity(precision, y, positive="1")


inspect(sort(subset(rule_modified,
                    subset=rhs %in% 'items'),
             by = 'lift',
             decreasing = T))

Traindata_Genre$UserID = as.factor(Traindata_Genre$UserID)
Traindata_Genre$MovieID = as.factor(Traindata_Genre)
# Rule build based on 20% Support and 70%Confidence
rule_modified <- apriori(trans, parameter = list(supp=0.2, conf=0.5,minlen = 2)) 
rule4DF = as(rule_modified,"data.frame")
rule_modified<-sort(rule_modified, decreasing=TRUE,by="confidence")

summary(rule_modified)
inspect(rule_modified)
plot(rule_modified)
potatoerules <- subset(rule4DF , rules %like% "Movieid=")

inspect( subset( rule4DF, subset = rhs %pin% "Movieid=" ) )


filterrules <- function(rule4DF) {
  pattern=paste("=> {Movieid=",sep="")
  firingrules=rule4DF[grep(pattern,rule4DF$rules,fixed=TRUE),]
  #gsub(" ","",toString(sub("\\}","",sub(".*=> \\{","",firingrules))))
}


Final_Rules = data.frame(filterrules(rule4DF))

# Writing All the Reports 
FinalRulesDF = as(Final_Rules,"data.frame")
write.csv(FinalRulesDF,file="FinalRulesDF.csv",row.names = FALSE)


# Testing the Data

Testdata_Test <- df3_recoded_test[c(1,2)]
colnames(Testdata_Test) <- c("basketID","items")



#Defining Functions  -- Removing Duplicate items from the list
uniqueitems <- function(itemstrg) {
  Y<-unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
  Y<-Y[Y!= ""]
  return(Y)
}


# Execute ruleset using item as rule antecedent

makepreds <- function(items,FinalRulesDF) {
  antecedent=paste("{",items,"} =>",sep="")
  firingrules=FinalRulesDF[grep(antecedent,FinalRulesDF$rules,fixed=TRUE),1]
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
Testdata_Test$preds = apply(Testdata_Test,1,function(X) makepreds(X["items"], FinalRulesDF))
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

Total_Persons_predicted <-sum(apply(userpreds,1,function(X) ifelse(countpreds(X["preds"][[1]])>0,1,0)))

cat("precision=", precision, "corr=",correctpreds,"total=",totalpreds, "Total_Persons_Predicted=",Total_Persons_predicted)

#########################################################################################################################################