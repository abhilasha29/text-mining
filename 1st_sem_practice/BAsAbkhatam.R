##########################################
# Building Demo Rules for Movie Lens Database 
# Project : Movie Lens Database 
# Team : Team Dosa
# Date : 29-10-2017
##########################################
install.packages("plyr", dependencies =TRUE)
install.packages("dplyr", dependencies =TRUE)
install.packages("arulesSequences")
install.packages("arules")
install.packages("arulesViz")

library(arulesSequences)
library(arulesViz)
library(arules)
library(plyr)
library(dplyr)
library(ggplot2)

## Setting THe Directory for the WOrking Folder
setwd("/Users/abhilashakumari/Downloads/")
movie_master = read.csv("Web.csv")              
movies = read.csv("/Users/abhilashakumari/Downloads/u_item.csv")
 b            
## Divide Test and Training Data (Ratings above 3,4 and 5 only considered)
Traindata = subset(movie_master,UserID < 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID))
Testdata = subset(movie_master,UserID >= 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID))

Traindata_Film <- data.frame(TId = rep(Traindata$UserID, times = 1), items = c(paste0("Movieid=", Traindata$MovieID)))
Testdata_Film <- data.frame(TId = rep(Testdata$UserID, times = 1), items = c(paste0("Movieid=", Testdata$MovieID)))
########################################################################################################

#Writing the Training and Test Data to Excel
write.csv(Traindata_Film,file="Traindata_Movies.csv",row.names = FALSE)
write.csv(Testdata_Film,file="Testdata_Movies.csv",row.names = FALSE)


Traindata_Movies <- read.transactions(file="Traindata_Movies.csv",rm.duplicates = TRUE,format="single",sep=",", cols=c("TId","items"))
Testdata_Movies <- read.transactions(file="Testdata_Movies.csv",rm.duplicates = TRUE,format="single",sep=",",cols=c("TId","items"))



Traindata_Film <- unique(Traindata_Film)
Testdata_Film <- unique(Testdata_Film)
trans_film_train <- as(split(Traindata_Film$items, Traindata_Film[, "TId"] ), "transactions")
trans_film_test <- as(split(Testdata_Film$items, Testdata_Film[, "TId"] ), "transactions")
inspect(trans_film)
TrainData_Movies_Mlist = as(trans_film_train,"data.frame")
TestData_Movies_Mlist = as(trans_film_test,"data.frame")
write.csv(TestData_Movies_Mlist,file="TestData_Movies_Mlist.csv",row.names = FALSE)


# Rule build based on 20% Support and 50%Confidence ##  834 Values
rule1 <- apriori(Traindata_Movies, parameter = list(supp=0.2, conf=0.5,minlen = 2, maxlen =4))
rule1<-sort(rule1, decreasing=TRUE,by="lift")
summary(rule1)
inspect(rule1)
plot (rule1, method ="graph")
plot(rule1, shading="order", control=list(main = "Two-key plot"))
rule1DF = as(rule1,"data.frame")

# Rule build based on 20% Support and 40%Confidence ## 916 Rules
rule2 <- apriori(Traindata_Movies, parameter = list(supp=0.2, conf=0.4,minlen = 2, maxlen =4)) 
rule2<-sort(rule2, decreasing=TRUE,by="lift")
summary(rule2)
inspect(rule2)
plot (rule1, method ="graph")
plot(rule2, shading="order", control=list(main = "Two-key plot"))
rule2DF = as(rule2,"data.frame")

# Rule build based on 20% Support and 60%Confidence ## 607 Rules
rule3 <- apriori(Traindata_Movies, parameter = list(supp=0.2, conf=0.6,minlen = 2, maxlen =4)) 
rule3<-sort(rule3, decreasing=TRUE,by="lift")
summary(rule3)
inspect(rule3)
plot(rule3, shading="order", control=list(main = "Two-key plot"))
rule3DF = as(rule3,"data.frame")


# Rule build based on 20% Support and 70%Confidence ## 388 RUles
rule4 <- apriori(Traindata_Movies, parameter = list(supp=0.2, conf=0.70,minlen = 2, maxlen =4)) 
rule4<-sort(rule4, decreasing=TRUE,by="lift")
summary(rule4)
inspect(rule4)
plot(rule4)
plot(rule4, shading="order", control=list(main = "Two-key plot"))
rule4DF = as(rule4,"data.frame")

# Rule build based on 20% Support and 80%Confidence ## 200 RUles
rule5 <- apriori(Traindata_Movies, parameter = list(supp=0.2, conf=0.8,minlen = 2, maxlen =4))  
rule5<-sort(rule5, decreasing=TRUE,by="lift")
summary(rule5)
inspect(rule5)
plot(rule5, shading="order", control=list(main = "Two-key plot"))
rule5DF = as(rule5,"data.frame")

# Rule build based on 20% Support and 85%Confidence ## 129 RUles
rule6 <- apriori(Traindata_Movies, parameter = list(supp=0.15, conf=0.90,minlen = 2, maxlen=4))  
rule6<-sort(rule6, decreasing=TRUE,by="lift")
summary(rule6)
inspect(rule6)
plot(rule6, shading="order", control=list(main = "Two-key plot"))
rule6DF = as(rule6,"data.frame")


# Rule build based on 20% Support and 85%Confidence ## 0 RUles
rule7 <- apriori(Traindata_Movies, parameter = list(supp=0.5, conf=0.5,minlen = 2, maxlen=4))  
rule7 <-sort(rule7, decreasing=TRUE,by="lift")
summary(rule7)
inspect(rule7)
plot(rule7, shading="order", control=list(main = "Two-key plot"))
rule7DF = as(rule7,"data.frame")

# Rule build based on 20% Support and 85%Confidence ## 12 RUles
rule8 <- apriori(Traindata_Movies, parameter = list(supp=0.3, conf=0.3,minlen = 2, maxlen=4))  
rule8 <-sort(rule7, decreasing=TRUE,by="lift")
summary(rule8)
inspect(rule8)
plot(rule8, shading="order", control=list(main = "Two-key plot"))
rule8DF = as(rule8,"data.frame")

# Writing the Rules in CSV

write.csv(rule6DF,file="Rules_Film.csv",row.names = FALSE)
write.csv(TestData_Movies_Mlist,file="TestData_Movies_Mlist.csv",row.names = FALSE)

y = read.csv("TestData_Movies_Mlist.csv")
x = read.csv("Rules_Film.csv")


# function Uniq

uniqueitems <- function(itemstrg) {
  Y<-unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
  Y<-Y[Y!= ""]
  return(Y)
}

# Find Predictions
makepreds <- function(items,rulesDemoGenreDF) {
  output = ""
  antecedent=paste("{",items,"} =>",sep="")
  antecedent = gsub("{","",antecedent, fixed = TRUE)
  antecedent = gsub("}","",antecedent, fixed = TRUE)
  antecedent = gsub(" =>","",antecedent, fixed = TRUE)
  asv = as.data.frame(strsplit(antecedent, ",")[[1]]   )
  
  
  antecedent2=as.data.frame(strsplit(as.character(rulesDemoGenreDF), " => ")  )
  ypred  = antecedent2[2,1]
  antecedent2 = gsub("{","",antecedent2[1,1], fixed = TRUE)
  antecedent2 = gsub("}","",antecedent2, fixed = TRUE)
  antecedent2 = gsub(" =>","",antecedent2, fixed = TRUE)
  asv2 = as.data.frame(strsplit(antecedent2, ",")[[1]]   )
  ASVfinal = asv2$`strsplit(antecedent2, ",")[[1]]`%in% asv$`strsplit(antecedent, ",")[[1]]`
  FALSE %in% ASVfinal
  if (FALSE %in% ASVfinal == TRUE)
  {output = ypred}
  return(as.character(output))
  
}

# Taking Oly Uniq

for(i in(1: length(y$V3)))
{
  y[i,4][[1]] = paste(uniqueitems(y[i,3]), collapse= ",")
}

for(i in(1: length(y$V4)))
{
  a2 = gsub("NA,","",y[i,4])
  b2 = gsub("NA","",a2)
  y[i,5] = b2
}
finalY = cbind(y$transactionID,y$V5)


# function Uniq

uniqueitems <- function(itemstrg) {
  Y<-unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
  Y<-Y[Y!= ""]
  return(Y)
}

# Find Predictions
makepreds <- function(items,rulesDemoGenreDF) {
  output = ""
  antecedent=paste("{",items,"} =>",sep="")
  antecedent = gsub("{","",antecedent, fixed = TRUE)
  antecedent = gsub("}","",antecedent, fixed = TRUE)
  antecedent = gsub(" =>","",antecedent, fixed = TRUE)
  asv = as.data.frame(strsplit(antecedent, ",")[[1]]   )
  ss
  
  antecedent2=as.data.frame(strsplit(as.character(rulesDemoGenreDF), " => ")  )
  ypred  = antecedent2[2,1]
  antecedent2 = gsub("{","",antecedent2[1,1], fixed = TRUE)
  antecedent2 = gsub("}","",antecedent2, fixed = TRUE)
  antecedent2 = gsub(" =>","",antecedent2, fixed = TRUE)
  asv2 = as.data.frame(strsplit(antecedent2, ",")[[1]]   )
  ASVfinal = asv2$`strsplit(antecedent2, ",")[[1]]`%in% asv$`strsplit(antecedent, ",")[[1]]`
  FALSE %in% ASVfinal
  if (FALSE %in% ASVfinal == TRUE)
  {output = ypred}
  return(as.character(output))
  
}




# Creating Basic
for ( j in (1:length(x$rules)))
{
  print(j)
  for ( i in (1:length(y$items)))
  {
    outValue = (makepreds((y[i,1]),(x[j,1])))
    if (outValue == "")
    {
      samplething = 2
    }
    else{
      oldValue = as.character(y[i,3])
      y[i,3] = paste(oldValue, outValue, sep=",") 
    }
  }
  
}



# Taking Oly Uniq

for(i in(1: length(y$V3)))
{
  y[i,4][[1]] = paste(uniqueitems(y[i,3]), collapse= ",")
}

for(i in(1: length(y$V4)))
{
  a2 = gsub("NA,","",y[i,4])
  b2 = gsub("NA","",a2)
  y[i,5] = b2
}
finalY = cbind(y$transactionID,y$V5)

# Saving File
write.csv(y, file= "C:\\Praveen\\Studies\\SEMII\\WebAnalytics\\Assignment1\\Assignment_Code\\mergedRules.csv")
save(y, file="C:\\Praveen\\Studies\\SEMII\\WebAnalytics\\Assignment1\\Assignment_Code\\DS.rda")

DataFIle_Prediction = read.csv("mergedRules.csv")

Testdata_Film_Prediction <- DataFIle_Prediction[c(3,6)]
colnames(Testdata_Film_Prediction) <- c("basketID","preds")


# count how many predictions are in the basket of items already seen by that user 

checkpreds <- function(preds,baskID) {
  
  plist=preds[[1]]
  blist=baskets[baskets$TId == baskID,"items"][[1]]
  cnt=0
  for(p in plist) {
    if(p %in% blist) cnt=cnt+1
    print (plist)
  }
  cnt
}


# count all predictions made
countpreds <- function(predlist) {
  len=length(predlist)
  if(len>0 && (predlist[[1]]=="")) 0 
  else len
}


formatcheckpreds <- function(preds,baskID) {
  
  plist=preds[[1]]
  blist=baskets[baskets$TId == baskID,"formatpred"][[1]]
  cnt=0
  for(p in plist) {
    if(p %in% blist) cnt=cnt+1
    print (plist)
  }
  print(cnt)
}


baskets = as.data.frame(aggregate(items ~ basketID, data = Testdata_Film_Prediction, paste, collapse=","))
baskets$items = apply(baskets,1,function(X) uniqueitems(X["items"]))



# formatted baskets pred column
for (i in seq(nrow(baskets))) {
  baskets$formatpred[i] = list(paste("{",baskets[i,2][[1]],"}",sep=""))
}

# formatted baskets pred column
for (i in seq(nrow(userpreds))) {
  userpreds$formatpred[i] = list(strsplit(as.character(userpreds[i,2]), ",")[[1]])
}

totalcnt=0
totalpred=0
totalbList=0
for (i in seq(nrow(userpreds))) {
  plist=userpreds$formatpred[i][[1]]
  blist=baskets$formatpred[i][[1]]
  cnt=0
  for(p in plist) {
    if(p %in% blist) cnt=cnt+1
  }
  totalpreds = totalpreds + length(plist)
  totalbList = totalbList + length(blist) ## Total items in the list
  totalcnt = totalcnt + cnt
}

# extract unique predictions for each test user

userpreds = Testdata_Film_Prediction
precision = totalcnt*100/totalpreds
recall = totalcnt*100/totalbList


cat("precision=", precision, "correctPreds=",totalcnt,"Recall=",recall,"totalPredictions=",totalpreds)


########################################################################################################################################

####################################################

## 2.Considering Movies with Demographic Details##

####################################################

## Divide Test and Training Data (Ratings above 3,4 and 5 only considered)

Traindata_DemoGraphic = subset(movie_master,UserID < 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID,Occupation.Details, Gender, Age))
Testdata_DemoGraphic = subset(movie_master,UserID >= 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID, Occupation.Details, Gender, Age))


Demographic_Train <- data.frame(TId = rep(Traindata_DemoGraphic$UserID, times = 4), items = c(paste0("Movieid=", Traindata_DemoGraphic$MovieID), paste0("Occupation=", Traindata_DemoGraphic$Occupation.Details),paste0("Gender=",Traindata_DemoGraphic$Gender), paste0("Age =",Traindata_DemoGraphic$Age)))
Demographic_Test <- data.frame(TId = rep(Testdata_DemoGraphic$UserID, times = 4), items = c(paste0("Movieid=", Testdata_DemoGraphic$MovieID), paste0("Occupation=", Testdata_DemoGraphic$Occupation.Details),paste0("Gendre=",Testdata_DemoGraphic$Gender), paste0("Age =",Testdata_DemoGraphic$Age)))


#Writing the Training and Test Data to Excel
write.csv(Demographic_Train,file="Demographic_Train.csv",row.names = FALSE)
write.csv(Demographic_Test,file="Demographic_Test.csv",row.names = FALSE)


Traindata_Movies_Demo <- read.transactions(file="Demographic_Train.csv",rm.duplicates = TRUE,format="single",sep=",", cols=c("TId","items"))
Testdata_Movies_Demo <- read.transactions(file="Demographic_Test.csv",rm.duplicates = TRUE,format="single",sep=",",cols=c("TId","items"))


Demographic_Train <- unique(Demographic_Train)
Demographic_Test <- unique(Demographic_Test)
trans_md_train <- as(split(Demographic_Train$items, Demographic_Train[, "TId"] ), "transactions")
trans_md_test <- as(split(Demographic_Test$items, Demographic_Test[, "TId"] ), "transactions")
#inspect(trans_md_train)
TrainData_MD_Mlist = as(trans_md_train,"data.frame")
TestData_MD_Mlist = as(trans_md_test,"data.frame")
write.csv(TrainData_MD_Mlist,file="TrainData_MD_Mlist.csv",row.names = FALSE)
write.csv(TestData_MD_Mlist,file="TestData_MD_Mlist.csv",row.names = FALSE)


rulesDemoMD <- apriori(Traindata_Movies_Demo, parameter = list(supp=0.15, conf=0.40,minlen = 2, maxlen =4)) 
rulesDemoMD<-sort(rulesDemoMD, decreasing=TRUE,by="lift")
rulesDemoDFMD = as(rulesDemoMD,"data.frame")

# Writing the Rules in CSV

filterrules <- function(rulesDemoDF) {
  pattern=paste("=> {Movieid=",sep="")
  firingrules=rulesDemoDF[grep(pattern,rulesDemoDF$rules,fixed=TRUE),]
  #gsub(" ","",toString(sub("\\}","",sub(".*=> \\{","",firingrules))))
}

Final_Rules = data.frame(filterrules(rulesDemoDFMD))



write.csv(Final_Rules,file="Rules_Film_Demo.csv",row.names = FALSE)
write.csv(TestData_MD_Mlist,file="TestData_MD_Mlist.csv",row.names = FALSE)

y = read.csv("TestData_MD_Mlist.csv")
x = read.csv("Rules_Film_Demo.csv")



# function Uniq

uniqueitems <- function(itemstrg) {
  Y<-unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
  Y<-Y[Y!= ""]
  return(Y)
}

# Find Predictions
makepreds <- function(items,rulesDemoGenreDF) {
  output = ""
  antecedent=paste("{",items,"} =>",sep="")
  antecedent = gsub("{","",antecedent, fixed = TRUE)
  antecedent = gsub("}","",antecedent, fixed = TRUE)
  antecedent = gsub(" =>","",antecedent, fixed = TRUE)
  asv = as.data.frame(strsplit(antecedent, ",")[[1]]   )
  
  
  antecedent2=as.data.frame(strsplit(as.character(rulesDemoGenreDF), " => ")  )
  ypred  = antecedent2[2,1]
  antecedent2 = gsub("{","",antecedent2[1,1], fixed = TRUE)
  antecedent2 = gsub("}","",antecedent2, fixed = TRUE)
  antecedent2 = gsub(" =>","",antecedent2, fixed = TRUE)
  asv2 = as.data.frame(strsplit(antecedent2, ",")[[1]]   )
  ASVfinal = asv2$`strsplit(antecedent2, ",")[[1]]`%in% asv$`strsplit(antecedent, ",")[[1]]`
  FALSE %in% ASVfinal
  if (FALSE %in% ASVfinal == TRUE)
  {output = ypred}
  return(as.character(output))
  
}




# Creating Basic
for ( j in (1:length(x$rules)))
{
  print(j)
  for ( i in (1:length(y$items)))
  {
    outValue = (makepreds((y[i,1]),(x[j,1])))
    if (outValue == "")
    {
      samplething = 2
    }
    else{
      oldValue = as.character(y[i,3])
      y[i,3] = paste(oldValue, outValue, sep=",") 
    }
  }
  
}



# Taking Oly Uniq

for(i in(1: length(y$V3)))
{
  y[i,4][[1]] = paste(uniqueitems(y[i,3]), collapse= ",")
}

for(i in(1: length(y$V4)))
{
  a2 = gsub("NA,","",y[i,4])
  b2 = gsub("NA","",a2)
  y[i,5] = b2
}
finalY = cbind(y$transactionID,y$V5)


# function Uniq

uniqueitems <- function(itemstrg) {
  Y<-unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
  Y<-Y[Y!= ""]
  return(Y)
}

# Find Predictions
makepreds <- function(items,rulesDemoGenreDF) {
  output = ""
  antecedent=paste("{",items,"} =>",sep="")
  antecedent = gsub("{","",antecedent, fixed = TRUE)
  antecedent = gsub("}","",antecedent, fixed = TRUE)
  antecedent = gsub(" =>","",antecedent, fixed = TRUE)
  asv = as.data.frame(strsplit(antecedent, ",")[[1]]   )
  ss
  
  antecedent2=as.data.frame(strsplit(as.character(rulesDemoGenreDF), " => ")  )
  ypred  = antecedent2[2,1]
  antecedent2 = gsub("{","",antecedent2[1,1], fixed = TRUE)
  antecedent2 = gsub("}","",antecedent2, fixed = TRUE)
  antecedent2 = gsub(" =>","",antecedent2, fixed = TRUE)
  asv2 = as.data.frame(strsplit(antecedent2, ",")[[1]]   )
  ASVfinal = asv2$`strsplit(antecedent2, ",")[[1]]`%in% asv$`strsplit(antecedent, ",")[[1]]`
  FALSE %in% ASVfinal
  if (FALSE %in% ASVfinal == TRUE)
  {output = ypred}
  return(as.character(output))
  
}




# Creating Basic
for ( j in (1:length(x$rules)))
{
  print(j)
  for ( i in (1:length(y$items)))
  {
    outValue = (makepreds((y[i,1]),(x[j,1])))
    if (outValue == "")
    {
      samplething = 2
    }
    else{
      oldValue = as.character(y[i,3])
      y[i,3] = paste(oldValue, outValue, sep=",") 
    }
  }
  
}



# Taking Oly Uniq

for(i in(1: length(y$V3)))
{
  y[i,4][[1]] = paste(uniqueitems(y[i,3]), collapse= ",")
}

for(i in(1: length(y$V4)))
{
  a2 = gsub("NA,","",y[i,4])
  b2 = gsub("NA","",a2)
  y[i,5] = b2
}
finalY = cbind(y$transactionID,y$V5)

# Saving File
write.csv(y, file= "mergedRules.csv")
#save(y, file="C:\\Praveen\\Studies\\SEMII\\WebAnalytics\\Assignment1\\Assignment_Code\\DS.rda")

DataFIle_Prediction = read.csv("mergedRules.csv")

Testdata_Film_Prediction <- DataFIle_Prediction[c(3,6)]
colnames(Testdata_Film_Prediction) <- c("basketID","preds")
Testdata_DemoGraphic

userpreds = Testdata_Film_Prediction
##############
baskets = as.data.frame(aggregate(items ~ TId, data = Demographic_Test, paste, collapse=","))
baskets$items = apply(baskets,1,function(X) uniqueitems(X["items"]))


# count how many predictions are in the basket of items already seen by that user 

checkpreds <- function(preds,baskID) {
  
  plist=preds[[1]]
  blist=baskets[baskets$TId == baskID,"items"][[1]]
  cnt=0
  for(p in plist) {
    if(p %in% blist) cnt=cnt+1
    print (plist)
  }
  cnt
}


# count all predictions made
countpreds <- function(predlist) {
  len=length(predlist)
  if(len>0 && (predlist[[1]]=="")) 0 
  else len
}


formatcheckpreds <- function(preds,baskID) {
  
  plist=preds[[1]]
  blist=baskets[baskets$TId == baskID,"formatpred"][[1]]
  cnt=0
  for(p in plist) {
    if(p %in% blist) cnt=cnt+1
    print (plist)
  }
  print(cnt)
}


# formatted baskets pred column
for (i in seq(nrow(baskets))) {
  baskets$formatpred[i] = list(paste("{",baskets[i,2][[1]],"}",sep=""))
}

# formatted baskets pred column
for (i in seq(nrow(userpreds))) {
  userpreds$formatpred[i] = list(strsplit(as.character(userpreds[i,2]), ",")[[1]])
}

totalcnt=0
totalpreds=0
totalbList=0
for (i in seq(nrow(userpreds))) {
  plist=userpreds$formatpred[i][[1]]
  blist=baskets$formatpred[i][[1]]
  cnt=0
  for(p in plist) {
    if(p %in% blist) cnt=cnt+1
  }
  totalpreds = totalpreds + length(plist)
  totalbList = totalbList + length(blist) ## Total items in the list
  totalcnt = totalcnt + cnt
}

# extract unique predictions for each test user

userpreds = Testdata_Film_Prediction
precision = totalcnt*100/totalpreds
recall = totalcnt*100/totalbList


cat("precision=", precision, "correctPreds=",totalcnt,"Recall=",recall,"totalPredictions=",totalpreds)


########################################################################################################################################
####################################################

## 3.Considering Movies,Demographic and Genres as Details##

####################################################

Traindata_Demo_Genre = subset(movie_master,UserID < 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID,Occupation.Details, Gender, Age, Genres1))
Testdata_Demo_Genre = subset(movie_master,UserID >= 4851 & Ratings >=3 ,select= c(UserID,Title,MovieID, Occupation.Details, Gender, Age, Genres1))

write.csv(Traindata_Demo_Genre,file="Traindata_Demo_Genre.csv",row.names = FALSE)
write.csv(Testdata_Demo_Genre,file="Traindata_Demo_Genre.csv",row.names = FALSE)

Demo_Genre_Train <- data.frame(TId = rep(Traindata_Demo_Genre$UserID, times = 5), items = c(paste0("Movieid=", Traindata_Demo_Genre$MovieID), paste0("Occupation=", Traindata_Demo_Genre$Occupation.Details),paste0("Gender=",Traindata_Demo_Genre$Gender), paste0("Age =",Traindata_Demo_Genre$Age),paste0("Genre =",Traindata_Demo_Genre$Genres1)))
Demo_Genre_Test <- data.frame(TId = rep(Testdata_Demo_Genre$UserID, times = 5), items = c(paste0("Movieid=", Testdata_Demo_Genre$MovieID), paste0("Occupation=", Testdata_Demo_Genre$Occupation.Details),paste0("Gendre=",Testdata_Demo_Genre$Gender), paste0("Age =",Testdata_Demo_Genre$Age), paste0("Genre =",Testdata_Demo_Genre$Genres1)))

Demo_Genre_Train <- unique(Demo_Genre_Train)
Demo_Genre_Test <- unique(Demo_Genre_Test)
trans <- as(split(Demo_Genre_Train$items, Demo_Genre_Train[, "TId"] ), "transactions")
a <- as(trans,'data.frame')
write.csv(a,file="Hybrid_FIle.csv",row.names = FALSE)
write.csv(Demographic_Train,file="Hybrid_Demo.csv",row.names = FALSE)





y = read.csv("TestData_MD_Mlist.csv")
x = read.csv("Rules_Film_Demo.csv")

# function Uniq

uniqueitems <- function(itemstrg) {
  Y<-unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
  Y<-Y[Y!= ""]
  return(Y)
}

# Find Predictions
makepreds <- function(items,rulesDemoGenreDF) {
  output = ""
  antecedent=paste("{",items,"} =>",sep="")
  antecedent = gsub("{","",antecedent, fixed = TRUE)
  antecedent = gsub("}","",antecedent, fixed = TRUE)
  antecedent = gsub(" =>","",antecedent, fixed = TRUE)
  asv = as.data.frame(strsplit(antecedent, ",")[[1]]   )
  
  
  antecedent2=as.data.frame(strsplit(as.character(rulesDemoGenreDF), " => ")  )
  ypred  = antecedent2[2,1]
  antecedent2 = gsub("{","",antecedent2[1,1], fixed = TRUE)
  antecedent2 = gsub("}","",antecedent2, fixed = TRUE)
  antecedent2 = gsub(" =>","",antecedent2, fixed = TRUE)
  asv2 = as.data.frame(strsplit(antecedent2, ",")[[1]]   )
  ASVfinal = asv2$`strsplit(antecedent2, ",")[[1]]`%in% asv$`strsplit(antecedent, ",")[[1]]`
  FALSE %in% ASVfinal
  if (FALSE %in% ASVfinal == TRUE)
  {output = ypred}
  return(as.character(output))
  
}




# Creating Basic
for ( j in (1:length(x$rules)))
{
  print(j)
  for ( i in (1:length(y$items)))
  {
    outValue = (makepreds((y[i,1]),(x[j,1])))
    if (outValue == "")
    {
      samplething = 2
    }
    else{
      oldValue = as.character(y[i,3])
      y[i,3] = paste(oldValue, outValue, sep=",") 
    }
  }
  
}



# Taking Oly Uniq

for(i in(1: length(y$V3)))
{
  y[i,4][[1]] = paste(uniqueitems(y[i,3]), collapse= ",")
}

for(i in(1: length(y$V4)))
{
  a2 = gsub("NA,","",y[i,4])
  b2 = gsub("NA","",a2)
  y[i,5] = b2
}
finalY = cbind(y$transactionID,y$V5)


# function Uniq

uniqueitems <- function(itemstrg) {
  Y<-unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
  Y<-Y[Y!= ""]
  return(Y)
}

# Find Predictions
makepreds <- function(items,rulesDemoGenreDF) {
  output = ""
  antecedent=paste("{",items,"} =>",sep="")
  antecedent = gsub("{","",antecedent, fixed = TRUE)
  antecedent = gsub("}","",antecedent, fixed = TRUE)
  antecedent = gsub(" =>","",antecedent, fixed = TRUE)
  asv = as.data.frame(strsplit(antecedent, ",")[[1]]   )
  ss
  
  antecedent2=as.data.frame(strsplit(as.character(rulesDemoGenreDF), " => ")  )
  ypred  = antecedent2[2,1]
  antecedent2 = gsub("{","",antecedent2[1,1], fixed = TRUE)
  antecedent2 = gsub("}","",antecedent2, fixed = TRUE)
  antecedent2 = gsub(" =>","",antecedent2, fixed = TRUE)
  asv2 = as.data.frame(strsplit(antecedent2, ",")[[1]]   )
  ASVfinal = asv2$`strsplit(antecedent2, ",")[[1]]`%in% asv$`strsplit(antecedent, ",")[[1]]`
  FALSE %in% ASVfinal
  if (FALSE %in% ASVfinal == TRUE)
  {output = ypred}
  return(as.character(output))
  
}




# Creating Basic
for ( j in (1:length(x$rules)))
{
  print(j)
  for ( i in (1:length(y$items)))
  {
    outValue = (makepreds((y[i,1]),(x[j,1])))
    if (outValue == "")
    {
      samplething = 2
    }
    else{
      oldValue = as.character(y[i,3])
      y[i,3] = paste(oldValue, outValue, sep=",") 
    }
  }
  
}



# Taking Oly Uniq

for(i in(1: length(y$V3)))
{
  y[i,4][[1]] = paste(uniqueitems(y[i,3]), collapse= ",")
}

for(i in(1: length(y$V4)))
{
  a2 = gsub("NA,","",y[i,4])
  b2 = gsub("NA","",a2)
  y[i,5] = b2
}
finalY = cbind(y$transactionID,y$V5)

# Saving File
write.csv(y, file= "mergedRules.csv")
#save(y, file="C:\\Praveen\\Studies\\SEMII\\WebAnalytics\\Assignment1\\Assignment_Code\\DS.rda")

DataFIle_Prediction = read.csv("mergedRules.csv")

Testdata_Film_Prediction <- DataFIle_Prediction[c(3,6)]
colnames(Testdata_Film_Prediction) <- c("basketID","preds")


# count how many predictions are in the basket of items already seen by that user 

checkpreds <- function(preds,baskID) {
  
  plist=preds[[1]]
  blist=baskets[baskets$TId == baskID,"items"][[1]]
  cnt=0
  for(p in plist) {
    if(p %in% blist) cnt=cnt+1
    print (plist)
  }
  cnt
}


# count all predictions made
countpreds <- function(predlist) {
  len=length(predlist)
  if(len>0 && (predlist[[1]]=="")) 0 
  else len
}


formatcheckpreds <- function(preds,baskID) {
  
  plist=preds[[1]]
  blist=baskets[baskets$TId == baskID,"formatpred"][[1]]
  cnt=0
  for(p in plist) {
    if(p %in% blist) cnt=cnt+1
    print (plist)
  }
  print(cnt)
}


# formatted baskets pred column
for (i in seq(nrow(baskets))) {
  baskets$formatpred[i] = list(paste("{",baskets[i,2][[1]],"}",sep=""))
}

# formatted baskets pred column
for (i in seq(nrow(userpreds))) {
  userpreds$formatpred[i] = list(strsplit(as.character(userpreds[i,2]), ",")[[1]])
}

totalcnt=0
totalpred=0
totalbList=0
for (i in seq(nrow(userpreds))) {
  plist=userpreds$formatpred[i][[1]]
  blist=baskets$formatpred[i][[1]]
  cnt=0
  for(p in plist) {
    if(p %in% blist) cnt=cnt+1
  }
  totalpreds = totalpreds + length(plist)
  totalbList = totalbList + length(blist) ## Total items in the list
  totalcnt = totalcnt + cnt
}

# extract unique predictions for each test user

userpreds = Testdata_Film_Prediction
precision = totalcnt*100/totalpreds
recall = totalcnt*100/totalbList


cat("precision=", precision, "correctPreds=",totalcnt,"Recall=",recall,"totalPredictions=",totalpreds)


