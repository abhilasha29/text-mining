#######################################################################
# simple demo of testing a set of association rules against a separate test set
#######################################################################

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
#setwd("Users/abhilashakumari/Web.csv")
movie_master = read.csv("/Users/abhilashakumari/Downloads/Web.csv")
Traindata = subset(movie_master,UserID < 4851 & Ratings >=4 ,select= c(UserID,Title))
Testdata = subset(movie_master,UserID >= 4851 & Ratings >=4 ,select= c(UserID,Title))
trans <- as(split(Traindata[,2], Traindata[,1]), "transactions")

# Rule build based on 20% Support and 50%Confidence
rules <- apriori(trans, parameter = list(supp=0.05, conf=0.05,minlen = 2, maxlen=5))
rules<-sort(rules, decreasing=TRUE,by="confidence")
summary(rules)
inspect(rules)
plot(rules)
plot(rules, measure=c("support", "lift"), shading="confidence")
plot(rules, shading="order", control=list(main = "Two-key plot"))
plot(rules, measure=c("support", "lift"), shading="confidence", interactive=TRUE)

rulesDF = as(rules,"data.frame")


#execute rules against test data
rulesDF = as(rules,"data.frame")
testegs$preds = apply(testegs,1,function(X) makepreds(X["items"], rulesDF))

# extract unique predictions for each test user
userpreds = as.data.frame(aggregate(preds ~ basketID, data = testegs, paste, collapse=","))
userpreds$preds = apply(userpreds,1,function(X) uniqueitems(X["preds"]))

# extract unique items bought (or rated highly) for each test user
baskets = as.data.frame(aggregate(items ~ basketID, data = testegs, paste, collapse=","))
baskets$items = apply(baskets,1,function(X) uniqueitems(X["items"]))

#count how many unique predictions made are correct, i.e. have previously been bought (or rated highly) by the user
correctpreds = sum(apply(userpreds,1,function(X) checkpreds(X["preds"],X["basketID"])))

# count total number of unique predictions made
totalpreds = sum(apply(userpreds,1,function(X) countpreds(X["preds"][[1]]))) 

precision = correctpreds*100/totalpreds

cat("precision=", precision, "corr=",correctpreds,"total=",totalpreds)

#######################################################################
# the supporting functions
#######################################################################

#remove duplicate items from a basket (itemstrg)
uniqueitems <- function(itemstrg) {
  unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
}

# execute ruleset using item as rule antecedent (handles single item antecedents only)
makepreds <- function(item, rulesDF) {
  antecedent = paste("{",item,"} =>",sep="") 
  firingrules = rulesDF[grep(antecedent, rulesDF$rules,fixed=TRUE),1]
  gsub(" ","",toString(sub("\\}","",sub(".*=> \\{","",firingrules))))
}

# count how many predictions are in the basket of items already seen by that user 
# Caution : refers to "baskets" as a global
checkpreds <- function(preds, baskID) {
  plist = preds[[1]]
  blist = baskets[baskets$basketID == baskID,"items"][[1]]
  cnt = 0 
  for (p in plist) {
    if (p %in% blist) cnt = cnt+1
  }
  cnt
}

# count all predictions made
countpreds <- function(predlist) {
  len = length(predlist)
  if (len > 0 && (predlist[[1]] == "")) 0 # avoid counting an empty list
  else len
}

