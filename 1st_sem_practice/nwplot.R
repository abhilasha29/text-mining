Traindata = subset(movie_master,UserID < 4851 & Ratings >=3 ,select= c(UserID,Title))
movie_master = read.csv("Web_master.csv")

Traindata_Movies <- read.transactions(file="Traindata_Movies.csv",rm.duplicates = TRUE,format="single",sep=",",cols=c("UserID","Title"))

rule61 <- apriori(Traindata_Movies, parameter = list(supp=0.2, conf=0.85,minlen = 2))  
rule61<-sort(rule61, decreasing=TRUE,by="lift")
summary(rule61)
inspect(rule61)
plot(rule61, shading="order", control=list(main = "Two-key plot"))
rule6DF = as(rule6,"data.frame")
str(Traindata_Movies)
plot(head(rule61,10),method = "graph",pch=200)
