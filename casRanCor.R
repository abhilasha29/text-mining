cycle<- read.csv("/Users/abhilashakumari/Documents/Day.csv")
#exploration analysis
str(cycle)
summary(cycle)

#missing values in all the variables from the data
colSums(is.na(cycle))

#Data preparation 
#step1
#manupulating the varible class and format
#converting data into date format
cycle$dteday<-as.Date(cycle$dteday, format = "%Y-%d-%m")

#imputing missing values in dates
cycle$dteday<-seq.Date(as.Date("2011/01/01"), as.Date("2012/12/31"), by = 1)

#converting season variable with season names
cycle$seasonspringer<-ifelse(cycle$season==1,1,0)
cycle$seasonsummer<-ifelse(cycle$season==2,1,0)
cycle$seasonfall<-ifelse(cycle$season==3,1,0)
cycle$seasonwinter<-ifelse(cycle$season==4,1,0)

#months dummy variable
for(t in unique(cycle$mnth)) {
  cycle[paste("month",t,sep="")] <- ifelse(cycle$mnth==t,1,0)
}

#weekday dummy variable
for(t in unique(cycle$weekday)) {
  cycle[paste("weekday",t,sep="")] <- ifelse(cycle$weekday==t,1,0)
}

#converting weather variable with season names
cycle$weatherclear<-ifelse(cycle$weathersit==1,1,0)
cycle$weathercloudy<-ifelse(cycle$weathersit==2,1,0)
cycle$weatherslightrain<-ifelse(cycle$weathersit==3,1,0)
cycle$weatherheavyrain<-ifelse(cycle$weathersit==4,1,0)

#prepared data
#eliminating the variable converted into dummy 
cycleprep<-subset( cycle, select = -c(season,mnth,weekday,weathersit) )

#outlier detection
#visulizing the outliers using the box plot and scatter plot over time
library(ggplot2)
with(cycleprep,qplot(y=casual,x=1,geom = "boxplot"))+ggtitle("Cycle demand distribution boxplot") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

#visulizing the distribution of cycle demand using histogram
hist(cycleprep$casual, breaks=10, main = "Histogram-Distribution of cycle demand")

#ploting overall sales for the two years by training and testing groups
#visulizing outliers using the scatter plot over time
# Create a dataframe with train, test and group indicator
group <- ifelse(cycle$yr==0,"Train","Test")
df <- data.frame(Date=cycle$dteday,Sales=cycle$casual,group)

# ...and plot it
ggplot(df,aes(x = Date,y = Sales, color = group)) + geom_point() +
  scale_color_discrete(name="") + theme(legend.position="top")+
  ggtitle("Distribution of Cycle demand over time") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))




#demand forecast logic in place (yesterday's actual demand is tomorrows demand)
library(DataCombine)

cycleprep <- slide(cycleprep, Var = "casual", slideBy = -2)

#renaming the expected demand variable
names(cycleprep)[names(cycleprep) == 'casual-2'] <- 'expecteddemand'

cycleprep <- slide(cycleprep, Var = "casual", slideBy = 2)

#renaming the actual demand variable
names(cycleprep)[names(cycleprep) == 'casual2'] <- 'actualdemand'

##Derived variables 
#average demand (lag3)
cycleprep <- slide(cycleprep, Var = "casual", slideBy = -2)
cycleprep <- slide(cycleprep, Var = "casual", slideBy = -3)
cycleprep <- slide(cycleprep, Var = "casual", slideBy = -4)

casuallagvector<-paste0("casual-",2:4)
max<-3
for(i in 1:(nrow(cycleprep)))
{
  for(j in max:1)
  {
    if(j==1)
    {
      break
    }
    else
    {
      z=j-1
      if(j==max)
      {
        y<-(cycleprep[i,casuallagvector[j]]+cycleprep[i,casuallagvector[z]])/2
      }
      else
      {
        j1<-j-1
        y<- (cycleprep[i,casuallagvector[j1]]+y)/2
      }
    }
  }
  cycleprep[i,c("malag2to4")]<-y
}



###############################################################################################
##business problem
busprep<-cycleprep[complete.cases(cycleprep),]

#revenue for the day
busprep$revenue<-ifelse(busprep$casual<busprep$expecteddemand, busprep$casual*3,busprep$expecteddemand*3)

#cost for the day
busprep$cost<-busprep$expecteddemand * 2

#profit for the day
busprep$profit<-busprep$revenue - busprep$cost
######################################################################


#total revenue for the years 2011 and 2012
(sum(busprep$casual))*3

##total revenue by year
tapply(busprep$casual*3,busprep$yr,FUN = sum)

##total revenue by casual customers by year
tapply(busprep$casual*3,busprep$yr,FUN = sum)

##total revenue by casual customers by year
tapply(busprep$casual*3,busprep$yr,FUN = sum)

#total profit for the years 2011 and 2012
(sum(busprep$profit,na.rm = TRUE))

##total profit by year

piedf<-as.data.frame(tapply(busprep$profit,busprep$yr,FUN = sum,na.rm = TRUE))
piedf$years<-row.names(piedf)
piedf$years<-ifelse(piedf$years==0,"2011","2012")
names(piedf)<-c("Profit","Years")

#pie chart for profit by year 
# Define some colors ideal for black & white print
barplot(piedf$Profit, main=paste("Profit for", labels=paste(piedf$Years,paste0("$",piedf$Profit))))




##############################################################################################

##############################################################################################

##FOrecasting the demand 
##SINGLE MODEL
#creating training and testing data
train<-busprep[which(busprep$yr==0),]
test<-busprep[which(busprep$yr==1),]


#######################################################################
#ploting overall sales for the two years by training and testing groups
library(ggplot2)

# Create a dataframe with train, test and group indicator
group <- ifelse(busprep$yr==0,"Train","Test")
df <- data.frame(Date=busprep$dteday,Sales=busprep$casual,group)

# ...and plot it
ggplot(df,aes(x = Date,y = Sales, color = group)) + geom_point() +
  scale_color_discrete(name="") + theme(legend.position="top")



#ploting for casual sales
# Create a dataframe with train, test and group indicator
group <- ifelse(busprep$yr==0,"Train","Test")
df <- data.frame(date=busprep$dteday,CasualSales=busprep$casual,group)

# ...and plot it
ggplot(df,aes(x = date,y = CasualSales, color = group)) + geom_point() +
  scale_color_discrete(name="") + theme(legend.position="top")



#ploting for casual sales
# Create a dataframe with train, test and group indicator
group <- ifelse(busprep$yr==0,"Train","Test")
df <- data.frame(date=busprep$dteday,casualSales=busprep$casual,group)

# ...and plot it
ggplot(df,aes(x = date,y = casualSales, color = group)) + geom_point() +
  scale_color_discrete(name="") + theme(legend.position="top")
#############################################################################################

#Finding the correlation between the continuous variables
cordf<-busprep[,c("temp","atemp","hum","windspeed",
                  "malag2to4","actualdemand","casual","casual")]
library(Hmisc)
rcorr(as.matrix(cordf), type="pearson")


busprep <-busprep[complete.cases(busprep),]

#######################################################
#default model
# default model - predict the mean of the training data
best.guess <- test$expecteddemand
dim(best.guess)
# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-test$casual)^2))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-test$casual))
MAE.baseline

busprept<-busprep[which(busprep$yr==1),]
busprept$best.guess<-best.guess
#############################
#############################
#removing the variable not needed for modelling
train1<-train[ , !names(train) %in% c("cost","instant","dteday","yr","seasonwinter","month3","weekday0","weatherheavyrain",
                                      "expecteddemand","revenue","profit","atemp","hum",casuallagvector)]


test1<-test[ , !names(test) %in% c("cost","instant","dteday","yr","seasonwinter","month3","weekday0","weatherheavyrain",
                                   "expecteddemand","revenue","profit","atemp","hum",casuallagvector)]


############################################################################################
##Modelling
#Multiple linear regression model
#Create a multiple (log)linear regression model using the training data
#Considering only highly significant variables
lin.reg <- lm(actualdemand ~ holiday+workingday+temp+seasonfall+month1+month2+month4+month5+month6+month7+month10+month12+weekday1+weekday2+weekday3+weekday6+malag2to4, data = train1)

#

#Inspect the model
summary(lin.reg)

# What is the multiplicative effect of the atemp variable?
lin.reg$coefficients["month1"]

#Apply the model to the testing data
# exponentiate the results to revert the log transformation
test.pred.lin <- round(predict(lin.reg,test1))
test.pred.lin<-test.pred.lin[1:(length(test.pred.lin)-2)]
# evaluate the accuracy
RMSE.lin.reg <- sqrt(mean((test.pred.lin-test1[-(1:2),c("actualdemand")])^2))
RMSE.lin.reg

MAE.lin.reg <- mean(abs(test.pred.lin-test1[-(1:2),c("actualdemand")]))
MAE.lin.reg

busprept<-busprept[-(1:2),] 
busprept$test.pred.lin <- test.pred.lin
#######################################################
########################################################
#revenue for the day
busprept$revenuetest.pred.lin<-ifelse(busprept$casual<test.pred.lin, busprept$casual*3,test.pred.lin*3)

#cost for the day
busprept$costtest.pred.lin<-test.pred.lin * 2

#profit for the day
busprept$profittest.pred.lin<-busprept$revenuetest.pred.lin - busprept$costtest.pred.lin


###################################################

#Decision tree
#Needed to grow a tree
library(rpart)
library(rpart.plot)
# To draw a pretty tree (fancyRpartPlot function)
library(rattle)

# rpart function applied to a numeric variable => regression tree
rt <- rpart(actualdemand ~ ., data=train1)

# Full-grown tree with x splits using y different variables 
# (Not running the line below - do it to see the tree)
fancyRpartPlot(rt)
rpart.plot(rt)
# As always, predict and evaluate on the test set
test.pred.rtree <- round(predict(rt,test1)) 
test.pred.rtree<-test.pred.rtree[1:(length(test.pred.rtree)-2)]

RMSE.rtree <- sqrt(mean((test.pred.rtree-test1[-(1:2),c("actualdemand")])^2))
RMSE.rtree

MAE.rtree <- mean(abs(test.pred.rtree-test1[-(1:2),c("actualdemand")]))
MAE.rtree

busprept$test.pred.rtree <- test.pred.rtree

#######################################################
########################################################
#revenue for the day
busprept$revenuetest.pred.rtree<-ifelse(busprept$casual<test.pred.rtree, busprept$casual*3,test.pred.rtree*3)

#cost for the day
busprept$costtest.pred.rtree<-test.pred.rtree * 2

#profit for the day
busprept$profittest.pred.rtree<-busprept$revenuetest.pred.rtree - busprept$costtest.pred.rtree


#############################
#############################
#random forest model
# Needed to run the algorithm
library(randomForest)

# Convert some factor variables to numeric (train and test sets)
# For reproducibility; 123 has no particular meaning
# Run this immediately before creating the random forest
set.seed(123)

# Create a random forest with 1000 trees
rf <- randomForest(actualdemand ~ ., data = train1, importance = TRUE, ntree=178)

# How many trees are needed to reach the minimum error estimate? 
# This is a simple problem; it appears that about 178 trees would be enough. 
which.min(rf$mse)

# Plot rf to see the estimated error as a function of the number of trees
plot(rf,main = "Error rate to the number of trees")


# Using the importance()  function to calculate the importance of each variable
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

# As usual, predict and evaluate on the test set
test.pred.forest <- round(predict(rf,test1))
test.pred.forest<-test.pred.forest[1:(length(test.pred.forest)-2)]


RMSE.forest <- sqrt(mean((test.pred.forest-test1[-(1:2),c("actualdemand")])^2))
RMSE.forest

MAE.forest <- mean(abs(test.pred.forest-test1[-(1:2),c("actualdemand")]))
MAE.forest

busprept$test.pred.forest <- test.pred.forest

#######################################################
########################################################
#revenue for the day
busprept$revenuetest.pred.forest<-ifelse(busprept$casual<test.pred.forest, busprept$casual*3,test.pred.forest*3)

#cost for the day
busprept$costtest.pred.forest<-test.pred.forest * 2

#profit for the day
busprept$profittest.pred.forest<-busprept$revenuetest.pred.forest - busprept$costtest.pred.forest

#############################################################
#setting seed and initiating nnet package
set.seed(177)
library(nnet)

neural<- nnet(actualdemand ~ .,data=train1,size=1, linout=TRUE, skip=TRUE, MaxNWts=100,trace=FALSE, maxit=1000,decay = 0.01)

#running the built model on the test set
nnpred <-round(predict(neural, newdata=test1))
nnpred<-nnpred[1:(length(nnpred)-2)]

#Calculating RMSE and MAE
RMSE.nn <- sqrt(mean((nnpred-test1[-(1:2),c("actualdemand")])^2))
RMSE.nn

MAE.nn <- mean(abs(nnpred-test1[-(1:2),c("actualdemand")]))
MAE.nn

busprept$nnpred <- nnpred

#######################################################
########################################################
#revenue for the day
busprept$revenuennpred<-ifelse(busprept$casual<nnpred, busprept$casual*3,nnpred*3)

#cost for the day
busprept$costnnpred<-nnpred * 2

#profit for the day
busprept$profitnnpred<-busprept$revenuennpred - busprept$costnnpred


#############################
#############################
##Creating an ensemble model with the exisiting models
##Linear regression, random forest, regressiontrees, NN
##averaging the predictions from the four models to have the final prediction value
busprept$ensemblepred<-round(((busprept$test.pred.lin+busprept$test.pred.rtree+busprept$test.pred.forest+busprept$nnpred)/4))


#Calculating RMSE and MAE
RMSE.ensemble <- sqrt(mean((busprept$ensemblepred-busprept$actualdemand)^2))
RMSE.ensemble

MAE.ensemble <- mean(abs(busprept$ensemblepred-busprept$actualdemand))
MAE.ensemble

ensemblepred<-busprept$ensemblepred

#######################################################
########################################################
#revenue for the day
busprept$revenueensemblepred<-ifelse(busprept$casual<ensemblepred, busprept$casual*3,ensemblepred*3)

#cost for the day
busprept$costensemblepred<-ensemblepred * 2

#profit for the day
busprept$profitensemblepred<-busprept$revenueensemblepred - busprept$costensemblepred


###############################################################################################


##random forest model is found to be the best model
##rebuilding the model every month for the last 12 months and predicting 

#calculating the MAE for the existing decision tree model by month
constantdt<-data.frame(time= test[-(1:2),c("dteday")],preddemand= test.pred.rtree, actual= test1[-(1:2),c("actualdemand")]) 

constantdt$er <- abs(constantdt$preddemand-constantdt$actual)

#  Get months
constantdt$month <- as.factor(months(constantdt$time))
target<-c("January","February","March","April","May","June","July","August",
          "September","October","November", "December")
require(gdata)
constantdt$month <- reorder.factor(constantdt$month, new.order=target)
#caluclating and ploting the error(MAE) by month 
#  Aggregate 'X2' on months and year and get mean
singlemodelaccuracy<-aggregate( er ~ month , constantdt , mean )
plot(aggregate( er ~ month , constantdt , mean ))


##################
#building individual models for every month and with the last 12 months data

busprep$uniquedate<-paste(months(busprep$dteday),ifelse(busprep$yr==0,2011,2012),sep=" ")

x<-unique(busprep$uniquedate)
x1<-unique(busprep$uniquedate)[(1:(length(unique(busprep$uniquedate))-12))]
predbymonth<-data.frame(monthyr=NA,maevaldynamic=NA)
twelvemonthrollingpred<-NA
for(i in x1)
{
  index<- match(i,x)
  j<-index+11
  train <- busprep[ (busprep$uniquedate  %in% x[(j-11):j]),]
  test <-  busprep[ (busprep$uniquedate  %in% x[(j+1)]),]
  
  train1<-train[ , !names(train) %in% c("cost","instant","dteday","yr","seasonwinter","month3","weekday0","weatherheavyrain",
                                        "expecteddemand","revenue","profit","atemp","hum",casuallagvector,"uniquedate")]
  
  
  test1<-test[ , !names(test) %in% c("cost","instant","dteday","yr","seasonwinter","month3","weekday0","weatherheavyrain",
                                     "expecteddemand","revenue","profit","atemp","hum",casuallagvector,"uniquedate")]
  
  
  set.seed(123)
  # Create a random forest with 1000 trees
  rf <- randomForest(actualdemand ~ ., data = train1, importance = TRUE, ntree=250)
  
  # As always, predict and evaluate on the test set
  predval <- predict(rf,test1) 
  
  maeval <- mean(abs(predval-test1$actualdemand))
  vec<-c(x[(j+1)],maeval)
  predbymonth <-rbind(predbymonth,vec)
  twelvemonthrollingpred<-c(twelvemonthrollingpred,predval)
}
predbymonth<-predbymonth[-1,]
twelvemonthrollingpred<-twelvemonthrollingpred[-1]
twelvemonthrollingpred<-round(twelvemonthrollingpred)
twelvemonthrollingpred<-twelvemonthrollingpred[1:(length(twelvemonthrollingpred)-2)]

#creating training and testing data
train1<-busprep[which(busprep$yr==0),]
test1<-busprep[which(busprep$yr==1),]

#Calculating RMSE and MAE
RMSE.12 <- sqrt(mean((twelvemonthrollingpred-test1[-(1:2),c("actualdemand")])^2))
RMSE.12

MAE.12 <- mean(abs(twelvemonthrollingpred-test1[-(1:2),c("actualdemand")]))
MAE.12

busprept$twelvemonthrollingpred <- twelvemonthrollingpred

#######################################################
########################################################
#revenue for the day
busprept$revenuetwelvemonthrollingpred<-ifelse(busprept$casual<twelvemonthrollingpred, busprept$casual*3,twelvemonthrollingpred*3)

#cost for the day
busprept$costtwelvemonthrollingpred<-twelvemonthrollingpred * 2

#profit for the day
busprept$profittwelvemonthrollingpred<-busprept$revenuetwelvemonthrollingpred - busprept$costtwelvemonthrollingpred

##total profit by year
profitdefault<-sum(busprep[which(busprep$yr==1),"profit"],na.rm = TRUE)
profitrandomforest<-sum(busprept$profittest.pred.forest,na.rm = TRUE)
profit12monthsrolling<-sum(busprept$profittwelvemonthrollingpred,na.rm = TRUE)
graphtemp1<-data.frame(Profit=c(profitdefault,profitrandomforest,profit12monthsrolling))
graphtemp1$Models<-c("Default model","Random forest","Random forest 12 months rolling")
barchart(Profit~Models,data=graphtemp1,xlab="Models",ylab="Profit in $",main="Comparing profits for various models")

#############################
#############################
##traing the model with 18 months data and testing it with the last 6 months data

train <- busprep[ (busprep$uniquedate  %in% x[1:18]),]
test <-  busprep[ (busprep$uniquedate  %in% x[19:24]),]
train1<-train[ , !names(train) %in% c("cost","instant","dteday","yr","seasonwinter","month3","weekday0","weatherheavyrain",
                                      "expecteddemand","revenue","profit","atemp","hum",casuallagvector,"uniquedate")]


test1<-test[ , !names(test) %in% c("cost","instant","dteday","yr","seasonwinter","month3","weekday0","weatherheavyrain",
                                   "expecteddemand","revenue","profit","atemp","hum",casuallagvector,"uniquedate")]

# rpart function applied to a numeric variable => regression tree
rf <- randomForest(actualdemand ~ ., data = train1, importance = TRUE, ntree=250)

# As always, predict and evaluate on the test set
predvaleighteenmonthstrain <- predict(rf,test1)
predvaleighteenmonthstrain<-predvaleighteenmonthstrain[1:(length(predvaleighteenmonthstrain)-2)]

#Calculating RMSE and MAE
RMSE.18 <- sqrt(mean((predvaleighteenmonthstrain-test1[-(1:2),c("actualdemand")])^2))
RMSE.18

MAE.18 <- mean(abs(predvaleighteenmonthstrain-test1[-(1:2),c("actualdemand")]))
MAE.18

#comparing with the single decision tree model with 12 months training and testing
RMSE.forest
MAE.forest

##DATA balancing 
##traing the model with 12 months data {(jan to june[2011,2012] (half number of records selected randomly))+(july to dec[2011]) }
##and testing it with the last 6 months data
temp <- busprep[ (busprep$uniquedate  %in% x[c(1:6,13:18)]),]
temp1<- busprep[ (busprep$uniquedate  %in% x[7:12]),]
temp2<-rbind(temp,temp1)
set.seed(1234)
train <- temp2[sample(1:nrow(temp2), 50,
                      replace=FALSE),]
test <-  busprep[ (busprep$uniquedate  %in% x[19:24]),]

train1<-train[ , !names(train) %in% c("cost","instant","dteday","yr","seasonwinter","month3","weekday0","weatherheavyrain",
                                      "expecteddemand","revenue","profit","atemp","hum",casuallagvector,"uniquedate")]


test1<-test[ , !names(test) %in% c("cost","instant","dteday","yr","seasonwinter","month3","weekday0","weatherheavyrain",
                                   "expecteddemand","revenue","profit","atemp","hum",casuallagvector,"uniquedate")]

# rpart function applied to a numeric variable => regression tree
rf <- randomForest(actualdemand ~ ., data = train1, importance = TRUE, ntree=250)

# As always, predict and evaluate on the test set
predvaleighteenmonthsdatabalancetrain <- predict(rf,test1) 
predvaleighteenmonthsdatabalancetrain <- predvaleighteenmonthsdatabalancetrain[1:(length(predvaleighteenmonthsdatabalancetrain)-2)]

#Calculating RMSE and MAE
RMSE.18databalance <- sqrt(mean((predvaleighteenmonthsdatabalancetrain-test1[-(1:2),c("actualdemand")])^2))
RMSE.18databalance

MAE.18databalance <- mean(abs(predvaleighteenmonthsdatabalancetrain-test1[-(1:2),c("actualdemand")]))
MAE.18databalance

########################
#Plotting single model vs dynamic model by month
#merging two accuracy dataset for plotting
singlemodelaccuracy$monthyr<-paste(singlemodelaccuracy$month,2012,sep=" ")
singlemodelaccuracy$month<-NULL
finalacc<-merge(predbymonth,singlemodelaccuracy,by=c("monthyr"))

finalacc$maevaldynamic<-as.numeric(finalacc$maevaldynamic)
finalacc$monthyr <- as.factor(finalacc$monthyr)
levels(finalacc$monthyr)<-c("January 2012","February 2012","March 2012","April 2012","May 2012","June 2012","July 2012","August 2012",
                            "September 2012","October 2012","November 2012", "December 2012") 


#graph
#line graph
library(reshape)
mdata <- melt(finalacc, id=c("monthyr"))
mdata$variable<-ifelse(mdata$variable=="maevaldynamic","12 months rolling window model MAE output","Single model MAE output")
pd <- position_dodge(.1)
p<-ggplot(mdata, aes(x=monthyr, y=value, color=variable)) 
p+geom_line(position=pd,aes(group=variable))



#The differences in the error- bars 
# Use a consistent y range
ymax <- max(mdata$value)
ymin <- min(mdata$value)

# Plot the individuals
ggplot(mdata, aes(x=variable, y=value, colour=monthyr, group=monthyr)) +
  geom_line() + geom_point(shape=21, fill="white") + 
  ylim(ymin,ymax)

#calculating final profits
graphfinal<-graphtemp1
graphfinal$Profit<-graphtemp1$Profit+graphtemp$Profit

# final profit plot
barchart(Profit~Models,data=graphfinal,xlab="Models",ylab="Profit in $",main="Comparing profits for various models")
