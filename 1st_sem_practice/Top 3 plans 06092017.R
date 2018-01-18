#Clean Memory
rm(list=ls())
gc()

#Load Library
library("RGA")# Gets data from GA
library("dplyr")# Manipulating Datasets
#library("rworldmap")# Matches countries to regions
library("arules")# Generates Rules
#library("arulesViz")# Visuaizes Rules
#library(mefa) #repeat data frame
#library("splitstackshape")

#GA Credentials
authorize()

#GA Settings
viewID_SG = ""
startDate = as.character(Sys.Date()-182)
endDate = as.character(Sys.Date()-2)
##Data Extraction##

#Read data from GA

#GA_Data=get_ga(viewID_SG, startDate, endDate, metrics=c("ga:itemRevenue","ga:itemQuantity"),
#                 dimensions=c("ga:transactionId","ga:channelGrouping","ga:dayOfWeekName","ga:deviceCategory","ga:productBrand"),
#                 filters=c("ga:itemQuantity>0"),fetch.by=c("month"))

GA_Data_All= get_ga(viewID_SG, startDate, endDate, metrics=c("ga:itemQuantity"),
                dimensions=c("ga:transactionId","ga:dimension3","ga:productName","ga:channelGrouping"),
                filters=c("ga:itemQuantity>0"),fetch.by=c("month"))

#A=left_join(A_Custom_Dimensions,A_Generic)

#Rename Columns 
colnames(GA_Data_All)=c("Transaction_ID","Destination","Product","Channel","Click_Outs")

GA_Data_All=subset(GA_Data_All, Channel!="Display")
GA_Data_All$Channel <- NULL

##
#GA_Data_All=GA_Data_All[,c("Transaction_ID_1","Destination","Product","Click_Outs","Transaction_ID_3")]
#colnames(GA_Data_All)=c("Transaction_ID","Destination","Product","Click_Outs","SKU Code")
##

B=strsplit(as.character(GA_Data_All[,1]),"\\|")
##Find Erroneous Transaction IDs
C=list(0)
for (i in 1:length(B)){
  C[i]=(length(B[[i]]))
}

e=GA_Data_All[-which(C!=3),] 
B=strsplit(as.character(e[,1]),"\\|")
sku.code=data.frame(sapply(B,"[[",3))
sku.code=cbind(sku.code,e)
names(sku.code)=c("SKU Code","Transaction_ID","Destination","Product","Click_Outs")



#Plans that are currently live (will be JSON input)
Live_Plans=read.csv("Live Plans.csv")
colnames(Live_Plans)=c("Product","SKU Code")

Check = sku.code$`SKU Code` %in% Live_Plans$`SKU Code`
GA_Data_SKU=sku.code[Check,]

GA_Data=merge(GA_Data_SKU, Live_Plans, by="SKU Code")
GA_Data=GA_Data[,c("Transaction_ID","Destination","Product.y","Click_Outs")]
colnames(GA_Data)=c("Transaction_ID","Destination","Product","Click_Outs")

#Country list from our data
ga_country=as.data.frame(GA_Data$Destination)
names(ga_country)=c("Destination")
country=read.csv("data.csv",header=TRUE)
names(country)=c("Country","Region")
#Fuzzy Match
test=adist(as.character(ga_country$Destination),as.character(country$Country))
colnames(test)=country[,1]
rownames(test)=ga_country[,1]

match="matching.country"
match.value="matching.score"
for (i in 1:nrow(test)){
  match[i]=names((which.min(test[i,])))
  match.value[i]=min(test[i,])
}


#Merge Data
Country_Data=data.frame(GA_Data,match,match.value)
names(Country_Data)=c(names(GA_Data),"Matched_Country","Matching_Score")

##Data Modelling##
##Finding Rules for Single Countries##
Data=as.data.frame(apply(Country_Data[-c(1,2,4,6)],2,as.factor))

##Data2=as.data.frame(Country_Data[-c(1,2,4,6)])


data=as (Data,"transactions")
#itemFrequencyPlot(data,topN=20,type="relative",popCol="red")

#rules=apriori(data,parameter=list(minlen=5,maxlen=8,support=0.01,confidence=0.5),
#             appearance = list(rhs = c("Product=Etiqa Basic"), default = "lhs"))

#rules=apriori(data,parameter=list(minlen=7,maxlen=7,support=0.01,confidence=0.1),
#              appearance = list(rhs = c("Destination=Thailand","Destination=Indonesia"), default = "lhs"))

rules=apriori(data,parameter=list(minlen=2,maxlen=10,support=0.0001,confidence=0.0001))

#ruleshl=subset(rules, subset = rhs %in% "Channel=Paid Search (Generic)" & lift > 1) 

rules.sorted = sort(rules, by="support")
#subset.matrix = is.subset(rules.sorted, rules.sorted)
#subset.matrix[lower.tri(subset.matrix, diag=T)]=0
#redundant = colSums(subset.matrix, na.rm=T) >= 1
#rules.pruned =rules.sorted[!redundant]


rules.product=subset( rules.sorted, subset = rhs %pin% "Product=" & lift > 1)
#inspect( subset( rules, subset = rhs %pin% "Product=" )) 



rules.single.df = data.frame(
  lhs = labels(lhs(rules.product)),
  rhs = labels(rhs(rules.product)), 
  rules.product@quality)

#rules.sorted.by.lhs=rules.single.df

#Search and Replace
rhs.df=as.data.frame(sub("\\{Product=","", rules.single.df$rhs))
#rhs.df=as.data.frame(gsub("\\{","",rhs.df[,1]))
rhs.df=as.data.frame(sub("\\}","",rhs.df[,1]))

lhs.df=as.data.frame(sub("\\{Matched_Country=","", rules.single.df$lhs))
#lhs.df=as.data.frame(gsub("\\{","", lhs.df[,1]))
lhs.df=as.data.frame(sub("\\}","", lhs.df[,1]))

rules.df=cbind(lhs.df,rhs.df,rules.single.df[,3],rules.single.df[,4],rules.single.df[,5])
names(rules.df)=c("Country","Product","Support","Confidence","Lift")


##Finding Rules for Regions##
##Performing a Vlookup for Country Region##
region=read.csv("Country_DMS.csv",header=TRUE)
names(region)=c("Matched_Country","Region")
region_data=merge(Data,region,by='Matched_Country')
region_data=as.data.frame(apply(region_data[-1],2,as.factor))

region_rules=apriori(region_data,parameter=list(minlen=2,maxlen=10,support=0.0001,confidence=0.0001))
region_rules.sorted = sort(region_rules, by="support")
#subset.matrix = is.subset(region_rules.sorted, region_rules.sorted)



region_rules.product=subset( region_rules.sorted, subset = rhs %pin% "Product=" & lift > 1)
rules.region.df = data.frame(
  lhs = labels(lhs(region_rules.product)),
  rhs = labels(rhs(region_rules.product)), 
  region_rules.product@quality)

#Search and Replace
region_rhs.df=as.data.frame(sub("\\{Product=","", rules.region.df$rhs))
#region_rhs.df=as.data.frame(gsub("\\{","",region_rhs.df[,1]))
region_rhs.df=as.data.frame(sub("\\}","",region_rhs.df[,1]))

region_lhs.df=as.data.frame(sub("\\{Region=","", rules.region.df$lhs))
#region_lhs.df=as.data.frame(gsub("\\{","", region_lhs.df[,1]))
region_lhs.df=as.data.frame(sub("\\}","", region_lhs.df[,1]))

region.rules.df=cbind(region_lhs.df,region_rhs.df,rules.region.df[,3],rules.region.df[,4],rules.region.df[,5])
names(region.rules.df)=c("Country","Product","Support","Confidence","Lift")


## Adding Click Out Count
rules.df$Count=as.integer(rules.df$Support*nrow(Data))
rules.df=rules.df[which(rules.df$Count>=10),]
head(rules.df)

## Single Destinations Ranking by 
ranked.df = as.data.frame(rules.df %>% 
                            arrange(Country, -Count) %>%
                            group_by(Country) %>%
                            mutate(rank=row_number()))

ranked.df=as.data.frame(ranked.df %>% 
                          group_by(Country) %>%
                          top_n(n=2,wt=-rank)
                        %>% mutate(Category="Count")
)

lift.df=as.data.frame(rules.df %>% 
                        arrange(Country, -Lift) %>%
                        group_by(Country) %>%
                        mutate(rank=row_number()))
lift.df=as.data.frame(lift.df %>% 
                        group_by(Country) %>%
                        top_n(n=5,wt=-rank)
                      %>% mutate(Category="Probability")
)

final.df=rbind(ranked.df,lift.df)
final.df=as.data.frame(final.df %>% 
                         arrange(Country)#,position)
)


#Remove Duplicate Values
unique.df=as.data.frame(final.df %>% group_by(Country) %>% filter (! duplicated(Product))) 


#Find Countries which do not have all three plans 
unique.df=as.data.frame(unique.df
                        %>% group_by(Country)
                        %>% mutate(Plan_Count=
                                     as.integer(count(as.data.frame(row_number())))))

modelling.df=as.data.frame(unique.df[which(unique.df$Plan_Count>=3),])
modelling.df=as.data.frame(modelling.df[-9])


##Adding Credebility Scores
modelling.df$Percentage=sprintf("%.1f%%", 100*modelling.df$Confidence)
modelling.df$Probability=sprintf("%.1f%%", 100*modelling.df$Lift)

modelling.df$Country_Or_Region="Country" #added

## Build Recommendations for region

#Countries that do not have rules
a=as.data.frame(setdiff(country$Country,modelling.df$Country))
names(a)="Country"

destinations_no_rules=merge(a,country,by='Country')
region_to_country_rules=merge(destinations_no_rules,a)
merge_region.rules.df=region.rules.df
names(merge_region.rules.df)=c("Region","Product","Support","Confidence","Lift")
merge_region.rules.df=merge(merge_region.rules.df,region_to_country_rules,by='Region')
merge_region.rules.df=merge_region.rules.df[-1]
names(merge_region.rules.df)=c("Product","Support","Confidence","Lift","Country")
merge_region.rules.df$Count=as.integer(merge_region.rules.df$Support*nrow(Data))

## Destination (from region) Ranking by 
region_ranked.df = as.data.frame(merge_region.rules.df %>% 
                                   arrange(Country, -Count) %>%
                                   group_by(Country) %>%
                                   mutate(rank=row_number()))

region_ranked.df=as.data.frame(region_ranked.df %>% 
                                 group_by(Country) %>%
                                 top_n(n=2,wt=-rank)
                               %>% mutate(Category="Count")
)

region_lift.df=as.data.frame(merge_region.rules.df %>% 
                               arrange(Country, -Lift) %>%
                               group_by(Country) %>%
                               mutate(rank=row_number()))
region_lift.df=as.data.frame(region_lift.df %>% 
                               group_by(Country) %>%
                               top_n(n=3,wt=-rank)
                             %>% mutate(Category="Probability")
)

region_final.df=rbind(region_ranked.df,region_lift.df)
region_final.df=as.data.frame(region_final.df %>% 
                                arrange(Country)#,position)
)


#Remove Duplicate Values
region_unique.df=as.data.frame(region_final.df %>% group_by(Country) %>% filter (! duplicated(Product))) 
region_unique.df=as.data.frame(region_unique.df[,c(5,1,2,3,4,6,7,8)])
##Adding Credebility Scores
region_unique.df$Percentage=sprintf("%.1f%%", 100*region_unique.df$Confidence)
region_unique.df$Probability=sprintf("%.1f%%", 100*region_unique.df$Lift)

region_unique.df$Country_Or_Region="Region" 

modelling_final.df=rbind(modelling.df,region_unique.df)

#Countries that do not have rules from destination as well as region
b=as.data.frame(setdiff(country$Country,modelling_final.df$Country))
b=rbind(b,b,b)
names(b)="Country"
b=b[with(b, order(Country)),]
b=as.data.frame(b)
names(b)="Country"

Most_Clicked_Out_Plans= get_ga(viewID_SG, startDate, endDate, metrics=c("ga:itemQuantity"),
                dimensions=c("ga:productName"),
                filters=c("ga:productCategoryHierarchy==travel"),fetch.by=c("month"))
names(Most_Clicked_Out_Plans)=c("Product","Count")
Most_Clicked_Out_Plans=Most_Clicked_Out_Plans[with(Most_Clicked_Out_Plans, order(-Count)),]

Most_Clicked_Out_Plans$Metric=sprintf("%.1f%%", 100*((Most_Clicked_Out_Plans$Count)/sum(Most_Clicked_Out_Plans$Count)))
Top_3=cbind(b,Most_Clicked_Out_Plans[1:3,])
Top_3$Country_Or_Region="Overall"

#first csv
modelling_final_overall.df=bind_rows(modelling_final.df,Top_3)

write.csv(modelling_final_overall.df,"final_top_plans_destinations.csv")



#b=do.call("rbind", replicate(3, a, simplify = FALSE))
#b=data.frame(b[order(b$Country),])
#b$ID=rep(seq.int(1:3),nrow(b)/3)

#generic_countries=left_join(b,topthree.df)
#generic_countries$ID=NULL
#names(generic_countries)=c("Country","Product","Count")
#country_names=data.frame(generic_countries$Country)
#names(country_names)=c("Country")



#generic.df=as.data.frame(country_names
#                        %>% arrange(Country)
#                        %>% mutate(Product=generic_countries$Product)
#                        %>% mutate(Support=0)
#                        %>% mutate(Confidence=0)
#                        %>% mutate (Lift=0)
#                        %>% mutate(Count=generic_countries$Count)
#                        %>% mutate(rank=0)
#                        %>% mutate(Category="General")
#                        %>% mutate(Plan_count=0)
#                        %>% mutate(Percentage=0)
#                        %>% mutate(Probability=0))

#names(generic.df)=names(modelling.df)
#final_modelling.df=rbind(modelling.df,generic.df)

#### Rules for regions ( Region --> Product --> Credibility) 

names(region.rules.df)=c("Region","Product","Support","Confidence","Lift")

## Adding Click Out Count
region.rules.df$Count=as.integer(region.rules.df$Support*nrow(Data))
region.rules.df=region.rules.df[which(region.rules.df$Count>=10),]

## Region Ranking by 
ranked.df = as.data.frame(region.rules.df %>% 
                            arrange(Region, -Count) %>%
                            group_by(Region) %>%
                            mutate(rank=row_number()))

ranked.df=as.data.frame(ranked.df %>% 
                          group_by(Region) %>%
                          top_n(n=2,wt=-rank)
                        %>% mutate(Category="Count")
)

lift.df=as.data.frame(region.rules.df %>% 
                        arrange(Region, -Lift) %>%
                        group_by(Region) %>%
                        mutate(rank=row_number()))
lift.df=as.data.frame(lift.df %>% 
                        group_by(Region) %>%
                        top_n(n=3,wt=-rank)
                      %>% mutate(Category="Probability")
)

final.df=rbind(ranked.df,lift.df)
final.df=as.data.frame(final.df %>% 
                         arrange(Region)#,position)
)


#Remove Duplicate Values
region_unique.df=as.data.frame(final.df %>% group_by(Region) %>% filter (! duplicated(Product))) 

##Adding Credebility Scores
region_unique.df$Percentage=sprintf("%.1f%%", 100*region_unique.df$Confidence)
region_unique.df$Probability=sprintf("%.1f%%", 100*region_unique.df$Lift)

region_unique.df$Country_Or_Region="Region" 


#Regions that do not have rules
b=as.data.frame(setdiff(country$Region,region_unique.df$Region))
b=rbind(b,b,b)
names(b)="Region"
b=b[with(b, order(Region)),]
b=as.data.frame(b)
names(b)="Region"

Top_3=cbind(b,Most_Clicked_Out_Plans[1:3,])
Top_3$Country_Or_Region="Overall"

#second csv
region_modelling_final_overall.df=bind_rows(region_unique.df,Top_3)

write.csv(region_modelling_final_overall.df,"final_top_plans_region.csv")

#------------------------------------------------------------------------------------------------------------------------------



