#Loading necessary Libraries
library(sqldf)
library(readr)
library(readxl)

#Importing Dataset
sdata1 <- read_csv("C:/Users/Sindhu/Downloads/train_ver2_1/train_ver2_0.csv")

View(sdata1)

#Creating COlumns to Data Frame
sdata_col <- as.data.frame(colnames(sdata1))

#Importing Column_dataset
column_names <- read_excel("C:/Users/Sindhu/Downloads/santander_column_desc.xlsx")

#Changing column names using the column_dataset
colnames(sdata1) <- t(column_names[,3]) 

colnames(sdata1) #Viewing Column_Names

#Writing the new data to a csv file
write.csv(sdata1, file = "C:/Users/Sindhu/Downloads/train_ver2_1/train_ver2_1.csv")

#Loading the new dataset
sdata2 <- read.csv("C:/Users/Sindhu/Downloads/train_ver2_1/train_ver2_1.csv")

#Counting data from date1 and grouping by 1 and ordering in ascending order 
sqldf("select date1, count (*) from sdata2 group by 1 order by 1")

sdata2 <- sdata2[,-1]

#Customers from Aug 2015 - Sample of 300000
customers <- sqldf("select distinct cust_code from sdata2
                   where date1 = '2015-08-28' limit 300000")

customers #Viewing Customers

#Creating Month1 as a variable from date1
set1 <- sqldf("select *, cast(substr(date1,6,2) as int) as month1 from sdata2
               where cust_code in (select cust_code from customers)")

set1 #Viewing data from all months

#Writing the new data to a csv file
write.csv(set1, file = "C:/Users/Sindhu/Downloads/train_ver2_1/set1.csv", row.names = F)

#Loading the new dataset
set1 <- read.csv("C:/Users/Sindhu/Downloads/train_ver2_1/set1.csv")

#Saving set1 as RDS file
saveRDS(set1, "set1.rds")

set1_RDS <- as.matrix(readRDS("set1.rds")) #Creating a Matix of set1 RDS file

############################ CODE 2 #########################################

library(stringr)

#Selecting data from set 1 - Top 5 Records
sqldf("select * from set1 limit 5")

#Counting data from Month, credit cards and ordering in ascending order of months
sqldf("select month1, credit_cards, count(*) from set1
      group by 1,2 order by 1")

#Creating Labels using customer Data and Credit cards using Join function
labels <- sqldf("select a.cust_code, case when a.credit_cards>b.credit_cards then 1 else 0 end new_card
                from (select cust_code, credit_cards from set1 where month1 = 8) a
                join (select cust_code, credit_cards
                from set1 where month1 = 7 group by 1) b on a.cust_code = b.cust_code")

#Creating features based on month1 = 7
set1 <- set1[, -c(1,7,11)]
set1[, c(5,7)] <- sapply(set1[, c(5,7)], as.numeric)
set1[,9] <- as.factor(gsub('.0', '', set1[,9]))

#Creating columns with type factor from set1
names <- colnames(set1)
factors <- sapply(set1, is.factor)
factors
colnames <- names[factors]
colnames

#Creating level data 
levels_1 <- levels(set1[, "emp_code"])
level_1 <- sapply(set1[,colnames], levels)

level_1

#Creating Flags using the level_1 data
flags <- list()
for(i in 1:length(colnames)){
  flags[[i]] <- paste0("case when ",colnames[i], "='", level_1[[colnames[i]]],"' then 1 else 0 end ",
                       colnames[i],substr(gsub('[ ,-]+','_', level_1[[colnames[i]]], perl = T),1,6), collapse = ",")
}

flags #Viewing Flags

level_1$segment

case1 <- str_c(flags, collapse = ',')
case1
nums <- sapply(set1, is.numeric) #Checking all the columns with numeric data
nums
colnames2 <- names[nums][-34] #Fetching all columns with numbers data 
colnames2

#Combining Labels 
set2 <- sqldf(paste0("select a.*, b.new_card
                     from (select ",paste(colnames2[], collapse = ","),", 
                     ",case1," from set1 where month1 = 7) a
                     join labels b on a.cust_code = b.cust_code"))

#Writing the new data to a csv file
write.csv(set2, file = "C:/Users/Sindhu/Downloads/train_ver2_1/set2.csv", row.names = F)
write.csv(set2, file = "C:/Users/Sindhu/Downloads/train_ver2_1/set2_1.csv", row.names = F)

#Saving set2 as RDS file
saveRDS(set2, "set2.rds")

#Creating a Matix of set1 RDS file
set2_RDS <- as.matrix(readRDS("set2.rds")) 

#### XGBOOST Modeling ####
library(xgboost)

#Creating the model using xgboost
m1 <- xgboost(data = as.matrix(set2[,2:328]), label = as.matrix(set2[,329]),
              max_depth = 2, eta = 0.5, nrounds = 7, objective = "binary:logistic", verbose = 2)

library(DiagrammeR)

xgb.plot.tree(model=m1) # plot tree
xgb.plot.multi.trees(model = m1) # plot collective tree

pred1 <- predict(m1, as.matrix(set2[,-c(1,329)]))

set3 <- as.data.frame(cbind(pred1, set2[,329]))

sqldf("select round(pred1,2), sum(v2)/count(*), count(*) from set3 group by 1 order by 1")

print(xgb.importance(model = m1)) # variable importance

print(round(xgb.importance(model = m1)[,2:4],2)) # rounding off values

f1<-xgb.importance(model = m1)

print(cbind(f1[,1],round(f1[,2:4],2)))

#####END######