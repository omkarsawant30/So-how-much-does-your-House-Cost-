##First we need to set the directory of the file's location in your computer and then read it so that R can download the data
setwd("E:/DATA ANALYTICS JOURNEY/R Edvancer/PROJECT 1 REAL ESTATE")
h_train=read.csv("housing_train.csv",stringsAsFactors = F)
h_test=read.csv("housing_test.csv",stringsAsFactors = F)

##You will need same set of vars on both train and test,its easier to manage that if you combine train and test
##in the beginning and then separate them once you are done with data preparation
##We'll fill test's response column with NAs.
h_test$Price=NA

h_train$data='train'
h_test$data='test'
h_all=rbind(h_train,h_test)
library(dplyr)
glimpse(h_all)

head(h_all)

##Next we'll create dummy variables for remaining categorical variables first by creating 
##a new function CreateDummies and then using sapply for creating dummies
CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    2
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

char_logical=sapply(h_all,is.character)
cat_cols=names(h_all)[char_logical]
cat_cols

cat_cols=cat_cols[!(cat_cols %in% c('data','Price'))]
cat_cols

# we are using frequency cutoff as 50, there is no magic number here,
# lower cutoffs will simply result in more number of dummy variables
for(col in cat_cols){
  h_all=CreateDummies(h_all,col,50)
}

glimpse(h_all)

##we can go ahead and separate training and test data BUT first we check NA values
h_all=h_all[!(is.na(h_all$Price) & h_all$data=="train"),]

for(col in names(h_all)){
  if(sum(is.na(h_all[,col]))>0 & !(col %in% c("data","Price"))){
    h_all[is.na(h_all[,col]),col]=mean(h_all[h_all$data=='train',col],na.rm=T)
  }
}

##Lets separate our two data sets and remove the unnecessary columns
## that we added while combining them
h_train=h_all %>% filter(data=='train') %>% select(-data)
h_test=h_all %>% filter(data=='test') %>% select(-data,-Price)

any(is.na(h_train))
any(is.na(h_test))

#Using Random Forest model
library(randomForest)
fit = randomForest(Price~., data = h_train)

### Make predictions on test and submit 
test.predictions = predict(fit, newdata = h_test)
write.csv(test.predictions,file = "Omkar_Sawant_P1_part2.csv", row.names = F)
