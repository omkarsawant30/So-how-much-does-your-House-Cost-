#First we need to set the directory of the file's location in your computer and then read it so that R can download the data
setwd("E:/DATA ANALYTICS JOURNEY/R Edvancer/PROJECT 1 REAL ESTATE")
re_train=read.csv("housing_train.csv",stringsAsFactors = F)
re_test= read.csv("housing_test.csv",stringsAsFactors = F)

##You will need same set of vars on both train and test,
##its easier to manage that if you combine train and test
##in the beginning and then separate them once you are done with data preparation
##We'll fill test's response column with NAs.
re_test$Price=NA
re_train$data='train'
re_test$data='test'
re_all=rbind(re_train,re_test)

library(dplyr)
glimpse(re_all)

##Next we'll create dummy variables for remaining categorical variables
##using sapply for creating dummies
char_logical=sapply(re_all,is.character)
cat_cols=names(re_all)[char_logical]
cat_cols

cat_cols=cat_cols[!(cat_cols %in% c('data','Price'))]
cat_cols

# we are using frequency cutoff as 50, there is no magic number here,
# lower cutoffs will simply result in more number of dummy variables
for(col in cat_cols){
  re_all=CreateDummies(re_all,col,50)
}

glimpse(re_all)

##we can go ahead and separate training and test data BUT first we check NA values
re_all=re_all[!((is.na(re_all$Price)) & re_all$data=='train'), ]

for(col in names(re_all)){
  if(sum(is.na(re_all[,col]))>0 & !(col %in% c("data","Price"))){
    re_all[is.na(re_all[,col]),col]=mean(re_all[re_all$data=='train',col],na.rm=T)
  }
}

##Lets separate our two data sets and remove the unnecessary columns
## that we added while combining them.
re_train=re_all %>% filter(data=='train') %>% select(-data)
re_test=re_all %>% filter(data=='test') %>% select(-data,-Price)

##Lets build a model on training data by checking VIF values as we need to remove multicollinearity
fit=lm(Price~.,data=re_train)
library(car)
sort(vif(fit),decreasing = T)[1:3]

fit=lm(Price~.-CouncilArea_,data=re_train)
sort(vif(fit),decreasing = T)[1:3]

fit=lm(Price~.-CouncilArea_-Postcode,data=re_train)
sort(vif(fit),decreasing = T)[1:3]

fit=lm(Price~.-CouncilArea_-Postcode-Distance,data=re_train)
sort(vif(fit),decreasing = T)[1:3]

rm(fit)
fit=lm(Price~.,data=re_train)
fit=step(fit)

summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale ,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond
       -Method_SP,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond
       -Method_SP-SellerG_Rendina,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond
       -Method_SP-SellerG_Rendina-SellerG_Raine,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond
       -Method_SP-SellerG_Rendina-SellerG_Raine-SellerG_Love,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond
       -Method_SP-SellerG_Rendina-SellerG_Raine-SellerG_Love-SellerG_Douglas,data=re_train)
summary(fit)
fit=lm(Price~.-CouncilArea_-Postcode-Distance-Suburb_NorthMelbourne
       -Suburb_Abbotsford-Suburb_Murrumbeena-Suburb_SouthMelbourne
       -Suburb_Ashburton-Suburb_BrunswickEast-Suburb_Niddrie
       -Suburb_FitzroyNorth-Suburb_Ormond-Suburb_Strathmore
       -Suburb_WestFootscray-Suburb_Burwood-Suburb_Melbourne
       -Suburb_BrunswickWest-Suburb_SurreyHills-Suburb_Elwood
       -Suburb_Newport-Suburb_Doncaster-Suburb_AscotVale
       -Suburb_Footscray-Suburb_MooneePonds-Suburb_Thornbury
       -Suburb_Yarraville-Suburb_Carnegie-Suburb_PortMelbourne
       -Suburb_Bentleigh-Suburb_Brunswick-Suburb_StKilda-Suburb_Richmond
       -Method_SP-SellerG_Rendina-SellerG_Raine-SellerG_Love-SellerG_Douglas
       -SellerG_Williams-SellerG_Village-SellerG_Stockdale-SellerG_Hodges
       -SellerG_McGrath-SellerG_Noel-SellerG_Gary-SellerG_Jas-SellerG_Fletchers
       -SellerG_Woodards-SellerG_Brad-SellerG_Biggin-SellerG_Ray-SellerG_Buxton
       -SellerG_Barry-SellerG_hockingstuart-SellerG_Nelson-CouncilArea_Monash
       -CouncilArea_Manningham-CouncilArea_Stonnington-CouncilArea_Darebin,data=re_train)
summary(fit)

#After removing VIF values > 10 we now make a prediction on our test data based on our Linear Regression model that we built
test.predictions=predict(fit,newdata=re_test)
write.csv(test.predictions,'Futureprices.csv',row.names = F)

#-QUIZ------------------------------------------------------------------------------------------------------------
#1st question - Find the variance of the target variable 'Price'
var(re_train$Price)

##2nd question - Find out how many observations have missing values for variable 'YearBuilt'?
library(dplyr)
sum(is.na(re_train$YearBuilt))

##3rd question - What is the difference in average price between house type h and t?
mean(re_train$Type_h)-mean(re_train$Type_t)

##4th - How many unique values variable postcode takes?
unique(re_train$Postcode)

#5th - how should you treat post code . As a categorical variable or numeric variable ? 
#Ans - numeric

#6th - Does distance follow a normal distribution?
#Ans - No
library(dplyr)
library(ggplot2)
ggplot(re_train,aes(x=Distance))+geom_histogram()

#7th - Which seller has maximum value transactions? ( Sum of Price)
#Ans - Jellis

#8th - Which CouncilArea has maximum average price?
#Ans - Bayside

#9th - which CouncilArea has maximum variance in the price?
#Ans - Stonnington

#10th - Should we use Address as is in the modeling process?
#Ans  - No, we can break it down

#-------------------------------------------------------------------------------------------------------------------

