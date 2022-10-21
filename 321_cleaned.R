df<-read.csv("house-data.csv")
sum(is.na(df))
sapply(df, function(x) sum(is.na(x)))
sapply (df, function(x) (sum(is.na(x))/nrow(df))*100)
drop<-c("Alley","PoolQC","Fence","MiscFeature","SaleType","ExterCond","BsmtQual","BsmtCond","GarageType",
        "GarageCond","LotConfig","Neighborhood","Condition1","Condition2","BldgType","HouseStyle",
        "RoofStyle","RoofMatl","Exterior1st","Foundation","Heating","Functional","PoolQC","Fence","MiscFeature","SaleCondition")
unique(df$Neighborhood)
df = df[,!(names(df) %in% drop)]

dim(df)
names(df)
str(df)
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}
unique(df$PavedDrive)
unique(df$OverallCond)


str(df)
unique(df$Street)
unique(df$Utilities)
unique(df$ExterQual)
unique(df$KitchenQual)
unique(df$PavedDrive)
# Converting Five character variables into dummy.
df$Street<- ifelse(df$Street == "Pave", 0 ,1)
df$Utilities <- ifelse(df$Utilities == "AllPub", 0 ,1)
df$ExterQual <- ifelse(df$ExterQual == "Gd", 0, ifelse(df$ExterQual=="TA",1, ifelse(df$ExterQual=="Ex",2, 3)))
df$KitchenQual <- ifelse(df$KitchenQual == "Gd", 0, ifelse(df$KitchenQual=="TA",1, ifelse(df$KitchenQual=="Ex",2, 3)))
df$PavedDrive <- ifelse(df$PavedDrive == "Y", 0,ifelse(df$PavedDrive== "N", 1, 2))
view(df)
#Correlation Matrix
install.packages("corrplot")
library(corrplot)
ColMat3 <- cor(df)
print(ColMat3)
ColMat3.corr <- cor(df)
# Heatmap of Correlation matrix
corrplot(ColMat3.corr)



class(df$OverallCond)
df$OverallCond<-as.factor(df$OverallCond)
#Q.2a)
df$OverallCond<- ifelse(df$OverallCond <= 3, "Poor", ifelse(df$OverallCond <= 6, "Average","Good"))
df$OverallCond<- ifelse(df$OverallCond == "Poor", 0 , ifelse(df$OverallCond == "Average", 1, 2))
dflog1<- sort(sample(nrow(df), nrow(df)*.7))
train <- df[dflog1,]
test <- df[-dflog1,]
names(df)
str(df)
is.na(df$OverallCond)
df$OverallCond <- as.numeric(overallcond)
dflog <- glm(OverallCond ~ YearBuilt + TotalBsmtSF + X1stFlrSF + GrLivArea + FullBath + TotRmsAbvGrd + GarageArea + Utilities + OverallQual + ExterQual, data = train, family=binomial(link=logit))
summary(dflog)
dflog$y
saaa <- ifelse(dflog>0.5, 1, 0)
bbb <- mean(aaa != test$OverallCond)
print(paste('Accuracy', 1- bbb))
class(df$OverallCond)

#Q.2b
install.packages("MASS")
library(MASS)
library(dplyr)
lll <- lda(OverallCond ~ ., data = train)

kkk <- lll %>% predict(test)


mean(kkk$class==test$OverallCond)

as.factor("dflog")
class("dflog")
df$OverallCond
df$OverallCond <- as.factor(df$OverallCond)
class(df$OverallCond)

predicted=predict(dflog,test,type = "response")
summary(dflog)
predicted

sensitivity<-sensitivity(df,)
specificity(test$OverallCond,predicted,threshold=optCutOff)
?specificity
class(df$OverallCond)

# Converting Coefficient/ Response Variables/ X  to Exponential
exp(coef(dflog))

#
head(round(fitted(dflog), 2))

dflog_predicted <- predict(dflog, newdata = train, "link")
tab <- table(train$OverallCond, dflog_predicted)
round((sum(diag(tab))/sum(tab))*100,2)


#Q.3)a)

library(corrplot)

# Removing Correlation matrix
ColMat3 <- cor(df)
print(ColMat3)
ColMat3.corr <- cor(df)
# Heatmap of Correlation matrix to Remove Some Variables from Model
corrplot(ColMat3.corr)

# Method 1

# Linear Model

linear_model <- lm(df$SalePrice ~ df$OverallQual + df$YearBuilt + df$MasVnrArea + df$TotalBsmtSF + df$X1stFlrSF + df$GrLivArea + df$FullBath + df$TotRmsAbvGrd + df$Fireplaces + df$GarageArea)
summary(linear_model)

# Method 2

# Random Forest
set.seed(50)
ran_forest <- trainControl(method = "cv", number = 10, verboseIter = FALSE)
model_rd <- train(train$SalePrice ~ train$OverallQual + train$YearBuilt + train$MasVnrArea + train$TotalBsmtSF + train$X1stFlrSF + train$GrLivArea + train$FullBath + train$TotRmsAbvGrd + train$Fireplaces + train$GarageArea, data= train, tuneLength = 1, method= "ranger", importance = 'impurity', trControl = ran_forest)




