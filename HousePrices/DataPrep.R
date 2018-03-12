setwd("C:/Users/taha/Documents/GitHub/Competition/HousePrices")

#----------------

train <- read.table("train.csv", 
                    stringsAsFactors = TRUE, sep = ",", header = TRUE)
train$LandSlope<-factor(train$LandSlope, 
                        levels=c("Sev","Mod", "Gtl"), 
                        ordered = TRUE)
train$OverallQual<-factor(train$OverallQual, 
                          levels = 1:10, ordered = TRUE)
train$OverallCond<-factor(train$OverallCond, 
                          levels = 1:10, ordered = TRUE)
train$ExterQual<-factor(train$ExterQual, 
                        levels = c("Po", "Fa", "TA","Gd","Ex"), 
                        ordered = TRUE)
train$ExterCond<-factor(train$ExterCond, 
                        levels = c("Po", "Fa", "TA","Gd","Ex"), 
                        ordered = TRUE)
train$BsmtCond<-factor(train$BsmtCond, 
                       levels = c("Po", "Fa", "TA","Gd","Ex"), 
                       ordered = TRUE)
train$BsmtQual<-factor(train$BsmtQual, 
                       levels = c("Po", "Fa", "TA","Gd","Ex"), 
                       ordered = TRUE)
train$BsmtExposure<-factor(train$BsmtExposure, 
                           levels = c("No", "Mn", "Av","Gd"), 
                           ordered = TRUE)
train$BsmtFinType1<-factor(train$BsmtFinType1, 
                           levels = c("Unf","LwQ","Rec", "BLQ", "ALQ","GLQ"), 
                           ordered = TRUE)
train$BsmtFinType2<-factor(train$BsmtFinType2, 
                           levels = c("Unf","LwQ","Rec", "BLQ", "ALQ","GLQ"), 
                           ordered = TRUE)
train$HeatingQC<-factor(train$HeatingQC, 
                        levels = c("Po", "Fa", "TA","Gd","Ex"), 
                        ordered = TRUE)
train$CentralAir<-ifelse(train$CentralAir=="Y", TRUE, 
                         ifelse(train$CentralAir =="N", FALSE, NA))
train$KitchenQual<-factor(train$KitchenQual, 
                          levels = c("Po", "Fa", "TA","Gd","Ex"), 
                          ordered = TRUE)
train$Functional<-factor(train$Functional, 
                         levels = c("Sal", "Sev", "Maj2","Maj1","Mod", "Min2",
                                    "Min1","Typ"), 
                         ordered = TRUE)
train$FireplaceQu<-factor(train$FireplaceQu, 
                          levels = c("Po", "Fa", "TA","Gd","Ex"), 
                          ordered = TRUE)
train$GarageFinish<-factor(train$GarageFinish, 
                           levels = c("Unf","RFn","Fin"),
                           ordered= TRUE)
train$GarageQual<-factor(train$GarageQual, 
                         levels = c("Po", "Fa", "TA","Gd","Ex"), 
                         ordered = TRUE)
train$GarageCond<-factor(train$GarageCond, 
                         levels = c("Po", "Fa", "TA","Gd","Ex"), 
                         ordered = TRUE)
train$PavedDrive<-factor(train$PavedDrive, 
                         levels = c("N","P","Y"), 
                         ordered= TRUE)
train$PoolQC<-factor(train$PoolQC, 
                     levels = c("Po", "Fa", "TA","Gd","Ex"), 
                     ordered = TRUE)
train$LotShape<-factor(train$LotShape, 
                       levels = c("IR3", "IR2", "IR1","Reg"), 
                       ordered = TRUE)
train$Alley<-factor(train$Alley, 
                    levels = c("Grvl", "Pave"), 
                    ordered = TRUE)
train$Street<-factor(train$Street, 
                     levels = c("Grvl", "Pave"), 
                     ordered = TRUE)
train$YearBuilt<-factor(train$YearBuilt, levels = sort(unique(train$YearBuilt)),
                        ordered=TRUE)
train$YearRemodAdd<-factor(train$YearRemodAdd, levels = sort(unique(train$YearRemodAdd)),
                        ordered=TRUE)
train$YrSold<-factor(train$YrSold, levels = sort(unique(train$YrSold)),
                           ordered=TRUE)
train$MoSold<-factor(train$MoSold, levels = sort(unique(train$MoSold)),
                     ordered=TRUE)
train$GarageYrBlt<-factor(train$GarageYrBlt, levels = sort(unique(train$GarageYrBlt)),
                     ordered=TRUE)


##---------------------------------------------------------------
  

test <- read.table("test.csv", 
                   stringsAsFactors = TRUE, sep = ",", header = TRUE)
test$LandSlope<-factor(test$LandSlope, 
                       levels=c("Sev","Mod", "Gtl"), 
                       ordered = TRUE)
test$OverallQual<-factor(test$OverallQual, 
                         levels = 1:10, ordered = TRUE)
test$OverallCond<-factor(test$OverallCond, 
                         levels = 1:10, ordered = TRUE)
test$ExterQual<-factor(test$ExterQual, 
                       levels = c("Po", "Fa", "TA","Gd","Ex"), 
                       ordered = TRUE)
test$ExterCond<-factor(test$ExterCond, 
                       levels = c("Po", "Fa", "TA","Gd","Ex"), 
                       ordered = TRUE)
test$BsmtCond<-factor(test$BsmtCond, 
                      levels = c("Po", "Fa", "TA","Gd","Ex"), 
                      ordered = TRUE)
test$BsmtQual<-factor(test$BsmtQual, 
                      levels = c("Po", "Fa", "TA","Gd","Ex"), 
                      ordered = TRUE)
test$BsmtExposure<-factor(test$BsmtExposure, 
                          levels = c("No", "Mn", "Av","Gd"), 
                          ordered = TRUE)
test$BsmtFinType1<-factor(test$BsmtFinType1, 
                          levels = c("Unf","LwQ","Rec", "BLQ", "ALQ","GLQ"), 
                          ordered = TRUE)
test$BsmtFinType2<-factor(test$BsmtFinType2, 
                          levels = c("Unf","LwQ","Rec", "BLQ", "ALQ","GLQ"), 
                          ordered = TRUE)
test$HeatingQC<-factor(test$HeatingQC, 
                       levels = c("Po", "Fa", "TA","Gd","Ex"), 
                       ordered = TRUE)
test$CentralAir<-ifelse(test$CentralAir=="Y", TRUE, 
                        ifelse(test$CentralAir =="N", FALSE, NA))
test$KitchenQual<-factor(test$KitchenQual, 
                         levels = c("Po", "Fa", "TA","Gd","Ex"), 
                         ordered = TRUE)
test$Functional<-factor(test$Functional, 
                        levels = c("Sal", "Sev", "Maj2","Maj1","Mod", "Min2",
                                   "Min1","Typ"), 
                        ordered = TRUE)
test$FireplaceQu<-factor(test$FireplaceQu, 
                         levels = c("Po", "Fa", "TA","Gd","Ex"), 
                         ordered = TRUE)
test$GarageFinish<-factor(test$GarageFinish, 
                          levels = c("Unf","RFn","Fin"),
                          ordered= TRUE)
test$GarageQual<-factor(test$GarageQual, 
                        levels = c("Po", "Fa", "TA","Gd","Ex"), 
                        ordered = TRUE)
test$GarageCond<-factor(test$GarageCond, 
                        levels = c("Po", "Fa", "TA","Gd","Ex"), 
                        ordered = TRUE)
test$PavedDrive<-factor(test$PavedDrive, 
                        levels = c("N","P","Y"), 
                        ordered= TRUE)
test$PoolQC<-factor(test$PoolQC, 
                    levels = c("Po", "Fa", "TA","Gd","Ex"), 
                    ordered = TRUE)
test$LotShape<-factor(test$LotShape, 
                      levels = c("IR3", "IR2", "IR1","Reg"), 
                      ordered = TRUE)
test$Alley<-factor(test$Alley, 
                   levels = c("Grvl", "Pave"), 
                   ordered = TRUE)
test$Street<-factor(test$Street, 
                    levels = c("Grvl", "Pave"), 
                    ordered = TRUE)
test$YearBuilt<-factor(test$YearBuilt, levels = sort(unique(test$YearBuilt)),
                       ordered=TRUE)
test$YearRemodAdd<-factor(test$YearRemodAdd, levels = sort(unique(test$YearRemodAdd)),
                          ordered=TRUE)
test$YrSold<-factor(test$YrSold, levels = sort(unique(test$YrSold)),
                    ordered=TRUE)
test$MoSold<-factor(test$MoSold, levels = sort(unique(test$MoSold)),
                    ordered=TRUE)
test$GarageYrBlt<-factor(test$GarageYrBlt, levels = sort(unique(test$GarageYrBlt)),
                         ordered=TRUE)

