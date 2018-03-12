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

train$YearBuilt<-as.numeric(2018-train$YearBuilt)
train$YearRemodAdd<-as.numeric(2018-train$YearRemodAdd)
train$MoSold<-factor(train$MoSold)
train$GarageYrBlt<-as.numeric(2018-train$GarageYrBlt)

train$PoolArea[train$PoolArea==0]<-NA
train$PoolQC[train$PoolQC==0]<-NA
train$MiscVal[train$MiscVal==0]<-NA
train$ScreenPorch[train$ScreenPorch==0]<-NA
train$X3SsnPorch[train$X3SsnPorch==0]<-NA
train$EnclosedPorch[train$EnclosedPorch==0]<-NA
train$OpenPorchSF[train$OpenPorchSF==0]<-NA
train$WoodDeckSF[train$WoodDeckSF==0]<-NA
train$GarageArea[train$GarageArea==0]<-NA
train$GarageCars[train$GarageCars==0]<-NA
train$LowQualFinSF[train$LowQualFinSF==0]<-NA
train$X2ndFlrSF[train$X2ndFlrSF==0]<-NA
train$BsmtFinSF2[train$BsmtFinSF2==0]<-NA
train$BsmtFinSF1[train$BsmtFinSF1==0]<-NA
train$MasVnrArea[train$MasVnrArea==0]<-NA

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
test$YearBuilt<-as.numeric(2018-test$YearBuilt)
test$YearRemodAdd<-as.numeric(2018-test$YearRemodAdd)
test$MoSold<-factor(test$MoSold)
test$GarageYrBlt<-as.numeric(2018-test$GarageYrBlt)
test$PoolArea[test$PoolArea==0]<-NA
test$PoolQC[test$PoolQC==0]<-NA
test$MiscVal[test$MiscVal==0]<-NA
test$ScreenPorch[test$ScreenPorch==0]<-NA
test$X3SsnPorch[test$X3SsnPorch==0]<-NA
test$EnclosedPorch[test$EnclosedPorch==0]<-NA
test$OpenPorchSF[test$OpenPorchSF==0]<-NA
test$WoodDeckSF[test$WoodDeckSF==0]<-NA
test$GarageArea[test$GarageArea==0]<-NA
test$GarageCars[test$GarageCars==0]<-NA
test$LowQualFinSF[test$LowQualFinSF==0]<-NA
test$X2ndFlrSF[test$X2ndFlrSF==0]<-NA
test$BsmtFinSF2[test$BsmtFinSF2==0]<-NA
test$BsmtFinSF1[test$BsmtFinSF1==0]<-NA
test$MasVnrArea[test$MasVnrArea==0]<-NA
