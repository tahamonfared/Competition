
train <- read.table("~/Dropbox (CSU Fullerton)/University/Kaggle/House Prices/train.csv", 
                            stringsAsFactors = TRUE, sep = ",", header = TRUE)
train$LandSlope<-factor(train$LandSlope, 
                                levels=c("Sev","Mod", "Gtl"), ordered = TRUE)
train$OverallQual<-factor(train$OverallQual, 
                                  levels = 1:10, ordered = TRUE)
train$OverallCond<-factor(train$OverallCond, 
                                  levels = 1:10, ordered = TRUE)
train$ExterQual<-factor(train$ExterQual, 
                                levels = c("Po", "Fa", "TA","Gd","Ex"), ordered = TRUE)
train$ExterCond<-factor(train$ExterCond, 
                                levels = c("Po", "Fa", "TA","Gd","Ex"), ordered = TRUE)
train$BsmtCond<-factor(train$BsmtCond, 
                               levels = c("Po", "Fa", "TA","Gd","Ex"), ordered = TRUE)
train$BsmtQual<-factor(train$BsmtQual, 
                               levels = c("Po", "Fa", "TA","Gd","Ex"), ordered = TRUE)
train$BsmtExposure<-factor(train$BsmtExposure, 
                                   levels = c("No", "Mn", "Av","Gd"), ordered = TRUE)
train$BsmtFinType1<-factor(train$BsmtFinType1, 
                                   levels = c("Unf","LwQ","Rec", "BLQ", "ALQ","GLQ"), ordered = TRUE)
train$BsmtFinType2<-factor(train$BsmtFinType2, 
                                   levels = c("Unf","LwQ","Rec", "BLQ", "ALQ","GLQ"), ordered = TRUE)
train$HeatingQC<-factor(train$HeatingQC, 
                                levels = c("Po", "Fa", "TA","Gd","Ex"), ordered = TRUE)

train$CentralAir<-ifelse(train$CentralAir=="Y", TRUE, ifelse(train$CentralAir =="N", FALSE, NA))

train$KitchenQual<-factor(train$KitchenQual, 
                                  levels = c("Po", "Fa", "TA","Gd","Ex"), ordered = TRUE)
train$Functional<-factor(train$Functional, 
                                 levels = c("Sal", "Sev", "Maj2","Maj1","Mod", "Min2","Min1","Typ"), ordered = TRUE)
train$FireplaceQu<-factor(train$FireplaceQu, 
                                  levels = c("Po", "Fa", "TA","Gd","Ex"), ordered = TRUE)
train$GarageFinish<-factor(train$GarageFinish, 
                                   levels = c("Unf","RFn","Fin"), ordered= TRUE)
train$GarageQual<-factor(train$GarageQual, 
                                 levels = c("Po", "Fa", "TA","Gd","Ex"), ordered = TRUE)
train$GarageCond<-factor(train$GarageCond, 
                                 levels = c("Po", "Fa", "TA","Gd","Ex"), ordered = TRUE)
train$PavedDrive<-factor(train$PavedDrive, 
                                 levels = c("N","P","Y"), ordered= TRUE)
train$PoolQC<-factor(train$PoolQC, 
                             levels = c("Po", "Fa", "TA","Gd","Ex"), ordered = TRUE)
train$LotShape<-factor(train$LotShape, 
                             levels = c("IR3", "IR2", "IR1","Reg"), ordered = TRUE)
train$Alley<-factor(train$Alley, 
                               levels = c("Grvl", "Pave"), ordered = TRUE)
train$Street<-factor(train$Street, 
                               levels = c("Grvl", "Pave"), ordered = TRUE)

summary(train)


install.packages("PCAmixdata")
require(PCAmixdata)
