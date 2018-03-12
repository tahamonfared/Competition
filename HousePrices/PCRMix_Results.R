##Solve through PCAmixdata
#install.packages("PCAmixdata")
require(PCAmixdata)

str(train)
find_quanti<-function(col){
  if(is.integer(train[,col])){
    return(col)
  }
  else{
    return(NULL)
  }
  
}

X_quanti<-unlist(sapply(1:ncol(train), find_quanti))
X_quali<-setdiff(1:ncol(train), c(10,X_quanti))
X_quanti<-X_quanti[-c(1,length(X_quanti))]

pca_res<-PCAmix(X.quanti = train[,X_quanti], X.quali = train[,X_quali], ndim = 5,rename.level = TRUE,graph = TRUE)
pca_res<-PCArot(pca_res, 5)


PCR_res<-lm(sqrt(train$SalePrice[-c(524,1299)])~(pca_res$ind$coord[-c(524,1299),]))
#plot(PCR_res)

pcamix_ontest<-predict(pca_res, X.quanti = test[,X_quanti], X.quali = test[,X_quali], test)
pcamix_ontest<-data.frame(pcamix_ontest)

regres<-function(arr){
  PCR_res$coefficients[1]+PCR_res$coefficients[2]*arr[1]+
  PCR_res$coefficients[3]*arr[2]+PCR_res$coefficients[4]*arr[3]+
  PCR_res$coefficients[5]*arr[4]+PCR_res$coefficients[6]*arr[5]
}


prediction_pcamixmodel<-apply(pcamix_ontest, 1, regres)

pcamixmodel_results<-cbind(test[1], SalePrice = prediction_pcamixmodel^2)
write.csv(x = pcamixmodel_results,file = "PCRMixedModel_Results.csv",row.names = FALSE)

