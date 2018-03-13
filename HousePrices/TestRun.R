##Solve through PCAmixdata
#install.packages("PCAmixdata")

test_rand<-sample(1:nrow(train), size=200)
test_ft <-train[test_rand,]
train_ft<-train[-test_rand,]

data<-train
i<-5
require(PCAmixdata)

explore_attributes<-function(data){
  res<-list()
  N<-nrow(data)
  for(i in 1:ncol(data)){
    if(is.numeric(data[,i])){
      res[[i]]<-list(i,names(data)[i],TRUE,sum(is.na(data[,i]))/N,
                    NA, NA)
    }else{
      x<-summary(data[,i])
      res[[i]]<-list(i,names(data)[i], FALSE,
                     if(names(x)[x==max(x)]=="NA's"){max(x)/N}else{NA},
                     names(x)[x==max(x)],
                     if(names(x)[x==max(x)]=="NA's"){
                       NA
                       }else{
                       if(!is.logical(data[,i])){
                         max(x)/N
                         }else{
                           max(sum(data[,i]), N-sum(data[,i]))/N
                           }
                       })
    }
  }
  res<-data.frame(matrix(unlist(res),nrow = ncol(data),byrow=TRUE))
  names(res)<-c("Column_Number","Column_name=","IsNumeric","Percent_NA","Cat_level_highest","Percent_InCat")
  return(res)
}

usability<-explore_attributes(train)


X_quanti<-unlist(sapply(setdiff(1:ncol(train),nonusableatts), find_quanti))
X_quali<-setdiff(setdiff(1:ncol(train),nonusableatts), c(10,X_quanti))
X_quanti<-X_quanti[-c(1,length(X_quanti))]

pca_res<-PCAmix(X.quanti = train_ft[,X_quanti], X.quali = train_ft[,X_quali], 
                ndim = 5,rename.level = TRUE,graph = TRUE)
pca_res<-PCArot(pca_res, 5)

PCR_res<-lm(train_ft$SalePrice[-c(524,1299)]~(pca_res$ind$coord[-c(524,1299),]))

pcamix_ontest<-predict(pca_res, X.quanti = test_ft[,X_quanti], X.quali = test_ft[,X_quali])
pcamix_ontest<-data.frame(pcamix_ontest)

regres<-function(arr){
  PCR_res$coefficients[1]+PCR_res$coefficients[2]*arr[1]+
    PCR_res$coefficients[3]*arr[2]+PCR_res$coefficients[4]*arr[3]+
    PCR_res$coefficients[5]*arr[4]+PCR_res$coefficients[6]*arr[5]
}


prediction_pcamixmodel<-apply(pcamix_ontest, 1, regres)


x<-cbind(test_ft[,c(1,81)], prediction_pcamixmodel)
plot(x[,1],x[,2],pch=20, col = 'blue')
points(x[,1],x[,3], pch=19,col='red')
require(scales)
abline(v = x[,1],col = alpha("grey",.4))


