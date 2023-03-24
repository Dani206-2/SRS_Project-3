### data
data_m<-c(1, 1, 1, 15, 0, 3, 19, 54, 
          4, 19, 62, 464, 76, 703, 1006)
LA<- c(rep(1,8),rep(0,7))
NGO<- c(rep(1,4),rep(0,4),rep(1,4),rep(0,3))
PF<-c(rep(c(1,0,1,0),3),c(1,0,1))
GO<-c(rep(c(1,1,0,0),3),c(1,1,0))

check<-cbind(data_m,LA,NGO,PF,GO)
check<-as.data.frame(check)

combine_variable<-cbind(LA,NGO,PF,GO)

model1<-glm(data_m~LA+NGO+PF+GO,family=poisson)
summary(model1)

par(mfrow=c(2,2))
plot(model1)

#install.packages("psych")
library(psych)

### Compute the 1 pair of variables

  
  one<-which(check$LA==0&check$NGO==0)
  two<-which(check$LA==1&check$NGO==0)
  three<-which(check$LA==0&check$NGO==1)
  four<-which(check$LA==1&check$NGO==1)
  
  pair<-matrix(c(sum(check$data_m[one]), sum(check$data_m[two]), 
                 sum(check$data_m[three]), sum(check$data_m[four])), nrow=2)
  
  tetrachoric(pair)
  
  

model2 <- glm(data_m~LA+NGO+PF+GO+(LA+NGO+PF+GO)^2, family=poisson(link="log"))

summary(model2)

