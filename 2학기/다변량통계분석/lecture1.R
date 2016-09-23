
### multivariate normal

install.packages("MVA")
library(MVA)

demo("Ch-MVA")

hypo
dim(hypo)

measure
dim(measure)
cov(measure[,1:3])  #공분산행렬(양적변수만 보기위해 ,1:3)
cov(measure[measure$gender=="male",1:3])  #남자의 공분산
cov(measure[measure$gender=="female",1:3]) #여자의 공분산

pairs(cor(measure[,1:3]))


round(cor(USairpollution),3)
cor.test(~SO2+temp, data=USairpollution) #SO2와 temp 두 변수의 상관계수가 유의한지.
 #Ho : P=0, H1 : P!=0) ; p-value=0.004624 --> 귀무가설 기각 --> P!=0 --> 두 변수간 상관관계가 있다고 말할 수 있다.

summary(USairpollution)
apply(USairpollution,2,sd)
#각 변수의 단위가 다르기때문에, 비교가 어려움. 따라서 비교를 위해, 평균이 0으로 표준화를 시켜보자
USairpollution_S <- scale(USairpollution) #scale
summary(USairpollution_S) #모든 변수의 평균이 0이 됨.
apply(USairpollution_S, 2, sd) #각 변수의 표준편차를 2=열 기준으로 구하기

plot(USairpollution)
plot(USairpollution[,1],USairpollution[,2])  
plot(USairpollution_S[,1],USairpollution_S[,2])
round(cov(USairpollution_S),3)  #공분산을 계산했는데, ..음? 공분산matrix와 

head(USairpollution)
#거리를 구할 때도 scale 이후 구하는게 나음.
round(dist(USairpollution_S),3)



install.packages("mvtnorm")
library(mvtnorm)

install.packages("rgl")
library(rgl)

x2=x1=seq(-4,4,0.1)
x1=matrix(x1,length(x1),length(x1))
x2=matrix(x2,length(x2),length(x2),byrow=T)


# independent normal  #독립일경우
f=dmvnorm(cbind(as.vector(x1),as.vector(x2)))
f=matrix(f,dim(x1)[1],dim(x1)[1])
persp3d(x1,x2,f,col="lightblue",xlab="x1",ylab="x2")

X=rmvnorm(500,c(0,0),matrix(c(1,0,0,1),2,2))
plot(X[,1],X[,2],xlim=c(-4,4),ylim=c(-4,4),xlab="x1",ylab="x2")


# correlated normal
f=dmvnorm(cbind(as.vector(x1),as.vector(x2)),c(0,0),matrix(c(1,0.5,0.5,1),2,2))
f=matrix(f,dim(x1)[1],dim(x1)[1])
persp3d(x1,x2,f,col="lightblue",xlab="x1",ylab="x2")

X=rmvnorm(500,c(0,0),matrix(c(1,0.5,0.5,1),2,2))
plot(X[,1],X[,2],xlim=c(-4,4),ylim=c(-4,4),xlab="x1",ylab="x2")


### qq plot
x=seq(-4,4,0.01)

q=seq(0.1,0.9,0.1)
plot(x,dnorm(x),type='l',lwd=1.5,ylab="f(x)",xaxt="n")
for (i in 1:length(q)){
  lines(c(1,1)*qnorm(q[i]),c(-0.1,dnorm(qnorm(q[i]))))
}

axis(1,at=qnorm(q),labels=parse(text=paste("q[",q,"]",sep="")),cex.axis=0.8)
