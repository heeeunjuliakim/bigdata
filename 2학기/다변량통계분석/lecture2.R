install.packages("MVA")
library(MVA)
demo("Ch-MVA")
head(USairpollution)

hist(USairpollution_S)
boxplot(USairpollution_S)

#산점도 그리기
plot(popul~manu, USairpollution)                 #방법 1
plot(USairpollution$manu, USairpollution$popul)  #방법2

#x y축 명을 바꾸고 싶다면
plot(USairpollution$manu, USairpollution$popul,
     xlab="Manufactureing enterprising with 20 or more workers",
     ylab="Population size",
     main="US airpollution")


library(psych)
pairs.panels(USairpollution)   
#대각선에 나오는 히스토그램들은 단변량 확인 가능
#오른쪽 상단 :  상관계수값

corr.test(USairpollution)  #상관계수, P-value 확인 가능

#이상치를 제외해보자!
#방법1 ) max값을 찾아서 검색
head(USairpollution)
summary(USairpollution)
USairpollution[USairpollution$manu==3344,]
USairpollution[USairpollution$manu>2500,]

#방법2 )
plot(USairpollution$manu, USairpollution$popul)  
identify(USairpollution$manu, USairpollution$popul,labels=rownames(USairpollution))
windows()

#도시명이 위의 4개인 값(이상치)들의 rownames를 뽑아내라.
outcity <- match(c("Chicago","Detroit","Houston", "Philadelphia"),
                 rownames(USairpollution))   
USairpollution[outcity,]

pairs.panels(USairpollution[-outcity,])   #이상치값을 제외하고 다시 그려보자.

##버블차트
with(USairpollution, symbols(temp, wind, circles=sqrt(SO2), inches=0.5))  #SO2의 크기에 따라 버블 그림
 # -> 온도와 바람에 따른 오염도의 상관관계

with(USairpollution, symbols(temp, wind, circles=sqrt(SO2), inches=0.5))


##모자이크 플롯
UCBAdmissions    #테스트 데이터
UCBAdmissions[,,2] #1번 테이블만 뽑을 때
UCBAdmissions[,1,] #합격 불합격
UCBAdmissions[1,,] #성별

mosaicplot(~Dept+Gender, 
           data=UCBAdmissions, color=T)

mosaicplot(~Gender+Dept, 
           data=UCBAdmissions, color=T)

#상자그림
head(chickwts)
summary(chickwts)
boxplot(weight~feed, chickwts)

#나이팅게일 차트
stars(USairpollution, cex=0.7, key.loc=C(15,2), draw.segments = TRUE)


######문제
#1.	Crime.csv는 2005년 미국의 범죄율 데이터로 범죄 유형별 발생건을 인구 100,000명 중의 발생 비율로 표시하였다. 살인, 강도, 폭행, 절도 등 총 7가지 범죄를 포함하는데 
#   이 중 살인(murder)와 절도(burglary) 사이의 관계를 살피려고 한다. 
#A.	두 변수 사이의 산점도를 단변량 분포와 함께 그리시오. 상관계수도 함께 살피시오.
Crime <- read.csv("Crime.csv")
summary(Crime)
head(Crime)
plot(murder~burglary, Crime) #산점도
install.packages("psych")
library(psych)
pairs.panels(Crime)   #단변량 분포, 상관계수
corr.test(Crime[,-1]) #상관계수 "0.28"

#B.	위를 통해 이상점 존재여부를 판단하고 존재한다면 해당 주를 확인하고 제거하시오. 제거 후 변수들 사이의 관계가 어떻게 변화하는지 살피시오.
plot(murder~burglary, Crime) #산점도
identify(Crime$murder, Crime$burglary, labels=Crime$state)

out <- match(c("United States","Alaska","District of Columbia"), Crime$state)
clr <- rep(1,dim(Crime)[1])  #1을 Crime의 변수만큼 1을 만들어라.
clr[out] <- 2                #미국, 알래스카, DC에 해당하는 인덱스에만 2를 넣어라.
windows()
plot(murder~burglary, Crime[-out,])
corr.test((Crime[-out,])[,-1])  #상관계수 "0.62"


pairs(Crime[,-1], col=clr, pch=clr) #2의 값만 빨강으로 출력
install.packages("psych")
library(psych)
pairs.panels(Crime) #단변량분포
corr.test(Crime)  #상관계수, P-value 확인 가능

identify(Crime$murder, Crime$burglary, labels=Crime$state)
identify(Crime$murder, Crime$burglary, labels=rownames(Crime[,1]))  #????

#C.	살인, 절도와 인구(population)의 관계를 함께 관찰하기 위해 bubble plot을 그리고 관찰한 사실을 기술하시오.
with(Crime, symbols(Crime$murder, Crime$burglary, circles =sqrt(population), inches = 0.5))

#D.	7가지 범죄의 발생 건수를 heatmap, 별그림, 나이팅게일 차트로 표현하고 범죄 발생 특징 간의 패턴이 비슷한 주들이 있는지 살피시오.

head(Crime)
heatmap(matrix(Crime3), Rowv=NA, Colv=NA, scale="column", margins=c(5,10), col=brewer.pal(9,"Blues"))
?heatmap

Crime2 <- Crime[,-1]
Crime3 <- Crime2[,-8]
str(Crime3)
matrix(Crime3)

stars(Crime, cex=0.7, key.loc = c(15,2))
stars(Crime, cex=0.7, key.loc=c(15,2), draw.segments = TRUE)
identify(Crime$murder, Crime$burglary, labels=Crime$state)
