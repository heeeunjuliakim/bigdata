install.packages("dplyr")
library(dplyr)

exam <- read.csv("exam1.csv")
head(exam1)
exam1 <- exam[,-5]

# SELECT 선택 #
filter(exam1, Exam2>=1 & Quiz<3.9)
filter(exam1, Exam2>=1, Quiz<3.9)
filter(exam1, Exam1>mean(exam1$Exam1) & Exam2>mean(exam1$Exam2))

# APPEND 추가 #
app <- c(6,3.5,1.5,3.5)
rbind(exam1, app)  #ID 6의 data rbind함수로 추가

app2 <- c(6,1)
rbind(exam1, app2)

# SORT 정렬 #
arrange(exam1, Quiz     )  #arrange default값 오름차순
arrange(exam1, desc(Quiz)) #내림차순

 #Quiz와 Exam1 순서로 오름차순으로 정렬한다면???
arrange(exam1, (Quiz&Exam1)) #Quiz와 Exam1 순서로 오름차순 ???

# SAMPLE #
sample_n(exam1, 3)      #데이터에서 3개의 샘플을 무작위로 추출하라
sample_frac(exam1, 0.4) #데이터에서 전체의 40%의 샘플을 추출하라 

exam1[as.logical((1:nrow(exam1))%%2),]    #a%%b : a를 b로 나눈 나머지값
# (1:nrow(exam1))%%2 = 1,0,1,0,1
# as.logical((1:nrow(exam1))%%2 = T,F,T,F,T)
# exam1[as.logical((1:nrow(exam1))%%2),] = T의 값만 출력 
# 1,3,5만 나오게 된다 (1-in-2 sampling)


# AGGREGATE #
exam3 <- read.csv("exam1.csv")

by_gender <- group_by(exam3, Gender)
summarise(by_gender, exam1=mean(Exam1), quiz=median(Quiz))

exam3 %>%
  group_by(Gender) %>%
  summarise_each(funs(min, max), Exam1, Exam2, Quiz)

# DISTINCT #
filter(exam1, !duplicated(Quiz))

distinct(exam1, Quiz)  #return unique values

# DERIVE 끌어내다. 파생 # 
exam1 <- mutate(exam1, ExamSum=Exam1+Exam2, ExamMean=ExamSum/2)

# Filter #
select(exam1, ID:Exam2)  #Column명 ID ~ Exam2 까지의 값을 뽑아라
exam1 <- select(exam1, -ExamSum, -ExamMean) #Column 명ExamSum, ExamMean의 값을 빼라
rename(exam1, id=ID, quiz=Quiz, ex1=Exam1, ex2=Exam2)

# Field Reorder #
select(exam1, ID, Quiz, Exam1, Exam2) #결과값을 원하는 순서대로 뽑기

# Filler #
exam1$Extra <- c(1,1,NA,NA,2)  #exam1 데이터 마지막 열에 Extra 내용을 추가해라
exam1$Extra[is.na(exam1$Extra)] <- 0 #[is.na()] NA 결측값 0으로 변경

# MERGE #


#--------------------------------------------------------



Exercises
cs <- read.table("dataCustomers.tab", sep="\t", header=T, stringsAsFactors = F)
tr <- read.table("dataTransactions.tab", sep="\t", header=T, stringsAsFactors = F)


filter(cs, age>=50&age<=59&gender=="du"&marriage=="기혼" 
       

#3)       
group_by(cs, gender) %>%
  summarise(age=mean(age))
       
#4)
distinct(cs, residence)


#5)
mg <- merge(cs, tr)
head(mg)

mg %<%
  group_by(gender) %>%
  summarise_each(funs(min, median, max), amount)  #?

#6)
group_by(tr, custid) %>%
  summarise(amount=sum(amount)) %>%
  arrange(desc(amount)) %>%
  head(10)



