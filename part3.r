# iris 데이터 행의 개수에서 70%에 해당하는 행번호를 랜덤으로 추출
# nrow(): 데이터의 행 개수를 산출해주는 함수
idx <- sample(1:nrow(iris), nrow(iris)*0.7, replace=FALSE)

# 추출한 행번호를 이용하여 training 데이터와 test 데이터 생성
training <- iris[idx,]
test <- iris[-idx,]

# 데이터의 개수 확인
dim(iris)        # 행의 개수 : 150개
dim(training)    # 행의 개수 : iris의 행 개수의 70%에 해당하는 105개
dim(test)        # 행의 개수 : iris의 행 개수의 30%에 해당하는 45개

# sampling 패키지 설치 및 로드
install.packages("sampling")
library(sampling)

# 층화임의추출을 수행한 뒤 sample 변수에 저장
sample <- strata(data=iris, c("Species"), size=c(20,15,15), method="srswor")

# sample의 상위 6개 행만 추출
head(sample)

# 추출된 데이터를 iris_sample 변수에 저장
iris_sample <- getdate(iris, sample)

# iris_sample의 상위 6개 행만 추출
head(iris_sample)

# 표본 데이터의 Species 변수에 대한 도수분포표 생성
table(iris_sample$Species)

# cats 데이터를 사용하기 위해 MASS 패키지 로드
library(MASS)

# cats 데이터의 구조 확인
str(cats)

# Bwt(고양이 몸무게) 변수에 대한 정규성 검정 수행
shapiro.test(cats$Bwt)

# 일표본 T-검정 수행
wilcox.test(cats$Bwt, mu=2.6, alternative="two.sided")

# 데이터 입력
data <- data.frame(before = c(7,3,4,5,2,1,6,6,5,4),
                   after = c(8,4,5,6,2,3,6,8,6,5))

data


t.test(data$before, data$after, alternative="less", paired=TRUE)

# 데이터 불러오기
library(MASS)
data("cats")

# 등분산 검정 수행
var.test(Bwt~Sex, data=cats)

t.test(Bwt~Sex, data=cats, alternative="two.sided", var.equal=FALSE)

# MASS 패키지의 survey 데이터 불러오기
data(survey, package="MASS")

# survey 데이터의 구조 확인
str(survey)

# W.Hnd 변수의 분할표 확인
table(survey$W.Hnd)

# W.Hnd변수의 분할표를 data변수에 저장
data <- table(survey$W.Hnd)

# 적합도 검정 수행
chisq.test(data, p=c(0.2,0.8))

# 분산분석 결과를 result 변수에 저장
result<-aov(Sepal.Width~Species, data=iris)

# 분산분석표 확인
summary(result)

TukeyHSD(aov(Sepal.Width~Species, data=iris))

# 데이터 확인 및 전처리
data("mtcars")
str(mtcars)

# aov함수를 사용하기 위해 독립변수인 cyl, am를 팩터형으로 변환
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

# cyl, am, mpg 변수들로만 구성된 분석용 데이터셋 생성
car <- mtcars[,c("cyl", "am", "mpg")]
str(cars)

# 분산분석 수행
car_aov <- aov(mpg~cyl*am, car)
summary(car_aov)

