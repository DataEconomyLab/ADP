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

# 데이터 확인 및 전처리
data("airquality")
str(airquality)

# air 데이터 생성
air <- airquality[,c(1:4)]
str(air)

# 피어슨 상관계수 계산
cor(air, use="pairwise.complete.obs", method="pearson")

# 켄달 상관계수 계산
cor(air, use="pairwise.complete.obs", method="kendall")

# 스피어만 상관계수 계산
cor(air, use="pairwise.complete.obs", method="spearman")

# air 데이터에 대한 상관계수 행렬 생성
air_cor <- cor(air, use="pairwise.complete.obs")

# 상관행렬 시각화
pairs(air_cor)

# 피어슨 상관계수에 대한 검정 수행
cor.test(air$Ozone, air$Wind, method="pearson")

# 데이터 로드 및 확인
library(MASS)
data("Cars93")
str(Cars93)

# 단순 선형 회귀 모형 생성
lm(Price~EngineSize, Cars93)

# 모형 살펴보기: summary() 이용
# summary(): 주어진 인자에 대한 요약 정보 산출
Cars93_lm <- lm(Price~EngineSize, Cars93)
summary(Cars93_lm)

# 2x3 형태로 그래프를 배치하기 위해 화면 조정
par(mfrow=c(2,3))

# 그래프 생성
plot(Cars93_lm, which=c(1:6))

# 회귀모형 생성
Cars93_lm <- lm(Price~EngineSize, Cars93)

# 실습을 위해 시드값 설정
set.seed(1234)

# Cars93 데이터에서 랜덤으로 5개의 행번호를 추출하여 idx 변수에 저장장
idx <- sample(1:nrow(Cars93),5) 
idx

# 예측에 사용할 데이터셋 구성
test <- Cars93[idx,]

# 예측 수행1 (점추정)
predict.lm(Cars93_lm, test, interval="none")

# 예측 수행2 (회귀계수의 불확실성을 감안한 구간추정)
predict.lm(Cars93_lm, test, interval="confidence")

# 예측 수행3 (회귀계수의 불확실성과 오차항을 감안한 구간추정)
predict.lm(Cars93_lm, test, interval="prediction")

# 중회귀모형 생성
# formula 인자값으로 여러개의 독립변수를 지정할 때 + 기호로 연결
iris_lm<-lm(Petal.Length~Sepal.Length+Sepal.Width+Petal.Width+Species, iris)

# 모형 확인
summary(iris_lm)

# 4. R을 이용한 다중선형회귀분석
# 데이터 로드 및 확인
library(MASS)
str(Cars93)

# 다중회귀모형 생성 후 Price_lm 변수에 저장
Price_lm<-lm(Price~EngineSize+Weight, Cars93)

# 모형 요약정보 살펴보기
summary(Price_lm)

# 6. R을 이용한 변수선택법
# 패키지 로드
library(MASS)

# 회귀모형을 생성한 후 lm_a 변수에 저장
lm_a<-lm(Price~EngineSize+RPM+Width+Length, Cars93)

# 모형의 요약정보 확인
summary(lm_a)

# 회귀모형 lm_b 생성
lm_b<-lm(Price~EngineSize+RPM+Length, Cars93)

# 모형의 요약정보 확인
summary(lm_b)

# 회귀모형 lm_c 생성
lm_c<-lm(Price~EngineSize+RPM, Cars93)

# 모형의 요약정보 확인
summary(lm_c)

# 후진제거법을 활용한 변수 선택
lm_result<-lm(Price~EngineSize+Horsepower+RPM+Width+Length+Weight, Cars93)
step(lm_result, direction="backward")

