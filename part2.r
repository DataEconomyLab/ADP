x <- 9
if(x%%2==0) {                        # x/2의 나머지가 0이면 참
  print(paste(x, "는 짝수입니다."))  # x%%2==0 조건이 참일 때 수행할 코드
} else if (x%%2==1) {                
  print(paste(x, "는 홀수입니다."))  # 첫째 조건이 거짓, x%%2==1 조건이 참일 때 수행할 코드
} else {
  print("정수를 입력해 주세요.")     # 위의 조건 중 어느 것도 만족하지 않을 때 수행할 코드드
}

x <- 3.9
ifelse(x%%2 ==0, "짝수입니다", 
       ifelse(x%%2==1, "홀수입니다", "정수가 아닙니다."))

# paste() : () 안에 나열된 문자열을 합쳐주는 함수
print(paste("The year is", 2015))
print(paste("The year is", 2016))
print(paste("The year is", 2017))
print(paste("The year is", 2018))

# year변수에 2015에서 2018까지의 값이 모두 대입될 때까지 {}만의 구문이 반복
for(year in c(2015:2018)) {
  print(paste("The year is", year))
}

# year 변수의 값이 2018이하이면 {}안의 구문이 실행
year <- 2015
while(year<=2018) {
  print(paste("The year is", year))
  year <- year+1
}

# break 사용 예시
# 1~10까지의 숫자 중 짝수만 출력하기
i<-0
repeat{i<-i+2
        print(i)
    if(i>=10) {
      break        # i가 10이상이면 반복문 종료
    }
}

# next 사용 예시
# 1~10까지의 숫자 중 홀수만 출력하기
# 반복문 내의 next를 만나면 print(i)를 실행하지 않고 for 문의 맨 처음으로 이동
for(i in 1:10) {
  if(i%%2==0) { next }
  print(i)
}

# iris 데이터의 구조 확인 : 150개의 행과 5개의 변수를 가지고 있다.
str(iris)

# 150개의 개체들의 고유번호에 해당하는 'ID'변수를 새로 생성
iris$ID<-1:150

# iris 데이터의 상위 6개의 행을 출력한 결과, ID 변수가 추가된 것을 확인할 수 있다.
head(iris)

# ifelse 함수를 이용하여 ID가 짝수이면 A, 홀수이면 B를 부여
iris["group"] <- ifelse(iris$ID%%2==0, "A", "B")

# iris 데이터의 상위 6개의 행을 출력 : Group 변수가 추가된 것을 확인할 수 있다.
head(iris)

# Sepal.Length 변수와 Petal.Length 변수의 값을 더하여 'Sum.Length'라는 새로운 변수 생성
transform(iris, Sum.Length=Sepal.Length + Petal.Length)

# 학생id(student_id 변수)와 시험점수(score 변수)로 이루어진 데이터 프레임 생성
student_id <- c("s1", "s2", "s3", "s4", "s5", "s6")    # 학생id가 담긴 벡터
score <- c(55, 90, 85, 71, 63, 99)
score_df <- data.frame(student_id, score)
score_df

# 학생의 점수(score변수)를 수, 우, 미, 양, 가로 분류하여 'grade'라는 새로운 변수 생성
score_df <- within(score_df, {
  grade = character(0)        # 새로운 변수의 데이터 타입 지정 (생략 가능)
  grade[ score < 60 ] = "가"  # score가 60미만인 경우 grade에 "가" 입력
  grade[ score >= 60 & score < 70 ] = "양"
  grade[ score >= 70 & score < 80 ] = "미"
  grade[ score >= 80 & score < 90 ] = "우"
  grade[ score >= 90 ] = "수"
  grade = factor(grade, level = c("수", "우", "미", "양", "가"))
  # grade변수를 "수", "우", "미", "양", "가"의 범주로 이루어진 팩터로 변환
})

# score_df 출력: grade 변수가 추가된 것을 확인할 수 있다.
score_df

# 주성분분석
# 1. 데이터 확인 및 산점도를 통한 변수 간 상관관계 파악
library(datasets)
data(USArrests)

head(USArrests)

# 산점도를 통해 변수 간 상관관계 파악
# pairs() : 둘 이상의 변수에 대해 모든 가능한 산점도를 그려줌
pairs(USArrests, panel = panel.smooth, main = "USArrests data")

# 2. 주성분분석 수행
US.prin <- princomp(USArrests, cor=TRUE)
summary(US.prin)

# screenplot 그리기
# 주성분분석의 결과를 plot 함수 인자값으로 지정하면 scree plot이 생성된다.
plot(US.prin, type='l')

# 3. Loading
US.prin$loadings

# 4. Scores
US.prin$scores

# 5. 제 1-2 주성분에 의한 행렬도
biplot(US.prin, scale=0)

# 요인분석
# swiss 데이터 확인
data(swiss)
str(swiss)

# 정규화 수행 및 실습용 데이터 생성
# apply(데이터, 계산방향, 적용함수) : 주어진 데이터의 행 혹은 열 방향으로 함수를 적용
Min <- apply(swiss,2,min)
Max <- apply(swiss,2,max)
swiss_fa <- scale(swiss, center=Min, scale=(Max-Min))

head(swiss_fa)

# 요인분석 수행
# factanal 함수 rotation 인자의 기본값은 "varimax" 임 (직각회전의 베리맥스 회전)
factanal(swiss_fa, factors=3)

# 표준화
# 데이터 확인
data("mtcars")
str(mtcars)

# 데이터프레임 생성
test.cars <- mtcars[,c("mpg", "hp")]

# test.cars 데이터의 상위 6개의 행 확인
head(test.cars)

# 표준화한 변수 추가
test.cars <- transform(test.cars,
                       mpg_scale = scale(test.cars$mpg),
                       hp_scale = scale(test.cars$hp))

head(test.cars)

# iris 데이터 Sepal.Length 변수의 최솟값 저장
Min <- min(iris$Sepal.Length)

# iris 데이터 Sepal.Length 변수의 최댓값 저장
Max <- max(iris$Sepal.Length)

# scale 함수를 활용한 min-max 정규화
iris$SL_new <- scale(iris$Sepal.Length, center=Min, scale=Max-Min)

head(iris)

# 정규화한 값을 반환하는 함수 정의
# x: 정규화하고자 하는 숫자형 벡터
normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}

# 숫자형 벡터 생성
num <- c(50:500)

# 생성한 사용자 정의 함수를 이용하여 정규화 수행
num_new <- normalize(num)
head(num_new)

# 데이터 결합
# 행을 기준으로 묶을 데이터 생성 : rbind를 사용할 때에는 열의 이름과 개수가 동일해야 함
customer1 <- data.frame(id = c("c01", "c02", "c03", "c04"), 
                        last_name = c("Lee", "Kim", "Choi", "Park"))

customer2 <- data.frame(id = c("c05", "c06", "c07"),
                        last_name = c("Lim", "Bae", "Kim"))


# rbind를 사용하여 두 데이터프레임을 결합
id_name <- rbind(customer1, customer2)
id_name

# 열을 기준으로 묶을 데이터 생성 : cbind를 사용할 때에는 행의 개수가 동일해야 함
# age 변수와 income 변수로 구성된 데이터프레임 생성
age_income <- data.frame(age = c(20, 25, 37, 40, 32, 45, 37),
                         income = c(2500, 6400, 0, 7000, 3400, 3800, 5010))

# cbind를 사용하여 두 데이터프레임을 결합
customer <- cbind(id_name, age_income)
customer

# 병합에 사용할 데이터프레임 생성
id_name <- data.frame(id = c("c01", "c02", "c03", "c04", "c05", "c06", "c07"),
                      last_name = c("Lee", "Kim", "Choi", "Park", "Lim", "Bae", "Kim"))

id_number <- data.frame(id = c("c03", "c04", "c05", "c06", "c07", "c08", "c09"),
                        number = c(3, 1, 0, 7, 3, 4, 1))

# id_name, id_number 데이터프레임 확인
id_name
id_number

merge(id_name, id_number, by = 'id')

# 기준칼럼에 공통된 값이 없는 경우, 다른 변수 값 자리에는 NA가 채워짐
merge(id_name, id_number, by = 'id', all = T)
merge(id_name, id_number, by = 'id', all.x = T)
merge(id_name, id_number, by = 'id', all.y = T)

# 데이터 요약 - aggregate
aggregate(Sepal.Width~Species, iris, mean)
aggregate(cbind(Sepal.Width, Petal.Width)~Species, iris, mean)

# 내장데이터 Titanic의 구조 확인
str(Titanic)

# 분석을 위해 Titanic 데이터를 데이터프레임으로 변환한 뒤 다시 구조를 확인
# as.data.frame() : 객체를 데이터프레임으로 변환
Titanic <- as.data.frame(Titanic)

str(Titanic)         # 데이터 구조가 데이터프레임으로 변환되었음을 확인 가능

# table 함수를 이용하여 범주형 변수 Class에 대한 도수분포표를 생성
table(Titanic$Class)

table(Titanic$Class, Titanic$Survived)

# Age에 따른 Survived에 대한 비율을 파악
prop.table(table(Titanic$Age, Titanic$Survived))

# 행 별 비율 파악
prop.table(table(Titanic$Age, Titanic$Survived), 1)

# 열 별 비율 파악
prop.table(table(Titanic$Age, Titanic$Survived), 2)

subset(iris,
       subset = (Species == 'setosa' & Sepal.Length > 5.5), 
       select = c(Species, Sepal.Length))

       
a <- matrix(1:12, nrow=4, ncol=3)
apply(a, 1, max)

apply(iris[,1:4], 2, mean)       

a <- c(1,2,3)            # a는 1,2,3의 숫자가 저장된 벡터터
# FUN 인자의 값으로 제곱을 계산해 주는 사용자 정의 함수를 지정
lapply(a, FUN=function(x){x^2})

# 데이터 구조 확인하기 : lapply 함수를 적용한 후 반환되는 데이터는 리스트임을 알 수 있음
class(lapply(a, FUN=function(x){x^2}))      # class() : 객체의 물리적 자료형 반환

# 만약 리스트로 반환된 결과를 벡터로 변환하고 싶다면 unlist함수를 이용
b <- lapply(a, FUN=function(x){x^2})        # lapply를 적용한 결과를 변수 b에 저장
unlist(b)                                   # unlist함수를 이용하여 리스트 b를 벡터로 변환환ㄴ

sapply(iris, class)

# 데이터 구조 확인 : 변수마다 함수를 적용한 결과 값이 하나씩 존재하므로 문자형 벡터로 반환
class(sapply(iris, class))                  # "character"는 문자를 저장한 벡터를 의미미

sapply(iris, summary)                       # summary() : 데이터의 기초 통계량을 요약해주는 함수

# 데이터구조 확인 : 변수마다 함수를 적용한 결과 값의 길이가 다르므로 리스트로 봔환
class(sapply(iris, summary))

# fivenum() : 최소값, 1사분위수, 중위수, 3사분위수, 최대값을 차례로 출력해주는 함수

# 1~100까지의 숫자가 저장된 벡터생성
test <- c(1:100)

# fivenum 함수를 적용
fivenum(test)

# vapply를 이용하여 fivenum 함수를 적용하는데, 출력되는 결과의 형식(Template)을 직접 지정
test <- list(test)          # 출력형태를 리스트로 변환

# 출력되는 결과의 양식(Template)을 Min, Q1, Median, Q3, Max로 지정
test2 <- vapply(test, fivenum, c("Min" = 0, "Q1" = 0, "Median" = 0, "Q3" = 0, "Max" = 0))
test2

# rep(x, times, ...)
# x: 반복할 객체, times: 반복횟수
rep(1,4)
rep(2,3)
rep(3,2)
rep(4,1)

# rep 함수의 x 인자값 : c(1:4), times 인자값:c(4,1)
mapply(rep, c(1:4), c(4:1))

# googleVis 패키지의 Fruits 데이터를 이용하기 위해 패키지 설치 및 로드
install.packages("googleVis")
library(googleVis)

# Fruits 데이터의 상위 6개의 행 확인
head(Fruits)

# tapply 함수를 이용하여 과일종류별 판매량의 평균 산출
tapply(Fruits$Sales, Fruits$Fruit, mean)

# INDEX 인자에 비교구문을 사용하여 그룹을 지정
tapply(Fruits$Profit, Fruits$Location=="West", mean)

# 패키지 설치 및 로드
install.packages("plyr")
library(plyr)

#변수명을 지정하지 않고 adply를 이용해 연산
adply(iris, 1, 
      function(row){ifelse(row$Petal.Length<1.5 &
                           row$Species=="setosa", "1", "0")})

# 변수명을 'setosa_PL1.5'로 지정하여 데이터프레임으로 반환
adply(iris,1,
      function(row) {
        data.frame(setosa_PL1.5=
                     c(ifelse(row$Petal.Length<1.5 &
                                row$Species=="setosa", "1", "0")))
      })

ddply(iris, .(Species), function(sub){
  data.frame(
    mean_SL=mean(sub$Sepal.Length), mean_SW=mean(sub$Sepal.Width),
    mean_PL=mean(sub$Petal.Length), mean_PW=mean(sub$Petal.Width))
})

# variables 인자 자리 .()에 그룹화 할 변수와 조건을 입력
ddply(iris, .(Species, Petal.Length<1.5), function(sub){
  data.frame(
    mean_SL=mean(sub$Sepal.Length), mean_SW=mean(sub$Sepal.Width),
    mean_PL=mean(sub$Petal.Length), mean_PW=mean(sub$Petal.Width))
})

# baseball 데이터 확인
str(baseball)

# 원본데이터에 avgG 칼럼(선수별 연평균 출전횟수)을 추가하여 출력
ddply(baseball, .(id), transform, avgG=sum(g)/length(year))

# 원본 데이터에 avgG 칼럼과 avgG_RND(avgG칼럼을 반올림) 칼럼을 한번에 추가하여 출력
# 이 경우, mutate가 아닌 transform을 사용하면 에러가 발생함
ddply(baseball, .(id), mutate, avgG=sum(g)/length(year), avgG_RND=round(avgG))

# summarise를 활용해 선수별 마지막 경기 출전년도 구하기기
ddply(baseball, .(id), summarise, year_fin=max(year))

# summarise를 활용해 팀별 홈런 수의 합 출력하기
ddply(baseball, .(team), summarise, hr_sum=sum(hr))

ddply(baseball, .(id), subset, year==max(year), select=c("id", "year", "stint", "team", "lg", "g"))

# 패키지 설치 및 로드
install.packages("dplyr")
library(dplyr)
library(MASS)

# filter 함수를 사용하여 조건에 맞는 행 추출
Cars93 %>% filter((Manufacturer=="Audi" | Manufacturer=="BMW") & EngineSize>=2.4)

# select를 사용하여 특정 변수만 추출
Cars93 %>% select(Model, Type, Price)
# 에러 발생 이유 : MASS 패키지의 select()와 dply의 select()가 충돌하기 때문

# 해결 방법 : 사용할 select 함수가 dplyr의 함수임을 명시해 주기
Cars93 %>% dplyr::select(Model, Type, Price)

# filter와 select를 조합하여 조건을 만족하는 데이터의 특정 열만 추출
Cars93 %>% filter((Manufacturer == "Chevrolet"|Manufacturer=="Volkswagen") & Price >= 10) %>%
  dplyr::select(Manufacturer, Model, Type, Price)

# group_by와 summarise를 조합하기
Cars93 %>% group_by(Manufacturer) %>%
  summarise(mean_Price=mean(Price), max_Weight=max(Weight))

Cars93 %>% group_by(Type, AirBags) %>% summarise(mean_Weight=mean(Weight))

# mutate를 이용해 파생변수 생성하기
Cars93 %>% mutate(Pr_level=ifelse(Price < 12, "low", 
                                  ifelse(Price >= 12 & Price < 23, "middle", "high"))) %>%
  dplyr::select(Model, Price, Pr_level)

# filter, select, group_by, mutate, arrange의 조합
Cars93 %>% 
  filter(Type %in% c("Midsize", "Small")) %>%
  dplyr::select(Model, Type, Weight, Price) %>%
  group_by(Type) %>%
  mutate(Weight_lv=ifelse(Weight<median(Weight),"low", "high")) %>%
  arrange(Price)

# NAME, PRICE 데이터 생성
NAME<-data.frame(code=c("A01", "A02", "A03"),
                 name=c("coffee", "cake", "cookie"))

NAME

PRICE<-data.frame(code=c("A01","A02","A04"),
                  price=c(3000, 4000, 3000))

PRICE

# left_join
cafe_left <- left_join(NAME, PRICE, by="code")
cafe_left

# right_join
cafe_right <- right_join(NAME, PRICE, by="code")
cafe_right

# inner_join
cafe_inner <- inner_join(NAME, PRICE, by="code")
cafe_inner

# full_join
cafe_full <- full_join(NAME, PRICE, by="code")
cafe_full

# base::rbind 함수를 이용해 데이터 결합
rbind(NAME, PRICE)         # 데이터 결합이 제대로 되지 않고, 에러가 발생함함

# dplyr::bind_rows 함수를 이용해 데이터 결합
bind_rows(NAME, PRICE)     #결합할 데이터들의 변수가 다르더라도 결합이 이루어지며, 빈자리는 NA로 채워짐

# 실습용 데이터 생성
A <- data.frame(code=c(1,2), name=c("coffee", "cake"))
B <- data.frame(code=c(3,4), name=c("cookie","juice"))
C <- data.frame(code=5, name="bread")

# 세 개의 데이터를 bind_rows함수를 이용해 행으로 결합
cafe_bind<-bind_rows(A,B,C, .id="id")
cafe_bind
# id열을 통해 1,2행의 원천은 첫 번째 데이터, 3,4행의 원천은 두 번째 데이터,
# 5행의 원천은 세번째 데이터임을 알 수 있음

# 실습용 데이터 생성
A<-data.frame(id=c(1:5), x=c(80,90,95,100,75))
B<-data.frame(y=c(80,75,100,90,80))

# bind_cols 함수를 이용해 데이터의 열을 붙여 결합
bind_cols(A,B)

# reshape2 패키지 설치 및 로드
install.packages("reshape2")
library(reshape2)

# melt함수를 사용한 데이터변환
melt(airquality, id.vars=c("Month", "Day"), na.rm=T)

# airquality 데이터에 melt함수를 적용하여 air_melt변수에 저장
air_melt<-melt(airquality, id.vars=c("Month", "Day"), na.rm=T)

# dcast 함수를 이용해 air_melt 데이터를 다시 원래 airquality의 형태로 변환
air_dcast<-dcast(air_melt, Month + Day ~ ...)

# air_dcast와 airquality 비교
# -> 변수의 순서는 조금 바귀었지만 변수명과 변수값은 동일한 것을 확인할 수 있음
head(air_dcast)
head(airquality)

dcast(air_melt, Month+Day ~ variable, fun.aggregate = mean)

# data.table 패키지 설치 및 로드
install.packages("data.table")
library(data.table)

# 데이터 테이블 생성
mydata <- data.table(x=c(1:3), y=c("가", "나", "다"))
mydata

# mydata의 데이터 타입 확인
class(mydata)          # 데이터테이블의 클래스가 data.frame도 포함하고 있음음

# iris 데이터의 클래스 : 데이터프레임
class(iris)

# iris 데이터를 데이터 테이블로 변환
iris_dt <- as.data.table(iris)
class(iris_dt)         # iris_dt의 클래스 확인인

# iris_dt 출력
iris_dt

# 모든 데이터 테이블 객체 확인
tables()

# iris_dt의 1행 출력
iris_dt[1,]

# iris_dt의 1~5행 출력
iris_dt[c(1:5),]

# Species 변수 값이 setosa인 행들만 출력
iris_dt[iris_dt$Species=="setosa"]

# Species별 Petal.Length의 평균 출력 (Species 변수로 데이터를 그룹화)
iris_dt[, mean(Petal.Length), by=Species]

# Petal.Length 값이 1 이상인 행들을 Species로 그룹화한 뒤,
# Sepal.Length와 Sepal.Width의 평균을 각각 mean.SL과 mean.SW를 변수명으로 하여 출력
iris_dt[Petal.Length>=1, .(mean.SL=mean(Sepal.Length), mean.SW=mean(Sepal.Width)), by=Species]

# airQuality를 데이터테이블로 변환
air<-as.data.table(airquality)
str(air)

# Wind_class 변수 생성
# := 기호 사용: air 데이터에 Wind_class 변수가 추가됨
air[, Wind_class:=ifelse(Wind>=mean(Wind),"U","D")]

# air 데이터 출력: Wind_class 변수가 추가된 것을 확인할 수 있음
air

# season 변수 생성
air[, season:=ifelse(Month %in% c(12,1,2), "winter", 
                     ifelse(Month %in% c(3:5), "spring",
                            ifelse(Month %in% c(6:8), "summer", "fall")))]

# air 데이터 1~5행 학인: season 변수가 추가되었음
air[1:5]

# season별 Ozone과 Solar.R 변수들의 평균 산출 및 정렬
air[, .(Ozone_mean=mean(Ozone, na.rm=T), Solar.R_mean=mean(Solar.R, na.rm=T)),
    by=.(season)][order(Ozone_mean, decreasing=T)]

# plyr 패키지에 내장된 baseball 데이터를 데이터 테이블로 변환
library(plyr)
baseball<-as.data.table(baseball)

# year 변수를 key로 지정
setkey(baseball, year)

# 1960년대 데이터 조회
baseball[J(1960)]

# 1960년대 경기 정보 중 팀별 출전횟수의 평균 산출
# 팀별 출전횟수의 평균을 저장할 변수 이름을 지정할 경우에는 'list(변수명=값)' 코드 활용
baseball[J(1960), list(gmean=mean(g)), by=team]

# is.na 함수 활용
is.na(airquality$Ozone)

# Ozone 변수에 존재하는 na의 개수 산출
sum(is.na(airquality$Ozone))

# Ozone 변수에 존재하는 na의 개수 산출
sum(is.na(airquality$Ozone))

# Ozone 변수에서 na가 아닌 값과 na값의 개수 비교
# table(): 범주별 도수를 구해주는 함수
# FALSE: 해당 데이터가 na가 아닌 경우, TRUE: 해당 데이터가 na인 경우
table(is.na(airquality$Ozone))

# airquality의 변수(열)별로 결측치의 개수를 구하는 사용자 정의 함수를 적용
# sum(is.na(x)): x 데이터에 존재하는 na값 개수의 합계를 구함
apply(airquality, 2, function(x) sum(is.na(x)))

# na 값이 하나라도 존재하는 행들은 air_na 변수에 저장
# complete.cases 함수를 적용했을 때 FALSE를 반환하는 행들만 저장하면 됨
air_na <- airquality[!complete.cases(airquality),]
head(air_na)

# na 값을 하나도 가지고 있지 않은 행들은 air_com 변수에 저장
# complete.cases 함수를 적용했을 때 TRUE를 반환하는 행들만 저장하면 됨
air_com <- airquality[complete.cases(airquality),]
head(air_com)

# 방법1
# 데이터에 NA가 존재할 경우 평균을 산출할 수 없다. 따라서 mean함수 내부의 na.rm인자 값을 T로 # 지정해야 하며,
# 이렇게 할 경우 NA값을 제거하고 나머지 값들에 대한 평균을 산출한다.

# ifelse 함수를 이용해 Ozone변수값이 na이면 평균으로 대체하고 na가 아니면 기존값을 그대로 가지게 함
airquality$Ozone <- ifelse(is.na(airquality$Ozone), mean(airquality$Ozone, na.rm=T), airquality$Ozone)
table(is.na(airquality$Ozone))

# 방법2
airquality[is.na(airquality$Ozone), "Ozone"] <- mean(airquality$Ozone, na.rm=T)
table(is.na(airquality$Ozone))

# 패키지 다운 및 로드
options("install.lock"=FALSE)
install.packages("Rtools")
install.packages("DMwR")
library(DMwR)

# airquality에서 na값을 가진 행들의 번호 추출
na_idx<-which(!complete.cases(airquality))

# na값을 제거하기 전의 원본 데이터를 air_before에 저장
air_before <- airquality

# na값을 제거한 데이터를 air_after에 저장 (centrallmputation 함수 활용)
air_after <- centralImputation(airquality)

# 두 데이터에서 na_idx에 저장된 행번호에 해당하는 데이터들을 추출하여 na값이 잘 대체되었는지
# 비교(head 함수를 사용하여 상위 6행만 비교)
head(air_before[na_idx,])
head(air_after[na_idx,])

# 아래 코드의 실행결과 Ozone변수의 중위수는 31.5, Solar.R변수의 중위수는 205이며
# air_before에 존재하던 NA값들이 각 변수의 중위수들로 잘 대체된 것을 확인할 수 있다.
median(airquality$Ozone, na.rm=T)
median(airquality$Solar.R, rn.rm=T)

# 참고) na.rm-=T 옵션: na값은 제외하고 나머지 숫자들로 중위수를 계산
boxplot(airquality$Ozone)

# boxplot의 반환값 확인
OzoneBP <- boxplot(airquality$Ozone)
OzoneBP

# Ozone의 1사분위수를 LowerQ에 저장
LowerQ <- fivenum(airquality$Ozone)[2]

# Ozone의 3사분위수를 UpperQ에 저장
UpperQ <- fivenum(airquality$Ozone)[4]

# Ozone 변수의 IQR을 구하여 IQR변수에 저장
IQR<- IQR(airquality$Ozone, na.rm=T)

# Ozone 값이 UpperQ + IQR*1.5보다 큰 행번호를 upperOutlier에 저장
# which(조건): 조건을 만족하는 행번호 추출
upperOutlier <- which(airquality$Ozone>UpperQ+IQR*1.5)
# Ozone 값이 LowerQ-IQR*1.5 보다 작은 행번호를 lowerOutlier에 저장
lowerOutlier<-which(airquality$Ozone<LowerQ-IQR*1.5)

# upperOutlier와 lowerOutlier에 해당하는 Ozone 변수의 값 출력
airquality[upperOutlier, "Ozone"]
airquality[lowerOutlier, "Ozone"]

# 현재 날짜를 today 변수에 wjwkd
today<-Sys.Date()
today

# today의 클래스 확인
class(today)

# 현재 날짜와 시간을 time변수에 저장
time<-Sys.time()
time

# time의 클래스 확인
class(time)

# today 내부의 값 확인 : 1970년 1월 1일 이후로 경과한 일 수를 의미함
unclass(today)

# time 내부의 값 확인 : 1970 1월 1일 00:00:00 이후로 경과한 초 수를 의미함
unclass(time)

# unclass를 적용한 time 값을 다시 원래 날짜 형식으로 변환하기
# unclass(time)은 1970년 1월 1일 이후로 경과한 초 수를 의미하므로,
# origin 인자값으로 "1970-01-01"을 지정
as.POSIXct(unclass(time), origin="1970-01-01")

# 현재 시간을 구한 뒤 POSIXlt 형식으로 변환하여 time 변수에 저장
time <- as.POSIXlt(Sys.time())

# 연(year) 추출
# time$year에는 1900년 이후의 경과 년도수가 저장되어 있으므로 현재 년도를 구하기 위해서는 1900을 더해야 함
time$year+1900

# 월(mon) 추출: 월 정보는 0-11값으로 저장되어 있기 때문에 1을 더함
time$mon+1

# 일(mday) 추출
time$mday

# 요일(wday) 추출출
time$wday

# 현재 날짜와 시간을 now에 저장
now <- Sys.time()

# now 데이터의 class 확인
class(now)

# now 데이터를 문자형으로 변환
format(now, "%y-%m-%d %H:%M:%S")

# 문자형으로 변환한 데이터의 class 확인
class(format(now, "%y-%m-%d %H:%M:%S"))

date <- as.Date("20200101", format="%Y%m%d")
date

# date의 class 확인 : Date 형식으로 변환된 것을 확인할 수 있음
class(date)

# 현재 날짜로브터 100일 후의 날짜 구하기
Sys.Date() + 100

# "2020-01-01"로 표현된 데이터로부터 365일 후의 날짜 구하기
as.Date("2020-01-01", format="%Y-%m-%d") + 365

# "1990-01-01"과 "2025-01-01" 사이의 일 수 구하기
as.Date("1990-01-01") - as.Date("2025-01-01")

# 두 날짜 사이의 일 수 만을 구하고 싶다면 날짜끼리 뺄셈을 한 후
# 해당 값을 숫자형으로 변환하면 된다.
days <- as.Date("1990-01-01") - as.Date("2025-01-01")
as.numeric(days)

# 오늘 날짜와 "1990-01-01" 사이의 일 수 구하기
difftime("1990-01-01", Sys.Date())

# "09:40:00" 시간과 "18:30:00" 시간 사이의 차이 구하기
as.difftime("09:40:00") - as.difftime("18:30:00")
