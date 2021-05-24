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
