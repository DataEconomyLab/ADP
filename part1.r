name <- "Jane";
name
number <- c(1,2,3);
number

a <- 1      # a 라는 변수에 정수 1 저장
b <- 2.7    # b 라는 변수에 소수 2.7 저장

a
b

mode(a)
mode(b)

a <- "가"
mode(a)   # a 변수의 데이터 타입 확인

b <- "1"
mode(b)   # b 변수의 데이터 타입 확인

c <- "2"
b+c       # 문자형 데이터에 대한 연산을 실행할 시 에러 발생

a <- TRUE # a 변수에 논리값 TRUE 저장
mode(a)   # a의 타입 확인 : logical(논리형)

b <- FALSE
mode(b)   # b의 타입 확인 : logical(논리형)

a+b       # 논리형 데이터는 산술연산이 가능

# f범주의 라벨은 "여", m범주의 라벨은 "남"으로 지정
factor(c("m","m","f","f","f"), levels=c("f","m"), labels=c("여", "남"), ordered=F)

# 1,2,3이 저장된 벡터를 변수 a에 저장(c() 사용)
a <- c(1,2,3)
a

# 1,2,3이 저장된 벡터를 변수 b에 저장(사용)
b<-1:3
b

# c(1,2,3)을 3번 반복
rep(c(1,2,3),3)

# 1을 1번 반복, 2를 2번 반복
rep(c(1,2), c(1,2))

# 1부터 10까지 2씩 증가하는 수열을 변수 c에 저장
c <- seq(1,10,2)
c

# 1~5까지의 숫자가 저장된 벡터 z 생성
z <- c(1:5)

# z 출력
z

z[2]          # z 벡터의 두 번째 원소 출력
z[c(1,3)]     # z 벡터의 첫 번째, 세 번째 원소 출력
z[1:3]        # z 벡터의 1~3번째 원소를 출력
z[-5]         # z 벡터의 다섯 번째 원소만 제외하고 출력
z[z>3]        # z 벡터의 원소 중 3보다 큰 값만을 출력

# 1~3까지의 값이 저장된 벡터 x 생성
x <- c(1,2,3)

# x의 각 요소에 대해 Kim, Park, Lee 라는 이름을 저장
names(x) <- c("Kia", "Park", "Lee")

# x벡터에서 이름이 Kim인 원소 출력
x["Kia"]

# x벡터의 각 요소에 부여된 이름 출력
names(x)

# names(x)의 두 번째 값 출력
names(x)[2]

# 벡터 연산의 예시
x <- c(1:4)
x

x*3        # x 벡터의 전체 원소에 3을 곱함
x+5        # x 벡터의 전체 원소에 5를 더함

# 벡터 간 연산의 예시
y <- c(5:8)
y

z <- c(1:2)
z

# 길이가 같은 벡터 간 연산
x+y

# 길이가 다른 벡터 간 연산
x+z

# 행렬 생성
matrix(1:9, nrow=3, ncol=3, dimnames=list(c("r1", "r2", "r3"), c("a", "b", "c")))

# 데이터프레임 생성
x <- data.frame(이름 = c("이유리", "최민준", "김민지"),
                  전공 = c("경영학과", "컴퓨터공학과", "데이터과학자"),
                  성별 = c("여", "남", "여"), 나이 = c(20,22,21))

# x 테이터 출력
x

# x 데이터의 구조 확인
str(x)

# 문자형 벡터 생성
v1 <- c("가", "나", "다")

# 논리형 벡터 생성
v2 <- c(T,F,F)

# 데이터프레임 생성
df <- data.frame(subject=c("미술", "음악", "체육"), class=c("1교시", "2교시", "3교시"))

# 문자형 벡터 생성
v1 <- c("가", "나", "다")

# 논리형 벡터 생성성
v2 <- c(T,F,F)

# 데이터프레임 생성
df <- data.frame(subject=c("미술", "음악", "체육"), class=c("1교시", "2교시", "3교시"))

# key를 지정하지 않고 리스트 생성
ls1 <- list(v1, v2, df, sum)
ls1

# key를 지정하여 리스트 생성
ls2 <- list(v1=v1, v2=v2, df=df, fun=sum)
ls2

# 배열에 저장될 매트릭스의 행 이름
rname = c("1행", "2행", "3행")
# 배열에 저장될 매트릭스의 열 이름름
cname = c("1열", "2열", "3열")
# 배열에 저장될 매트릭스의 이름름
mname = c("matrix_1", "matrix_2", "matrix+3")

ar <- array(1:27, dim=c(3,3,3), dimnames=list(rname, cname, mname))

# ar 출력
ar

# 함수 생성
fun1 <- function(num) {
  return(num*2)
}

# fun1 함수에 숫자 2를 입력
fun1(2)

# 함수 생성
fun2 <- function(...) {
  x <- sum(...)
  paste("합계 : ", x)
}

# fun2 함수에 2,4,6,8,10을 입력
fun2(2,4,6,8,10)

# 함수 생성
fun3 <- function(x,y) {
  print(x)
  print(y)
  fun4 <- function(x,y) {
    sum <- x+y
    paste(x, "+", y, "=", sum)
  }
  fun4(x,y)
}

# fun3 함수에 3.5를 입력
fun3(3,5)

