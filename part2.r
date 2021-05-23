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


