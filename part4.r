# 1. 산점도
# MASS 패키지 및 데이터 로드
library(MASS)
data("Cars93")

# Length와 Weight 변수에 대한 산점도 생성
plot(Cars93$Length, Cars93$Weight)
plot(Cars93$Length, Cars93$Weight, main="Cars93", xlab="Length", ylab="Weight")

# Length와 Weight의 범위(최솟값, 최댓값) 구하기
