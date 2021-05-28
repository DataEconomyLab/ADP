# iris 데이터 행의 개수에서 70%에 해당하는 행번호를 랜덤으로 추출
# nrow(): 데이터의 행 개수를 산출해주는 함수
idx <- sample(1:nrow(iris), nrow(iris)*0.7, replace=FALSE)

# 추출한 행번호를 이용하여 training 데이터와 test 데이터 생성
training <- iris[idx,]
test <- iris[-idx,]

