# 1. 산점도
# MASS 패키지 및 데이터 로드
library(MASS)
data("Cars93")

# Length와 Weight 변수에 대한 산점도 생성
plot(Cars93$Length, Cars93$Weight)
plot(Cars93$Length, Cars93$Weight, main="Cars93", xlab="Length", ylab="Weight")

# Length와 Weight의 범위(최솟값, 최댓값) 구하기
range(Cars93$Length)
range(Cars93$Weight)

# x축과 y축 범위를 지정하여 산점도 그리기
# x축 범위 c(130, 230), y축 범위 c(1600, 4400)로 지정
plot(Cars93$Length, Cars93$Weight, main="Cars93", xlab="Length", ylab="Weight",
     xlim=c(130, 230), ylim=c(1600,4400))

# pch 인자에 8 지정 
plot(Cars93$Length, Cars93$Weight, main="Cars93", xlab="Length", ylab="Weight",
     xlim=c(130, 230), ylim=c(1600, 4400), pch=8)

# pch 인자에 "*" 지정
plot(Cars93$Length, Cars93$Weight, main="Cars93", xlab="Length", ylab="Weight",
     xlim=c(130, 230), ylim=c(1600, 4400), pch="*")

# cex 인자에 0.5 지정
plot(Cars93$Length, Cars93$Weight, main="Cars93", xlab="Length", ylab="Weight",
     xlim=c(130, 230), ylim=c(1600, 4400), cex=0.5)

# cex 인자에 2 지정
plot(Cars93$Length, Cars93$Weight, main="Cars93", xlab="Length", ylab="Weight",
     xlim=c(130, 230), ylim=c(1600, 4400), cex=2)

# type 인자에 "p" 지정
plot(tapply(Cars93$Weight, Cars93$Length, mean), xlab="Length", ylab="Weight", type="p")

# type 인자에 "b" 지정
plot(tapply(Cars93$Weight, Cars93$Length, mean), xlab="Length", ylab="Weight", type="b")

# 월(Month 변수) 별 평균 온도(Temp 변수의 평균값)에 대한 꺾은 선 그래프
plot(tapply(airquality$Temp, airquality$Month, mean), type="b", ylim=c(60,90),
     xlab="Month", ylab="Mean of Temperature", main="airquality")

# type 인자에 "l" 지정
plot(tapply(Cars93$Weight, Cars93$Length, mean), xlab="Length", ylab="Weight", cex=1.5, type="l", lty=4)

# 그래프 배열 설정
par(mfrow=c(1,2))

# 그래프 생성
plot(Cars93$Length, Cars93$Weight, main="Cars93", xlab="Length", ylab="Weight",
     xlim=c(130,230), ylim=c(1600, 4400), cex=0.5)

plot(Cars93$Length, Cars93$Weight, main="Cars93", xlab="Length", ylab="Weight",
     xlim=c(130,230), ylim=c(1600, 4400), cex=2)

# 빈 좌표평면 그리기
plot(1:10, type="n", xlab=" ", ylab=" ")

# 여러 위치에 다양한 범례를 생성
legend("bottom", c("x1", "y1"), pch=1:2, title="bottom")
legend("left", c("x2", "y2"), pch=3:4, title="left")
legend("top", c("x3", "y3"), pch=5:6, title="top")
legend("right", c("x4", "y4"), pch=7:8, title="right")
legend("center", c("x5", "y5"), lty=1:2, title="center")

# 좌표를 직접 입력하여 범례의 위치를 지정
legend(2.5,8,c("x6","y6"),lty=3:4,title="사용자 지정1")
legend(2.5,4,c("x7","y7"),lty=5:6,title="사용자 지정2")
legend(7.5,8,c("x8","y8"),lty=5:6,bg="gray",title="사용자 지정3")
legend(7.5,4,c("x9","y9"),pch=1:2,lty=7:8,bg="gray",title="사용자 지정4")

# 2. 그래프
# plot 함수를 이용해 빈 그래프 생성
plot(NULL, type="n", xlim=c(0,8), ylim=c(0,3), xlab="Petal.Length", ylab="Petal.Width", main="iris")

# points 함수를 이용해 산점도를 겹쳐 그리기
points(iris$Petal.Length, iris$Petal.Width, cex=0.5)

# plot함수를 이용해 빈 좌표평면 생성
plot(NULL, type="n", xlim=c(0,20), ylim=c(0,20), main="선 그래프")

# lines 함수를 이용해 선그래프 생성
lines(c(0,17), c(17,17), lty=1);
lines(c(0,15), c(15,15), lty=2);
lines(c(0,13), c(13,13), lty=3);

lines(c(0,11), c(11,11), lty="solid", lwd=1);
lines(c(0,9), c(9,9), lty="dotdash", lwd=2);
lines(c(0,7), c(7,7), lty="twodash", lwd=3);
lines(c(0,5), c(5,5), lty="longdash", lwd=4);

# cars 데이터의 두 변수(speed와 dist)에 대한 산점도
plot(cars, main="Stopping Distnace versus Speed")

# 산점도를 설명하는 지역 가중 다항식 회귀선
lines(lowess(cars))

# plot 함수를 이용해 cars 데이터에 대한 산점도 생성
plot(cars, ylim=c(0,130), xlim=c(0,30), main="cars data")

# speed와 dist 사이의 선형회귀모형 생성
cars_lm<-lm(dist~speed, data=cars)

# 선형회귀식에 대한 그래프 생성
abline(cars_lm, col="red")

# speed의 중위수를 그래프로 나타내기
abline(v=median(cars$speed), lty=3)

# dist의 중위수를 그래프로 나타내기
abline(h=median(cars$dist), lty=3)

# Origin 변수에 대한 막대그래프 생성
barplot(table(Cars93$Origin), ylim=c(1,50), xlab="Origin",
        ylab="도수", main="Cars93의 Origin변수")

# Cylinder변수에 대한 막대그래프 생성
barplot(table(Cars93$Cylinders), ylim=c(1,55), xlab="Cylinders",
        ylab="도수", main="Cars93의 Cylinders 변수")

# 두 변수를 하나의 막대에 나타내기 (beside=F 지정)
barplot(table(Cars93$Origin, Cars93$Cylinders), beside=F, ylim=c(0,60), legend=T)

# 각각의 변수를 별도의 막대에 나타내기 (beside=T 지정)
barplot(table(Cars93$Origin, Cars93$Cylinders), beside=T, ylim=c(0,30), legend=T)

# breaks 인자값을 지정하지 않은 히스토그램
hist(iris$Petal.Length)

# breaks를 5로 지정한 히스토그램
hist(iris$Petal.Length, breaks=5)

# 실린더의 개수별 차량의 비중을 파이 차트로 나타내기
pie(table(Cars93$Cylinders), main="실린더의 개수별 차량의 비중")

# 산점도 행렬 생성
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
      data=iris, col=c("red", "green", "blue")[iris$Species],
      pch=c("+","*","#")[iris$Species])

# Species의 범주 확인
levels(iris$Species)

