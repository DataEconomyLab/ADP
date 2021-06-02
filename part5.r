# 1. 데이터 분할과 성과분석
# 데이터 분할 전, 데이터 불러오기
credit.df <- read.csv("https://raw.githubusercontent.com/PacktPublishing/R-Data-Analytics-Projects/master/Section05/german_credit_dataset.csv", header=TRUE, sep=",")   # 데이터 로드
nrow(credit.df)                    # 분석 데이터의 행 개수를 파악

set.seed(1111)                     # 난수를 동일하게 추출되도록 고정시키는 함수
idx<-sample(3, nrow(credit.df),    # idx에 1,2,3을 credit 데이터 행 개수와 동일하게 추출
            replace=T,             # 랜덤 복원추출을 실시
            prob=c(0.5,0.3,0.2))   # 1은 train으로 전체의 50%,
                                   # 2는 validation으로 전체의 30%, 3은 test로 전체의 20%
train<-credit.df[idx==1,]          # 분석 데이터에 idx 값과 비교하여, idx가 1인 행은 train으로 추출
validation<-credit.df[idx==2,]
test<-credit.df[idx==3,]

nrow(train)                        # train 데이터의 행 개수 파악
nrow(validation)                   # validation 데이터의 행 개수 파악
nrow(test)                         # test 데이터의 행 개수 파악

install.packages("caret")
library(caret)
part<-createDataPartition(credit.df$credit.rating,  # 목적변수인 credit.rating를 지정
                          times=1,                  # 생성할 데이터 분할은 1개로 지정
                          p=0.7)                    # 훈련데이터를 70%로 설정 

parts<-as.vector(part$Resample1)   # 행번호를 벡터형으로 변환
train<-credit.df[parts,]
test<-credit.df[-parts,]

nrow(train)
nrow(test)

install.packages("caret")
library(caret)
predicted<-factor(c(1,0,0,1,1,1,0,0,0,1,1,1))
actual<-factor(c(1,0,0,1,1,0,1,1,0,1,1,1))
xtabs(~predicted + actual)         # 분할표 그리기

sum(predicted==actual)/NROW(actual) # 정분류율(Accuracy)을 직접 식으로 계산
confusionMatrix(predicted, actual) # predicted 값과 actual 값 모두 factor형 변수로 되어 있어야 됨  

library(ROCR)
set.seed(12345)
probability<-runif(100)            # runif 함수는 균일분포값을 무작위로 추출하는 함수
labels<-ifelse(probability>0.5&runif(100)<0.4,1,2)
pred<-prediction(probability, labels)    # prediction 함수로 ROC 커브를 그릴 값을 예측
plot(performance(pred, "tpr", "fpr"))    # ROC CURVE 그래프 그리기기

# 2. 분류 분석
# credit 데이터 불러오기
credit<-read.csv("credit_final.csv")
class(credit$credit.rating)
credit$credit.rating<-factor(credit$credit.rating)  # 종속변수 factor 변환
str(credit)

# 데이터 분할: train 70%, test 30%
set.seed(123)
idx<-sample(1:nrow(credit),nrow(credit)*0.7,replace=FALSE)
train<-credit[idx,]
test<-credit[-idx,]

# 로지스틱 회귀분석 실시
logistic<-glm(credit.rating~.,
              data=train,
              family="binomial")
summary(logistic)                 # 회귀계수의 p-value가 유의수준 0.05 하에서 유의하지 않은 변수가 많음
                                  # step 함수를 활용하여 다시 분석 실시

# step 함수를 활용한 로지스틱 회귀분석 실시
step.logistic<-step(glm(credit.rating~1, data=train, family="binomial"),
                    scope=list(lower=~1, upper=~account.balance+credit.duration.months+previous.credit.payment.status+
                                 credit.purpose+credit.amount+savings+employment.duration+installment.rate+
                                 marital.status+guarantor+residence.duration+current.assets+age+other.credits+
                                 apartment.type+bank.credits+occupation+dependents+telephone+foreign.worker),
                    direction="both")

summary(step.logistic)

# 예측을 통한 정분류율 확인
install.packages("caret")
library(caret)
pred<-predict(step.logistic,test[,-1],type="response")              # 예측값을 "response"로 지정하여 확률값을 출력
pred1<-as.data.frame(pred)
pred1$grade<-ifelse(pred1$pred<0.5,pred1$grade<-0,pred1$grade<-1)   # 확률값만 나오므로 0.5를 기준으로 0,1 범주 추가
confusionMatrix(data=as.factor(pred1$grade), reference=test[,1], positive='1')

# ROC 커브 그리기 및 AUC 산출
install.packages("ROCR")
library(ROCR)
pred.logistic.roc<-prediction(as.numeric(pred1$grade),as.numeric(test[,1]))
plot(performance(pred.logistic.roc,"tpr","fpr"))                    # ROC Curve 그리기   
abline(a=0, b=1, lty=2, col="black")

# iris 데이터 train, test로 분할
idx<-sample(1:nrow(iris),nrow(iris)*0.7,replace=FALSE)
train.iris<-iris[idx,]
test.iris<-iris[-idx,]

# train 데이터로 다항 로지스틱 회귀분석 실시
library(nnet)
mul.iris<-multinom(Species~., train.iris)

# 예측을 통한 정분륭류 확인
pred.mul<-predict(mul.iris,test.iris[,-5])
confusionMatrix(pred.mul, test.iris[,5])

# rpart 함수를 활용하여 의사결정나무분석 실시
library(rpart)
library(rpart.plot)
dt.model <- rpart(credit.rating~.,                        # 종속변수는 credit.rating, 독립변수는 모든 변수
                  method="class",                         # method는 분류는 "class" 선택
                  data=train,
                  control = rpart.control(maxdepth=5,     # 의사결정나무의 최대 깊이는 5개까지      
                                          minsplit=15))   # 노드에서 최소 관측치는 15개 이상

prp(dt.model,type=4,extra=2)

dt.model$cptable

# rpart 함수를 활용하여 의사결정나무분석 실시(최적 나무 선정)
dt.model$cptable

opt<-which.min(dt.model$cptable[,"xerror"])
cp<-dt.model$cptable[opt,"CP"]
prune.c<-prune(dt.model,cp=cp)
plotcp(dt.model)

# 예측을 통한 정분류율 확인
install.packages("caret")
library(caret)
pred.dt<-predict(dt.model,test[,-1],type="class")       # 예측값을 "class"로 지정하여 분류 그룹을 출력
confusionMatrix(data=pred.dt, reference=test[,1], positive='1')

# ROC 커브 그리기 및 AUC 산출
install.packages("ROCR")
library(ROCR)
pred.dt.roc<-prediction(as.numeric(pred.dt),as.numeric(test[,1]))
plot(performance(pred.dt.roc,"tpr","fpr"))       # ROC Curve 작성
abline(a=0,b=1,lty=2,col="black")

performance(pred.dt.roc,"auc")@y.values

# rpart 함수를 활용하여 의사결정나무분석 실시
library(rpart)
library(rpart.plot)
dt.model2<-rpart(Species~., data=train.iris)
prp(dt.model2,type=4,extra=2)

pred.dt2<-predict(dt.model2,test.iris[,-5],type="class")
confusionMatrix(data=pred.dt2,reference=test.iris[,5])

# bagging 함수를 활용하여 bagging 분석 실시
library(adabag)
bag<-bagging(credit.rating~.,
             data=train,
             mfinal=15)
names(bag)
bag$importance

# 예측을 통한 정분류율 확인
install.packages("caret")
library(caret)
pred.bg<-predict(bag,test,type="class")
confusionMatrix(data=as.factor(pred.bg$class),     # class 열을 factor로 변환하여 test의 class열과 형태를 맞춤
                reference=test$credit.rating,
                positive='1')

# ROC 커브 그리기 및 AUC 산출
install.packages("ROCR")
library(ROCR)
pred.bg.roc<-prediction(as.numeric(pred.bg$class), as.numeric(test[,1]))
plot(performance(pred.bg.roc,"tpr","fpr"))         # ROC Curve 작성
abline(a=0,b=1,lty=2,col="black")

performance(pred.bg.roc,"auc")@y.values

# boosting 함수를 활용하여 boosting 분석 실시
library(adabag)
boost<-boosting(credit.rating ~.
                data=train,
                boos=TRUE,
                mfinal=80)                         # 반복 또는 트리의 수는 80   
names(boost)

boost$importance      # importance는 변수의 상대적인 중요도를 나타내며, 지니지수의 gain을 고려한 측도

# 예측을 통한 정분류율 확인
install.packages("caret")
library(caret)
pred.boos<-predict(boost,test,type="class")
confusionMatrix(data=as.factor(pred.boos$class),
                reference=test$credit.rating,
                positive='1')

# ROC 커브 그리기 및 AUC 산출
install.packages("ROCR")
library(ROCR)
pred.boos.roc<-prediction(as.numeric(pred.boos$class),as.numeric(test[,1]))
plot(performance(pred.boos.roc,"tpr","fpr"))        # ROC Curve 작성
abline(a=0,b=1,lty=2,col="black")

performance(pred.boos.roc,"auc")@y.values

# randomForest 함수를 활용하여 RandomForest 분석 실시
install.packages("randomForest")
library(randomForest)
rf.model<-randomForest(credit.rating ~.,
                       data=train,
                       ntree=50,
                       mtry=sqrt(20),
                       importance=T)
rf.model

names(rf.model)
rf.model$importance
varImPlot(rf.model)

# 예측을 통한 정분류율 확인
install.packages("caret")
library(caret)
pred.rf<-predict(rf.model,test[,-1],type="class")
confusionMatrix(data=pred.rf, reference=test[,1], positive='1')

# ROC 커브 그리기 및 AUC 산출
install.packages("ROCR")
library(ROCR)
pred.rf.roc<-prediction(as.numeric(pred.rf),as.numeric(test[,1]))
plot(performance(pred.rf.roc,"tpr","fpt"))
abline(a=0,b=1,lty=2,col="black")

abline(a=0,b=1,lty=2,col="black")
perfomance(pred.rf.roc,"auc")@y.values[[1]]

# randomForest 함수를 활용하여 RandomForest 분석 실시
library(randomForest)
rf.model2<-randomForest(Species ~.
                        data=train.iris,
                        ntree=50,
                        mtry=sqrt(4),
                        importance=T)

rf.model2

pred.rf2<-predict(rf.model2,test.iris[,-5],type="class")
confusionMatrix(data=pred.rf2, reference=test.iris[,5],positive='1')

# tune.svm 함수를 활용하여 최적의 파라미터값 찾기
library(e1071)
tune.svm(credit.rating~.,
         data=credit,
         gamma=10^(-6:-1),         # 여기서는 6*2=12개의 조합에서 모수조율이 이루어짐
         cost=10^(1:2))

# svm 함수를 활용하여 SVM 분석 실시
svm.model<-svm(credit.rating~.,
               data=train,
               kernel="radial",
               gamma=0.01,
               cost=10)
summary(svm.model)


# 예측을 통한 정분류율 확인
install.packages("caret")
library(caret)
pred.svm<-predict(svm.model,test,type="class")
confusionMatrix(data=pred.svm, reference=test[,1], positive='1')

# ROC 커브 그리기 및 AUC 산출 
install.packages("ROCR")
library(ROCR)
pred.svm.roc<-prediction(as.numeric(pred.svm),as.numeric(test[,1]))
plot(performance(pred.svm.roc,"tpr","fpr"))        # ROC Curve 작성
abline(a=0,b=1,lty=2,col="black")

performance(pred.svm.roc,"auc")@y.values

# tune.svm, svm 함수를 활용하여 SVM 분석 실시
library(e1071)
tune.svm(Species~.,
         data=iris,
         gamma=2^(-1:1),
         cost=2^(2:4))

svm.model2<-svm(Species~.,
                data=train.iris,
                kernel="radial",
                gamma=0.5,
                cost=16)

pred.svm2<-predict(svm.model2,test.iris,type="class")
confusionMatrix(daat=pred.svm2, reference=test.iris[,5],positive='1')

# naiveBayes 함수를 활용하여 나이브 베이즈 분류분석 실시
library(e1071)
nb.model<-naiveBayes(credit.rating~.,
                     data=train,
                     laplace=0)
nb.model

# 예측을 통한 정분류율 확인
install.packages("caret")
library(caret)
pred.nb<-predict(nb.model,test[,-1],type="class")
confusionMatrix(data=pred.nb, reference=test[,1], positive='1')

# ROC 커브 그리기 및 AUC 산출
install.packages("ROCR")
library(ROCR)
pred.nb.roc<-prediction(as.numeric(pred.nb),as.numeric(test[,1]))
plot(performance(pred.nb.roc,"tpr","fpr"))       # ROC Curve 작성
abline(a=0,b=1,lty=2,col="black")

performance(pred.nb.roc,"auc")@y.values

# knn 함수를 활용하여 K-NN 실시
library(class)
train.data<-train[,-1]                           # 분석을 위해 데이터셋의 종속변수(credit.rating) 제외
test.data<-test[,-1]
class<-train[,1]
knn.3<-knn(train.data,test.data,class,k=3)
knn.7<-knn(train.data,test.data,class,k=7)
knn.10<-knn(train.data,test.data,class,k=10)

# 각각의 k에 대한 분류 table 작성과 분류 정확도 확인
t.1<-table(knn.3,test$credit.rating)
t.1

t.2<-table(knn.7,test$credit.rating)
t.2

t.3<-table(knn.10,test$credit.rating)
t.3

# 분류를 가장 잘하는 최적의 k값을 찾기 위한 함수 구현
result<-numeric()
k=3:22
for(i in k) {
  pred<-knn(train.data,test.data,class,k=i-2)
  t<-table(pred,test$credit.rating)
  result[i-2]<-(t[1,1]+t[2,2])/sum(t)
}
result

sort(result,decreasing=T)

which(result==max(result))

# nnet 함수를 활용하여 인공신경망 분석 실시
library(nnet)
set.seed(1231)                               # 재현성을 위해 설정
nn.model<-nnet(credit.rating~.,              # 45개의 가중치가 주어졌고 iteration이 반복될수록 error이 줄고 있음.
               data=train,
               size=2,
               maxit=200,
               decay=5e-04)

summary(nn.model)

# github을 활용한 인공신경망 시각화와 변수 중요도 파악
install.packages("devtools")
library(devtools)
X11()
plot.nnet(nn.model)

install.packages("NeuralNetTools")
library(NeuralNetTools)
X11()
garson(nn.model)

# 예측을 통한 정분류율 확인
install.packages("caret")
library(caret)
pred.nn<-predict(nn.model,test[,-1],type="class")
confusionMatrix(data=as.factor(pred.nn),reference=test[,1],positive='1')

# ROC 커브 그리기 및 AUC 산출
install.packages("ROCR")
library(ROCR)
pred.nn.roc<-prediction(as.numeric(pred.nn),as.numeric(test[,1]))
plot(performance(pred.nb.roc,"tpr","fpr"))
abline(a=0,b=1,lty=2,col="black")

performance(pred.nn.roc,"auc")@y.values

# neuralnet 함수를 활용하여 인공신경망 분석 실시
library(neuralnet)
data(infert)
in.part<-createDataPartition(infert$case,
                             times=1,
                             p=0.7)
table(infert[in.part$Resample1,"case"])

parts<-as.vector(in.part$Resample1)
train.infert<-infert[parts,]
test.infert<-infert[-parts,]
nn.model2<-neuralnet(case~age+parity+induced+spontaneous,
                     data=train.infert,
                     hidden=c(2,2),
                     algorithm="rprop+",
                     threshold=0.01,
                     stepmax=1e+5)
plot(nn.model2)

names(nn.model2)

# compute 함수를 활용한 정분류율 확인
library(neuralnet)
set.seed(1231)                 # nnet 함수를 사용할 때, 실행했으면 실행 안해도 됨
test.infert$nn.model2_pred.prob <- compute(nn.model2, covariate=test.infert[,c(2:4:6)])$net.result
test.infert$nn.model2_pred <- ifelse(test.infert$nn.model2_pred.prob > 0.5, 1, 0)
confusionMatrix(as.factor(test.infert$nn.model2_pred), as.factor(test.infert[,5]))

# 3. 군집분석
# dist 함수를 활용하여 거리 구하기
US<-USArrests
US.dist<-dist(US,"euclidean")
US.dist

# hclust 함수를 활용하여 계층적 군집분석
US.single<-hclust(US.dist^2,method="single")
plot(US.single)

# 계층적 군집 결과 그룹 나누기와 덴드로그램 구분 짓기
group<-cutree(US.average,k=6)
group

plot(US.average)
rect.hclust(US.average,k=6,border="red")

# kmeans 함수를 활용하여 kmeans 군집분석 실시
train.data<-train[,-1]
credit.kmeans<-kmeans(train.data, centers=2)
credit.kmeans

kmeans.table<-table(train$credit.rating, credit.kmeans$cluster)
kmeans.table
(kmeans.table[1,1] + kmeans.table[2,2]) / sum(kmeans.table)

# NbClust 함수로 최적의 군집 수 찾기
install.packages("NbClust")
library(NbClust)
nc <- NbClust(train.data, min.nc=2, max.nc=15, method="kmeans")

# Mclust 함수를 활용하여 혼합 분포 군집분석 실시
install.packages("mclust")
library(mclust)
mc<-Mclust(iris[,1:4], G=3)

# plot.Mclust 함수를 활용하여 혼합 분포 군집분석 실시
plot.Mclust(mc)
mc$classification

# 4. 연관분석
# 데이터 입력 / as 함수를 활용한 데이터 변형 / 데이터 확인
install.packages("arules")
library(arules)
id <- c(1,2,3,4,5,6)
gender <- c("FEMALE", "MALE", "FEMALE", "FEMALE", "MALE", "FEMALE")
age <- c("age_20", "age_20", "age_40", "age_30", "age_40", "age_30")
rank <- c("Gold", "Silver", "Silver", "VIP", "Gold", "Gold")
mobile_app_use <- c("YES", "YES", "NO", "YES", "NO", "YES")
re_order <- c("YES", "NO", "NO", "YES", "NO", "YES")
cust_tel <- cbind(id, gender, age, rank, mobile_app_use, re_order)
cust_tel <- as.data.frame(cust_tel)
cust_tel_1 <- subset(cust_tel, select = -c(id))
cust_tel_1

tran.cust<-as(cust_tel_1, "transactions")
tran.cust

inspect(tran.cust)

# apriori 함수를 활용하여 연관규칙분석 실시
install.packages("arules")
library(arules)
data(Groceries)
Groceries                              # Groceries 데이터셋은 식료품 판매점의 1달 간 POS 데이터로 총 169개의 제품과
                                       # 9835건의 거래건수를 포함, 해당 데이터는 이미 transaction으로 변환되어 있음
inspect(Groceries[1:3])                # inspect 함수는 transaction 데이터와 연관규칙분석 결과를 확인하기 위한 함수

rules<-apriori(Groceries,
               parameter=list(support=0.01,
                              condidence=0.3))

# 연관규칙분석 결과 확인
inspect(sort(rules,by=c("confidence"), decreasing=T)[1:5])
# 중복가지치기 함수 구현
prune.dup.rules <- function(rules) {
  rule.subset.matrix <- is.subset(rules, rules, sparse=FALSE)
  rule.subset.matrix[lower.tri(rule.sebset.matrix, diag=T)] <- NA
  dup.rules <- colSums(rule.subset.matrix, na.rm=T) >= 1
  pruned.rules <- rules[!dup.rules]
  return(pruned.rules)
}

# 특정 규칙 찾기 : 우변의 아이템 구매를 이끌 아이템 찾기
metric.params <- list(supp=0.001, conf=0.5, minlen=2)           # minlen은 좌항과 우항을 합친 최소 물품수
rules<- apriori(data=Groceries, parameter=metric.params,
                appearance=list(default="lhs", rhs="soda"),
                control=list(verbose=F))
rules<-prune.dup.rules(rules)                                   # 중복 규칙 가지치기 실시
rules<-sort(rules, decreasing=TRUE, by="confidence")            # confidence를 기준으로 정렬
inspect(rules[1:5])

# 특정 규칙 찾기 : 좌변의 아이템 세트를 가지고 있을 때 물품 찾기
metric.params<-list(supp=0.001, conf=0.3, minlen=2)
rules<-apriori(data=Groceries, parameter=metric.params,
               appearance=list(default="rhs",
                               lhs=c("yogurt", "sugar")),
               control=list(verbose=F))
rules<-sort(rules, decreasing=TRUE, by="confidence")
inspect(rules[1:5])

