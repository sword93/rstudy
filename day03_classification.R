# http://archive.ics.uci.edu/ml/machine-learning-databases/adult/

adult <- read.csv("adult.data", header = F, strip.white = T)
View(adult)

glimpse(adult)
str(adult)

names(adult) = c("age","workclass","fnlwgt","education",
                 "education_num","marital_status","occupation","relationship",
                 "race","sex","capital_gain","capital_loss","hours_per_week",
                 "native_country","wage")

# 로지스틱 회귀분석(glm) : 실패(0)/성공(1)
# 데이터에 대한 민감도가 크지만
# 분류모델로도 사용 가능하고 선형회귀모델로도 사용 가능하다는 장점이 있음

# "<=50K" : 중산층이 아니다
# ">50K" : 중산층이다
summary(adult)
levels(adult$wage)
levels(adult$native_country)
levels(adult$workclass)

# factor 유형의 범주 데이터가 어떻게 다루어지는지
x <- model.matrix( ~ . -wage, adult)
glimpse(x)
dim(x)

# idx 구성 시 아래 전체 블록 지정 후 실행
set.seed(1601)
n <- nrow(adult)
idx <- 1:n
# 데이터를 60(train):20(validation):20(test)으로 분할
training_idx <- sample(idx, n * 0.6) # training data index
idx <- setdiff(idx, training_idx) # training data index 제외한 나머지로 다시 idx 구성
validation_idx <- sample(idx, n * 0.2) 
test_idx <- setdiff(idx, validation_idx)

length(training_idx) #19536
length(validation_idx) #6512
length(test_idx) #6513

# 랜덤으로 셋업한 인덱스 이용하여 각 데이터셋 구성
training <- adult[training_idx,]
validation <- adult[validation_idx,]
test <- adult[test_idx,]

# 데이터 시각화 확인
training %>%
  ggplot(aes(age)) + geom_bar()
training %>%
  ggplot(aes(age)) + geom_density()
# wage별로 두 개의 그래프 생성, alpha로 투명도 조정
training %>%
  ggplot(aes(age, fill = wage)) + geom_density(alpha = 0.5)
# 연령이 증가할수록 wage가 계속 증가하는 것이 아니라, 증가했다가 감소 -> 데이터가 비선형 모델
# 데이터가 비선형 모델인 경우 로지스틱 회귀분석은 적합하지 않음, 랜덤포레스트 모델이 더 적합

training %>%
  filter(race %in% c("Black","White")) %>%
    ggplot(aes(age, fill = wage)) + geom_density(alpha = 0.5) + facet_grid(race ~ sex)
# male의 경우 인종간 차이가 많이 나타나지 않음
# female의 경우 차이가 존재

training %>%
  ggplot(aes(education_num, fill = wage)) + geom_bar()

# glm 생성
ad_glm_full <- glm(wage ~ ., family = binomial(), data = training)
summary(ad_glm_full)
# 정석대로 하면 모델 생성하기 전에 각 독립변수간 상관성이 있는지를 먼저 검증하고,
# 상관성이 있는 변수들은 제거하는 작업이 먼저 필요함
# glm 결과 중 유의한 변수만을 추려서 모형 만들면 됨

# 생성된 모형을 이용한 예측
predict(ad_glm_full, newdata = adult[1:5,], type = "response")

y_obs <- ifelse(validation$wage == ">50K", 1, 0) # 실제값
y_obs
yhat_lm <- predict(ad_glm_full, newdata = validation, type = "response") # 예측값
yhat_lm
summary(yhat_lm)

pred_lm <- prediction(yhat_lm, y_obs)
perf_lm <- performance(pred_lm, measure = "tpr", x.measure = "fpr") # 순서 : y, x

performance(pred_lm, "auc")@y.values[[1]]
attributes(performance(pred_lm, "auc"))$y.values[[1]] # 0.90

plot(perf_lm)
abline(0,1)


#######################################################################
# gridExtra 이용해서 그래프 그리드 형태로 출력

p1 <- training %>%
        ggplot(aes(age)) + geom_bar()

p2 <- training %>%
        filter(race %in% c("Black","White")) %>%
          ggplot(aes(age, fill = wage)) + geom_density(alpha = 0.5) + facet_grid(race ~ sex)

p3 <- training %>%
        ggplot(aes(education_num, fill = wage)) + geom_bar()

grid.arrange(p1,p2,p3)

#######################################################################
# rpart 패키지 -> 트리모형 생성

ad_tr <- rpart(wage ~., data = training)
ad_tr
printcp(ad_tr) # tree를 구성한 변수가 뭐가 있는지 확인
summary(ad_tr)
plot(ad_tr) # tree 구조 확인
text(ad_tr) # tree 텍스트 확인
text(ad_tr, use.n = T) # tree 분기점 수치 확인

yhat_ad_tr <- predict(tr, validation)
yhat_ad_tr <- yhat_ad_tr[, ">50K"] # 중산층일 확률 (성공의 확률)
yhat_ad_tr

pred_ad_tr <- prediction(yhat_ad_tr, y_obs)
pred_ad_tr

perf_ad_tr <- performance(pred_ad_tr, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col = "red")
plot(perf_ad_tr, col = "blue", add = "T")
abline(0,1)

attributes(performance(pred_tr, "auc"))$y.values[[1]] # 0.84
# glm 모델보다 tree 모델이 예측력이 더 낮음
# but 데이터에 민감하지 않다는 장점 있음
# tree 모델의 전제조건 : 1개 factor에 대한 레벨 갯수가 32 이하여야 함

#######################################################################
# randomForest 패키시 설치 후 로딩
# random이기 때문에 set.seed() 필요
# DS 인증 시험 시, random forest 모델에서 가장 중요한 변수가 어떤 것인지 물어보는 문제

set.seed(1607)
ad_rf <- randomForest(wage ~., data = training)
ad_rf
plot(ad_rf) # 100개 tree일때부터 오차 감소율이 거의 없음, 위아래는 error range
# 변수 중요도 확인 (Gini index 이용 : 0  < p(1-p) < 0.25)
# Gini index가 작은 쪽으로 유도하는 모델이 좋은 모델
mdg <- importance(ad_rf) # MeanDecreaseGini(지니계수 평균 감소량)
str(mdg)
mdg[order(-mdg[,1]),1, drop = F] # drop : matrix 구조를 깨지 않기 위해 사용
varImpPlot(ad_rf) # 변수 중요도 plot

yhat_rf <- predict(ad_rf, newdata = validation)
summary(yhat_rf)
yhat_rf # 관측치와 분류값 : decision tree와의 차이 있음, decision tree는 확률값 출력
# decision tree처럼 확률값 반환하고 싶으면 type = "prob" 추가
yhat_rf <- predict(ad_rf, newdata = validation, type = "prob")
yhat_rf <- yhat_rf[,">50K"]
yhat_rf # 중산층일 확률값 출력

pred_rf <- prediction(yhat_rf, y_obs)
perf_rf <- performance(pred_rf, measure = "tpr", x.measure = "fpr")
plot(perf_rf, col = "blue")
plot(perf_tr, col = "red", add = T)
plot(perf_lm, add = T)
abline(0,1)

performance(pred_rf, "auc")@y.values[[1]] # 0.91




