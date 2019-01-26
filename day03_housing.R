# http://archive.ics.uci.edu/ml/machine-learning-databases/housing/

hs <- read.table("housing.data", header = F, sep = '', strip.white = F)
View(hs)

names(hs) <- c('crim','zn','indus','chas','nox',
               'rm','age','dis','rad','tax',
               'ptratio','b','lstat','medv')
summary(hs)
glimpse(hs)

# 범주형 데이터
# hs$CHAS <- as.factor(hs$CHAS)
# str(hs$CHAS)

# 동일한 결과로 데이터 로딩 가능
hs2 <- read.delim("housing.data", header = F, sep = "")
names(hs2) <- c('crim','zn','indus','chas','nox',
               'rm','age','dis','rad','tax',
               'ptratio','b','lstat','medv')
View(hs2)

# 데이터 처리 순서
# 1. 결측치 처리 : is.na()
# 2. 이상치 처리 : q1, q3, IQR
# 3. 데이터 정규화 : (mead - feature ) / sd 또는 log10 변환
# 4. 시각화 : boxplot (이상치 존재여부 확인 가능), hist, bar, density, scatterplot
# 5. 훈련, 검증, 테스트 데이터 셋 구성 : 70/30, 80/20, k-fold (k=5, 60/20/20)
# 6. 모델 생성
# 7. 예측
# 8. 모델 평가

# 데이터 정규화
mean_crim <- mean(hs$crim)
sd_crim <- sd(hs$crim)
hs$crim_norm <- (hs$crim - mean_crim) / sd_crim
summary(hs$crim_norm)


# train : test = 70 : 30
set.seed(10)
n <- nrow(hs)
idx <- 1:n
training_idx <- sample(idx, n*0.7)
test_idx <- setdiff(idx, training_idx)

length(training_idx) # 354
length(test_idx) # 152

training <- hs[training_idx,]
test <- hs[test_idx,]

summary(training)

# 모델 생성 : lm(선형회귀)
hs_lm_full <- lm(medv ~ ., data = training)
summary(hs_lm_full) # 모델에 대한 p-value, R-squared / 각 변수 p-value 값 확인
coef(hs_lm_full) # 각 변수의 계수만 확인

# 예측
predict(hs_lm_full, newdata = hs[1:5,])

# 평가 : RMSE -> 값이 작을수록 좋음
# rmse 함수 생성
rmse <- function(yi, yhat_i){
  sqrt(mean(yi - yhat_i)^2)
}

y_obs <- test$medv
yhat_lm <- predict(hs_lm_full, newdata = test)

rmse(y_obs, yhat_lm)

# 모델 복잡도 증가 : degree를 높임
hs_lm_full2 <- lm(medv ~ .^2, data = training)
summary(hs_lm_full2)

length(coef(hs_lm_full))
length(coef(hs_lm_full2)) # overfitting 모델일 가능성이 높음, adjstd r-squared 0.88

library(MASS)
# 모델에서 중요한 변수를 자동으로 선택 : stepwise 알고리즘 사용
hs_step <- stepAIC(hs_lm_full, scope = list(upper = ~ .^2, lower = ~1))
summary(hs_step)
length(coef(hs_step))

yhat_lm <- predict(hs_lm_full, newdata = test)
yhat_lm2 <- predict(hs_lm_full2, newdata = test)
yhat_step <- predict(hs_step, newdata = test)

rmse(y_obs, yhat_lm)
rmse(y_obs, yhat_lm2)
rmse(y_obs, yhat_step) # rmse가 stepwise 모델 사용 시 가장 작아야 정상



