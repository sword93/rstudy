# http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/

wine_red <- read.csv("winequality-red.csv", header = T, sep = ";")
View(wine_red)

wine_white <- read.csv("winequality-white.csv", header = T, sep = ";")
View(wine_white)

glimpse(wine_red)
glimpse(wine_white)
summary(wine_red)
summary(wine_white)

# 시각화
# bar (x:quality)
wine_red %>%
  ggplot(aes(quality)) + geom_bar()

plt1 <- wine_white %>%
  ggplot(aes(quality)) + geom_bar()

# boxplot (x:quality, y:alcohol)
wine_red %>%
  ggplot(aes(as.factor(quality), alcohol)) + geom_boxplot()

plt2 <- wine_white %>%
  ggplot(aes(as.factor(quality), alcohol)) + geom_boxplot()

# boxplot (x:quality, y:density)
wine_red %>%
  ggplot(aes(as.factor(quality), density)) + geom_boxplot()

plt3 <- wine_white %>%
  ggplot(aes(as.factor(quality), density)) + geom_boxplot()

# 산점도 (x:alcohol, y:density)
wine_red %>%
  ggplot(aes(alcohol, density)) + geom_point(alpha = 0.1)

plt4 <- wine_white %>%
  ggplot(aes(alcohol, density)) + geom_point(alpha = 0.1)

# 산점도에 데이터 경향을 보여주는 그래프 추가
wine_white %>%
  ggplot(aes(alcohol, density)) + geom_point(alpha = 0.1) + geom_smooth()

# gridExtra 패키지 설치
# 여러 개의 그래프를 그리드 형태로 출력해주는 함수
grid.arrange(plt1, plt2, plt3, plt4, ncol = 2)

# 훈련/검증/평가 데이터셋 구성 (60:20:20)
set.seed(110)
n <- nrow(wine_white)
idx <- 1:n

training_idx <- sample(idx, n*0.6)
idx <- setdiff(idx, training_idx)
valid_idx <- sample(idx, n*0.2)
test_idx <- setdiff(idx, valid_idx)

length(training_idx)
length(valid_idx)
length(test_idx)

train_data <- wine_white[training_idx,]
valid_data <- wine_white[valid_idx,]
test_data <- wine_white[test_idx,]

nrow(train_data) # 2938
nrow(valid_data) # 979
nrow(test_data) # 981

# 선형회귀모형 적합(fit)
wine_white_lm_full <- lm(quality ~ ., data = train_data)
summary(wine_white_lm_full)
length(coef(wine_white_lm_full))

wine_white_lm_full2 <- lm(quality ~ .^2, data = train_data)
summary(wine_white_lm_full2)
length(coef(wine_white_lm_full2))
# 설명변수의 차원을 늘렸지만, 모델의 설명력이 그렇게 커지지 않음

# MASS library 로딩 -> stepwise 알고리즘 이용
library("MASS")

wine_white_step <- stepAIC(wine_white_lm_full, scope = list(upper = ~.^2, lower = ~1))
summary(wine_white_step)
length(coef(wine_white_step)) # stepwise 결과 변수 개수가 67 -> 34로 줄어듦

rmse <- function(y, f){
  sqrt(mean((y-f)^2))
}

y_obs <- valid_data$quality
yhat_lm <- predict(wine_white_lm_full, newdata = valid_data) 
yhat_lm2 <- predict(wine_white_lm_full2, newdata = valid_data) 
yhat_step <- predict(wine_white_step, newdata = valid_data)

rmse(y_obs, yhat_lm)
rmse(y_obs, yhat_lm2)
rmse(y_obs, yhat_step)


