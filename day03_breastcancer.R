# http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/

# Class distribution(Diagnosis): 357 benign, 212 malignant
# B : 양성 / M : 악성

bc <- read.csv("wdbc.data", header = F)
View(bc)
str(bc)

# c("radius","texture","perimeter","area","smoothness",
#  "compactness","concavity","concave_points","symmetry","fractal_dimension")
#
# paste0("Worst_", c("radius","texture","perimeter","area","smoothness",
#                 "compactness","concavity","concave_points","symmetry","fractal_dimension"))

feature_names <- c("radius","texture","perimeter","area","smoothness",
                   "compactness","concavity","concave_points","symmetry","fractal_dimension")

names(bc) <- c('id','class',
               paste0('mean_',feature_names),
               paste0('se_',feature_names),
               paste0('worst_',feature_names))
glimpse(bc)
summary(bc)

# class의 B, M을 0, 1로 변경
levels(bc$class)
bc$class <- factor(ifelse(bc$class == "B", 0, 1))

# id 컬럼 제거
bc <- bc %>%
        select(-id)
glimpse(bc)

# 시각화로 데이터 확인
# bar graph by class
bc %>%
  ggplot(aes(class)) + geom_bar()

# boxplot by class(x), mean_radius(y)
bc %>%
  ggplot(aes(class, mean_radius)) + geom_jitter(color = "grey") + geom_boxplot(alpha = 0.5)

# 전체 데이터 q1, q3, iqr
q1 <- quantile(bc$mean_radius, 0.25)
q3 <- quantile(bc$mean_radius, 0.75)
iqr <- IQR(bc$mean_radius)

bc$mean_radius[bc$mean_radius < q1 - 1.5*iqr]
bc$mean_radius[bc$mean_radius > q3 + 1.5*iqr]

bc$mean_radius[bc$class == 0]
bc$mean_radius[bc$class == 1]

length(bc$mean_radius[bc$class == 0]) # 357
length(bc$mean_radius[bc$class == 1]) # 212

# class 0일 때 q1, q3, iqr
q1_0 <- quantile(bc$mean_radius[bc$class == 0], 0.25)
q3_0 <- quantile(bc$mean_radius[bc$class == 0], 0.75)
iqr_0 <- IQR(bc$mean_radius[bc$class == 0])

bc$mean_radius[bc$class == 0][bc$mean_radius[bc$class == 0] < q1_0 - 1.5*iqr_0]
bc$mean_radius[bc$class == 0][bc$mean_radius[bc$class == 0] > q3_0 + 1.5*iqr_0]

# class 1일 때 q1, q3, iqr
q1_1 <- quantile(bc$mean_radius[bc$class == 1], 0.25)
q3_1 <- quantile(bc$mean_radius[bc$class == 1], 0.75)
iqr_1 <- IQR(bc$mean_radius[bc$class == 1])

bc$mean_radius[bc$class == 1][bc$mean_radius[bc$class == 1] < q1_1 - 1.5*iqr_1]
bc$mean_radius[bc$class == 1][bc$mean_radius[bc$class == 1] > q3_1 + 1.5*iqr_1]

summary(bc$mean_radius)

# train : test = 70 : 30
n <- nrow(bc)
idx <- 1:n
training_idx <- sample(idx, n*0.7)
test_idx <- setdiff(idx, training_idx)

length(training_idx) # 398
length(test_idx) # 171

training <- bc[training_idx,]
test <- bc[test_idx,]

summary(training)


bc_lm_full <- glm(class ~ ., family = binomial(), data = training)
# 데이터 전처리 과정 중 수치데이터에 대한 scale 조정이 되지 않아 모든 변수가 유의하지 않은 것처럼 보임
summary(bc_lm_full)
predict(bc_lm_full, newdata = bc[1:5,], type = "response")

test$class
# factor를 numeric vector로 변경 시 character로 변경 후 다시 numeric으로 변경
y_obs <- as.numeric(as.character(test$class))
y_obs
yhat_lm <- predict(bc_lm_full, newdata = test, type = "response")
yhat_lm
pred_lm <- prediction(yhat_lm, y_obs)
performance(pred_lm, "auc")@y.values[[1]] # 예측능력 확인






