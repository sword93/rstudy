# http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/

# spam 분류기
# 로지스틱 회귀분석
# 트리모형
# 랜덤포레스트
# training : validation : test = 60:20:20

data <- read.csv("spambase.data", header = F, strip.white = T)
View(data)

names(data) = c('word_freq_make','word_freq_address','word_freq_all','word_freq_3d','word_freq_our'
                ,'word_freq_over','word_freq_remove','word_freq_internet','word_freq_order'
                ,'word_freq_mail','word_freq_receive','word_freq_will','word_freq_people'
                ,'word_freq_report','word_freq_addresses','word_freq_free','word_freq_business'
                ,'word_freq_email','word_freq_you','word_freq_credit','word_freq_your','word_freq_font'
                ,'word_freq_000','word_freq_money','word_freq_hp','word_freq_hpl','word_freq_george'
                ,'word_freq_650','word_freq_lab','word_freq_labs','word_freq_telnet','word_freq_857'
                ,'word_freq_data','word_freq_415','word_freq_85','word_freq_technology','word_freq_1999'
                ,'word_freq_parts','word_freq_pm','word_freq_direct','word_freq_cs','word_freq_meeting'
                ,'word_freq_original','word_freq_project','word_freq_re','word_freq_edu','word_freq_table'
                ,'word_freq_conference','char_freq_;','char_freq_(','char_freq_[','char_freq_!'
                ,'char_freq_$','char_freq_#','capital_run_length_average','capital_run_length_longest'
                ,'capital_run_length_total', 'class')

summary(data)
data$class <- as.factor(data$class)

glimpse(data)
summary(data$`char_freq_$`)


#####################################################################
# 1. 시각화를 이용한 데이터 확인

# 상관관계 확인
# 각 변수와 class 간의 상관계수 도출 : -1 < 상관계수 < 1
data[,-58]
unique(as.numeric(as.character(data$class)))
cor(data[,-58],as.numeric(as.character(data$class)))

tmp <- as.data.frame(cor(data[,-58],as.numeric(as.character(data$class))))
View(tmp)
tmp <- tmp %>%
        rename(cor = V1) # 컬럼명 변경
tmp$var <- rownames(tmp)
tmp %>%
  ggplot(aes(reorder(var, cor), cor)) + geom_point() + coord_flip()
# coord_flip : x축과 y축 변경
# 단변수 형태로 변경했을 때, class 요인을 결정하는 단어는 your, 000, remove, $ ... 순서
# (spam인 메일로 분류될 확률이 높은 순서)

# bar graph by class
p1 <- data %>%
        ggplot(aes(class)) + geom_bar()

# boxplot for '$' by class
p2 <- data %>%
        ggplot(aes(class, `char_freq_$`)) + geom_jitter(col = 'gray') + geom_boxplot(alpha = 0.5) + scale_y_sqrt()

# density graph for '$' by class
p3 <- data %>%
        ggplot(aes(`char_freq_$`, group = class, fill = class)) + geom_density(alpha = 0.5) + scale_x_sqrt() + scale_y_sqrt()

grid.arrange(p1, p2, p3, ncol = 2)

# make.names() : 컬럼명을 유효한 컬럼명으로 변경 (기존 컬럼명에 특수문자 등이 포함된 경우)
# char_freq_$ -> char_freq_..4
old_names <- names(data)
new_names <- make.names(old_names, unique = T)
cbind(old_names, new_names)[old_names != new_names,]
names(data) <- new_names
names(data)

# 변경된 컬럼명을 이용하여 다시 그래프 생성
# bar graph by class
p1 <- data %>%
  ggplot(aes(class)) + geom_bar()

# boxplot for '$' by class
p2 <- data %>%
  ggplot(aes(class, char_freq_..4)) + geom_jitter(col = 'gray') + geom_boxplot(alpha = 0.5) + scale_y_sqrt()

# density graph for '$' by class
p3 <- data %>%
  ggplot(aes(char_freq_..4, group = class, fill = class)) + geom_density(alpha = 0.5) + scale_x_sqrt() + scale_y_sqrt()

grid.arrange(p1, p2, p3, ncol = 2)


#####################################################################
# 2. 데이터셋 분류

set.seed(101)
n <- nrow(data)
idx <- 1:n

train_idx <- sample(idx, n*0.6)
idx <- setdiff(idx, train_idx)
valid_idx <- sample(idx, n*0.2)
test_idx <- setdiff(idx, valid_idx)

train_data <- data[train_idx,]
valid_data <- data[valid_idx,]
test_data <- data[test_idx,]

nrow(train_data)
nrow(valid_data)
nrow(test_data)


#####################################################################
# 3. logistic regression analysis

data_glm_full <- glm(class ~., family = binomial(), data = train_data)
summary(data_glm_full)

predict(data_glm_full, newdata = data[1:5,], type = "response")

y_obs <- ifelse(valid_data$class == 1,1,0)
y_obs
yhat_glm <- predict(data_glm_full, newdata = valid_data, type = "response")
yhat_glm
summary(yhat_glm)

pred_glm <- prediction(yhat_glm, y_obs)
perf_glm <- performance(pred_glm, measure = "tpr", x.measure = "fpr")
plot(perf_glm)
abline(0,1)


#####################################################################
# 4. Decision tree analysis

data_dt_full <- rpart(class ~., data = train_data)
summary(data_dt_full)

predict(data_dt_full, newdata = data[1:5,])

yhat_dt <- predict(data_dt_full, newdata = valid_data)
yhat_dt <- yhat_dt[,2]
summary(yhat_dt)

pred_dt <- prediction(yhat_dt, y_obs)
perf_dt <- performance(pred_dt, measure = "tpr", x.measure = "fpr")
plot(perf_dt)
abline(0,1)


#####################################################################
# 5. random forest analysis

set.seed(110)
data_rf_full <- randomForest(class ~., data = train_data)
summary(data_rf_full)

opar <- par(no.readonly = T)
par(mfrow=c(1,2))
plot(data_rf_full)
varImpPlot(data_rf_full)
par(opar)

predict(data_rf_full, newdata = data[1:5,])

yhat_rf <- predict(data_rf_full, newdata = valid_data)
summary(yhat_rf)
yhat_rf <- predict(data_rf_full, newdata = valid_data, type = "prob")
yhat_rf <- yhat_rf[,2]

pred_rf <- prediction(yhat_rf, y_obs)
perf_rf <- performance(pred_rf, measure = "tpr", x.measure = "fpr")
plot(perf_rf)
abline(0,1)


#####################################################################
# 6. 모델 성능평가 (model estimation)

plot(perf_glm)
plot(perf_dt, col = "blue", add = T)
plot(perf_rf, col = "red", add = T)
abline(0,1)

performance(pred_glm, "auc")@y.values[[1]] # black : 0.9776835
performance(pred_dt, "auc")@y.values[[1]] # blue : 0.9080126
performance(pred_rf, "auc")@y.values[[1]] # red : 0.9907478



