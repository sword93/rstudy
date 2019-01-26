# 다음은, 유라시아 횡단열차에 대한 승객들의 리스트와 열차 내 기념 상품 구매여부이다.
# 유라시아 횡단열차는 부산을 출발해 블라디보스토크와 하얼빈에서 정차를 한 후
# 승객을 추가로 태우고 유럽으로 여행을 떠나는 열차이다.
# 이 열차 내부에서는 유라시아 횡단열차 개통 기념으로 기념상품을 판매하고 있다.
# 한중러 공통 운영본부는 이 기념상품에 대한 판매 패턴을 파악한 후,
# 추가적인 상품기획에 이용할 예정이다.
# 데이터분석가인 당신은 이를 위해 데이터를 분석하고자 한다.

# train_info.txt : 훈련을 위한 고객 테이터 (탭 구분)
# train_label.csv : 해당 고객의 상품 구매여부
# test_info.txt : 예측을 위한 고객 고객 데이터 (탭 구분)
# test_lavel.csv : 예측 평가를 위한 고객 데이터

info_class <- c("numeric",rep("character",2),rep("numeric",2),"character","numeric",
                (rep("character",2)))
label_class <- c("numeric", "character")


train_info <- read.delim("train_info.txt", header = T, sep = "\t", stringsAsFactors = F)
# 위와 동일한 결과
# train_info <- read.delim("train_info.txt", header = T, sep = "\t", as.is = T)
# train_info <- read.delim("train_info.txt", header = T, sep = "\t", colClasses = info_class)

train_label <- read.csv("train_label.csv", header = T, stringsAsFactors = F)
train_label <- read.csv("train_label.csv", header = T, colClasses = label_class)

View(train_info)
View(train_label)

glimpse(train_info)
glimpse(train_label)

summary(train_info)
summary(train_label)


# 1. 데이터 분석을 시행하기 위해 간단한 탐색을 해보고자 한다.
# train_label 파일과 train_info 파일간passengerId를 기준으로 데이터를 조인하고
# (train_lavel과 train_info 두 데이터에 passengerid가 모두 전재하는 경우만 처리)
# 나이(age), 열차구매가격(fare) 컬럼 각각에 대해 결측치가 몇 개 인지 차례대로 답하시오

train <- merge(train_info, train_label, by = "passengerId")
View(train)
glimpse(train)
summary(train)

# 결측치 : age 192건, fare 없음


# 2. 변수들끼리 영향을 미치는지 확인하기 위해 age와 fare간 상관관계가 있는지 확인해보고자 한다.
# 열차구매가격(fare), 나이(age) 컬럼에 대해 결측치를 제거한 후 (결측치가 존재한다면)
# 열차구매가격(fare) 컬럼에 대한 데이터 중 음수를 제거한 후,
# 나이(age) 100살 이상이나 음수를 제거하고
# 상관계수를 (Pearson)를 구하시오 (소수점 넷째 자리 미만은 버림)

# 유효한 데이터의 인덱스만 뽑아서 구함
idx <- which(!is.na(train$fare) &
             !is.na(train$age) &
             train$fare >= 0 &
             (train$age >= 0 & train$age <= 100))
cor(train$fare[idx], train$age[idx])


# 3. 열차 상품 구매여부에 로지스틱 회귀분석을 시행해보고자 한다.
# 회귀분석의 종속변수(Y)인 구매여부(purchased)는 Y는 1로, N은 0으로 변환하여 사용하며,
# 독립변수(X)는 범주형 변후인 성별(sex), 좌석등급(class), 그리고 수치형 변수인 나이(age)이다.
# 먼저 (2)에서 진행한 것처럼,
# 나이(age) 100살 이상이나 음수로 표기되어 있는
# 아웃라이어(Outlier) 부분은 제거(0은 제거하지 않는다)하고
# 결측치 또한 제거한다. 또한 독립변수 중 범주형 변수를
# 원핫인코딩(one-hot encoding) 기법을 통해 변환한다.
# 훈련데이터를 대상으로 로지스틱 회귀분석 모델을 구성한 후,
# 나이(age) 변수에 대한 계수(Coefficient)를 구하시오.
# - 소수점 넷째 자리 미만은 버림

# length(idx)
# train_q3 <- train[idx,]
# View(train_q3)
# 
# train_q3$purch_fg <- ifelse(train_q3$purchased == 'Y', 1,0)
# sex <- dummy.code(train_q3$sex)
# class <- dummy.code(train_q3$class)
# train_q3_dummy <- data.frame(sex, class, age = train_q3$age, puchased = train_q3$purch_fg)
# head(train_q3_dummy)
# 
# names(train_q3)
# train_q3 <- train_q3[, c("passengerId","class","sex","age","purch_fg")]
# 
# train_q3_glm <- glm(puchased ~ ., data = train_q3_dummy, family = binomial())
# summary(train_q3_glm)
# floor(coef(train_q3_glm) * 10000) / 10000

# dummies 패키지를 이용하여 변환
length(idx)
train_q3 <- train[idx,]
View(train_q3)

train_q3$purch_fg <- ifelse(train_q3$purchased == 'Y', 1,0)
train_q3$purch_fg <- as.factor(train_q3$purch_fg)

library("dummies")
train_q3_dummy2 <- dummy.data.frame(train_q3[,c("sex","class","age")],sep = "_")
train_q3_dummy2 <- data.frame(train_q3_dummy2, purch_fg = train_q3$purch_fg)
head(train_q3_dummy2)

train_q3_glm <- glm(purch_fg ~ ., data = train_q3_dummy2, family = binomial())
summary(train_q3_glm)
floor(coef(train_q3_glm) * 10000) / 10000

# 4. (3)에서 완성한 로지스틱 회귀분석 모델을 대상으로 테스트 데이터를 모델을 검증하고자 한다.
# 주어진 테스트 데이터에 대해 (test_info.csv, test_label,csv) 조인(inner join)을 시행하고,
# 새로운 예측데이터에 대한 예측을 하라.
# 모델에 대한 평가를 하기 위한 predict 값에 대해 0.5 이상이면
# 해당 손님이 기념품을 구매했다고 가정하고,
# 재현율(Recall)을 구하시오.
# - 소수점 넷째 자리 미만은 버림
# - 재현율(Recall)은 실제 양성(Positive)인 것 중
# 예측결과도 양성(Positive)이라고 올바르게 예측한 비율을 의미

test_info <- read.delim("test_info.txt", header = T, sep = "\t", stringsAsFactors = F)
test_label <- read.csv("test_label.csv", header = T, stringsAsFactors = F)

test <- merge(test_info, test_label, by = "passengerId")
View(test)
glimpse(test)
summary(test)

test_q4 <- test
test_q4$purch_fg <- ifelse(test_q4$purchased == 'Y', 1,0)
test_q4$purch_fg <- as.factor(test_q4$purch_fg)
View(test_q4)
summary(test_q4)

test_q4_dummy <- dummy.data.frame(test_q4[,c("sex","class","age")],sep = "_")
test_q4_dummy <- data.frame(test_q4_dummy, purch_fg = test_q4$purch_fg)

test_pred <- predict(train_q3_glm, newdata = test_q4_dummy, type = "response")
summary(test_pred)

tbl <- table(pred = as.numeric(test_pred > 0.5),
            real = test_q4_dummy$purch_fg)
tbl[2,2] / sum(tbl[,2]) # Recall(재현율)



