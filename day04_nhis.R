info <- read.csv("NHIS_2016_info.csv", header = T, strip.white = F, stringsAsFactors = F)
result <- read.csv("NHIS_2016_result.csv", header = T, strip.white = F, stringsAsFactors = F)

View(info)
View(result)

summary(info)
summary(result)

# base_table 생성 (key : IDV_ID)
base <- merge(info, result, by = c("HCHK_YEAR", "IDV_ID"))
View(base)
summary(base)
nrow(base)

summary(base$AGE_GROUP)
quantile(base$AGE_GROUP)

# 연령코드 변환 (5세 단위 -> 10세 단위)
floor((base$AGE_GROUP*5-1)/10) * 10 # 해보기

base$AGE_GROUP <- floor((base$AGE_GROUP*5-1)/10) * 10
base$AGE_GROUP

# 키(HEIGHT), 수축기혈압(BP_HIGH), 이완기혈압(BP_LWST),
# HDL 콜레스테롤(HDL_CHOLE), LDL 콜레스테롤(LDL_CHOLE),
# 흡연상태(SMK_STAT_TYPE_CD)에 대해 결측치 제거 : 27명
idx <- which(is.na(base$HEIGHT) |
             is.na(base$BP_HIGH) |
             is.na(base$BP_LWST) |
             is.na(base$HDL_CHOLE) |
             is.na(base$LDL_CHOLE) |
             is.na(base$SMK_STAT_TYPE_CD))
length(idx)
base <- base[-idx,]

glimpse(base)

# 문제1. 30대 남성의 수축기 혈압(BP_HIGH)의 이상치 개수는?
# 이상치 : 이상치 < 평균 - 3*표준편차 or 이상치 > 평균 + 3*표준편차

summary(base$AGE_GROUP)
glimpse(base$AGE_GROUP)

base_q1_idx <- which(base$SEX == "M" & base$AGE_GROUP == 30)

length(base_q1_idx)
base_q1 <- base[base_q1_idx,]

# base_age30_2 <- base[base$AGE_GROUP == 30,]
# base_age30_2 <- base_age30_2[base_age30_2$SEX == "M",]

nrow(base_q1) # 1088
View(base_q1)

round(mean(base_age30$BP_HIGH),2)
round(sd(base_age30$BP_HIGH),2)

mean(base_age30$BP_HIGH)
mean(base_age30$BP_HIGH) * 100
floor(mean(base_age30$BP_HIGH)*100)/100
floor(sd(base_age30$BP_HIGH)*100)/100

mean <- floor(mean(base_age30$BP_HIGH)*100)/100 # 123.33
sd <- floor(sd(base_age30$BP_HIGH)*100)/100 # 12.35

outlier_idx <- which(base_q1$BP_HIGH < mean - 3 * sd |
                       base_q1$BP_HIGH > mean + 3 * sd)
length(outlier_idx) # 5

# 문제2. 성인 남성의 “저HDL-C혈증”과 흡연상태(SMK_STAT_TYPE_CD)의 연관성을
# 카이제곱 검정을 사용해 분석한 뒤, p_value를 확인하시오
# (소수점 넷째자리까지 표기, 소수점 다섯째자리에서 반올림)
# “저HDL-C혈증”은 HDL 콜레스테롤(HDL_CHOLE) 값이 40m/dL 미만인 상태를 의미힌다.
# HDL 콜레스테롤(HDL_CHOLE) 값이 40 이상인 값을 0(정상),
# 40 미만인 값을 1(병이 있음) 처리한 컬럼을 추가하여 처리하시오.

base_q2_idx <- which(base$SEX == "M" & base$AGE_GROUP >= 20)
length(base_q2_idx)
base_q2 <- base[base_q2_idx,]

nrow(base_q2) # 5373
View(base_q2)

base_q2[,c("AGE_GROUP", "SMK_STAT_TYPE_CD", "HDL_CHOLE")]
base_q2$HDL_CHOLE_FG <- ifelse(base_q2$HDL_CHOLE >= 40, 0, 1)
# base_q2$HDL_CHOLE_FG <- factor(as.numeric(base_q2$HDL_CHOLE < 40), levels = c(0, 1))
base_q2[,c("AGE_GROUP", "SMK_STAT_TYPE_CD", "HDL_CHOLE", "HDL_CHOLE_FG")]

# 범주형 데이터이므로 factor 처리
base_q2$SMK_STAT_TYPE_CD <- as.factor(base_q2$SMK_STAT_TYPE_CD)
base_q2$HDL_CHOLE_FG <- as.factor(base_q2$HDL_CHOLE_FG)
glimpse(base_q2$SMK_STAT_TYPE_CD)
glimpse(base_q2$HDL_CHOLE_FG)
summary(base_q2)

# 분할표
table(x = base_q2$SMK_STAT_TYPE_CD, y = base_q2$HDL_CHOLE_FG)

# 카이제곱검정
chisq.test(x = base_q2$SMK_STAT_TYPE_CD, y = base_q2$HDL_CHOLE_FG)
# Ho : 흡연상태와 저HDL-C 혈증은 서로 독립적이다
# H1 : 흡연상태와 저HDL-C 혈증은 서로 독립적이지 않다
# p-value = 0.0002987 -> reject H0

# 문제3. 성별(SEX)을 더미변수로 바꾼 뒤 키(HEIGHT)와 몸무게(WEIGHT) 대신 사용할
# BMI 데이터를 추가하시오 (BMI = 몸무게(kg)/키(m)의 제곱)
# 가입자 일련번호 9000번 이하의 데이터를 기준으로
# 선형회귀분석(Linear Regreesion)을 하여 9001번 이상의 허리둘레 데이터를 예측
# 예측 결과값의 평균과 분산을 구하시오

base_q3 <- base
View(base_q3)

# 성별 컬럼 이용한 더미변수 추가
base_q3 <- data.frame(base_q3, dummy.code(base_q3$SEX))
glimpse(base_q3)
summary(base_q3)

# WAIST 값 NA인 데이터
base_q3[is.na(base_q3$WAIST),]
range(base_q3[is.na(base_q3$WAIST),"IDV_ID"]) # 9001 부터 10000번까지

# 결측치값 제거
base_q3[is.na(base_q3$SIGHT_LEFT),c("SIGHT_LEFT","SIGHT_RIGHT","HEAR_LEFT","HEAR_RIGHT"),]

na_idx <- which(is.na(base_q3$SIGHT_LEFT) |
                is.na(base_q3$SIGHT_RIGHT) |
                is.na(base_q3$HEAR_LEFT) |
                is.na(base_q3$HEAR_RIGHT))
base_q3 <- base_q3[-na_idx,]
nrow(base_q3) # 9973 -> 9972

# BMI 컬럼 추가
base_q3$BMI <- base_q3$WEIGHT / (base_q3$HEIGHT/100)^2
glimpse(base_q3)

# 데이터셋 분할
train <- base_q3[base_q3$IDV_ID <= 9000,]
test <- base_q3[base_q3$IDV_ID > 9000,]

glimpse(train)
summary(test)

# train 데이터 상관계수 측정, 상관계수는 수치형 데이터 간의 분석
#train <- train[,-c(6,7)] # HEIGHT, WEIGHT 컬럼 제거
n_col <- ncol(train)
cor(train[,4:n_col])[,"WAIST"]

# 상관계수가 0.1 이상인 변수 도출
cor_idx_col <- which(abs(cor(train[,4:n_col])[,"WAIST"]) >= 0.1)
names(train[,4:n_col])[cor_idx_col]

train_2 <- train[,4:ncol(train)][cor_idx_col]
train_2 <- train_2[,-(1:2)]
glimpse(train_2)

train_2_lm <- lm(WAIST ~ ., data = train_2)
summary(train_2_lm)

test_2 <- test[,4:n_col][cor_idx_col]
test_2 <- test_2[,-(1:2)]
glimpse(test_2)

pred <- predict(train_2_lm, test_2)
round(mean(pred), 2) # 81.19
round(var(pred), 2) # 63.21

# 문제4. 검진자들의 흡연무경험여부가 HDL콜레스테롤(HDL_CHOLE)에 의해 영향을 받는지
# 로지스틱 회귀(Logistic Regression) 모형으로 분석
# 로지스틱 회귀모형에서 HDL콜레스테롤(HDL_CHOLE)의 오즈비(odds ration)가 무엇인지 구하시오.

base_q4 <- base_q3
base_q4$SMK_FG <- ifelse(base_q4$SMK_STAT_TYPE_CD == 1, 1, 0)
base_q4$SMK_FG <- as.factor(base_q4$SMK_FG)
glimpse(base_q4$SMK_FG)

base_q4_glm <- glm(SMK_FG ~ HDL_CHOLE, data = base_q4, family = binomial())
summary(base_q4_glm)

coef(base_q4_glm)
exp(coef(base_q4_glm))
round(exp(coef(base_q4_glm)),2)[2] # 1.03

######################################################################
# base_q4$SMK_EX_CD <- ifelse(base_q4$SMK_STAT_TYPE_CD == 1, 1, 0)
# SMK_FG <- dummy.code(base_q4$SMK_EX_CD)
# base_q4 <- data.frame(base_q4, SMK_FG)
# base_q4_glm_x0 <- glm(X0 ~ HDL_CHOLE, data = base_q4, family = binomial())
# base_q4_glm_x1 <- glm(X1 ~ HDL_CHOLE, data = base_q4, family = binomial())
#
# round(exp(coef(base_q4_glm_x1))[2],2)
# round(exp(coef(base_q4_glm_x2))[2],2)
######################################################################