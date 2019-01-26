# 다음은 2009년 프로야구선수(투수)의 성적과
# 해당 선수의 2009년, 2010년의 연붕정보에 대한 데이터가 수집되어 있다.
# 다음은 데이터 모델링을 위해 주어지는 파일 목록이다.
# moneyball.csv : 2009년 22명의 상위 투수들에 대한 성적 및 2009년, 2010년 연봉 정보
# moneyball2.csv : 2011년 3명의 상위 투수들에 대한 성적 정보

# 1. 2009년과 2010년의 연봉차이(연봉상승분)을 새로운 변수로 생성하고
# 해당 변수에서의 outlier를 제거하고자 한다.
# 단, outlier 탐지방법은 tukey 방법을 사용하여 multiplier는 1.5를 사용한다.
# tukey 방법의 정의는 다음과 같다.
# 탐지된 outlier값을 구하시오.
# - 소수점 첫째 자리에서 반올림
# - tukey 방법 : q1 - 1.5 * IQR, Q3 + 1.5 * IQR

moneyball <- read.csv("moneyball.csv", header = T, stringsAsFactors = F)
View(moneyball)
glimpse(moneyball)
summary(moneyball) # 결측치는 없음

moneyball$sal_inc <- moneyball$Salary2010 -moneyball$Salary2009
summary(moneyball$sal_inc)

q1 <- quantile(moneyball$sal_inc, 0.25)
q3 <- quantile(moneyball$sal_inc, 0.75)
iqr <- IQR(moneyball$sal_inc)

moneyball$sal_inc[which(moneyball$sal_inc < q1 - ceiling((1.5 * iqr)))] # -10000
moneyball$sal_inc[which(moneyball$sal_inc > q3 + ceiling((1.5 * iqr)))] # 탐지값 없음

idx_outlier <- which(moneyball$sal_inc < q1 - ceiling((1.5 * iqr)) |
                     moneyball$sal_inc > q3 + ceiling((1.5 * iqr)))

moneyball$sal_inc[idx_outlier]

# 2. (1)에서 outlier를 제거한 후 연봉상승분과
# 2009년 모든 성적 변수(ex, 평균자책점, 출전경기수..)와의 상관관계를 구하고
# 해당 상관계수의 절대값을 취하여 그 크기가 큰 순서대로 정렬한다.
# 정렬된 변수 중 상관계수의 절대값이 큰 순서대로 3개의 변수명에 대해 차례대로 답하시오
# - 연봉상승분 자기 자신 (상관계수값:1)은 제외할 것
# - 영어로 된 원래 변수명을 답안에 기재할 것
# - Pearson 상관계수를 사용할 것

moneyball_q2 <- moneyball[-idx_outlier,]
head(moneyball_q2)
moneyball_q2 <- moneyball_q2[,-(1:2)]
cor(moneyball_q2)[,"sal_inc"]
cor_saldiff <- abs(cor(moneyball_q2)[,"sal_inc"])
cor_saldiff[order(-cor_saldiff)][2:4]
names(cor_saldiff[order(-cor_saldiff)][2:4])

order_by(abs(cor(moneyball_q2)[,"sal_inc"]), order_by = )

# 3. "연속형"인 2009년 성적변수 중,
# 연봉상승분과 상관관계 절대값이 0.6보다 큰 변수들을 독립변수로,
# 생성된 연봉상승분을 종속변수로 설정하여 linear regression을 수행하시오.
# 모델에 들어간 변수 중 유의한 계수(coefficient)를 갖는 변수의 개수와
# R^2 값을 순서대로 답하시오.
# - 2009년, 2010년 연봉정보는 독립변수로 사용하지 말 것
# - 회귀 계수의 유의성은 유의수준 5%에서 검정하시오
# (hint : 회귀 계수의 유의성 검정은 t-test로 수행함)
# - R2는 100을 곱하여 소수점 이하 버림한 값을 기재할 것

moneyball_q3 <- moneyball_q2
glimpse(moneyball_q3)
cor(moneyball_q3)[,"sal_inc"]
cor_saldiff <- abs(cor(moneyball_q3)[,"sal_inc"])
cor_saldiff[cor_saldiff > 0.6]
names(cor_saldiff[cor_saldiff > 0.6])

moneyball_q3_lm <- lm(sal_inc ~ Avg_era + Win + Innings + Hits_Avg + SO,
                      data = moneyball_q3)
summary(moneyball_q3_lm)
# 유의한 계수 없음
# R^2 : 66

# 4. moneyball2.csv에는 2011년 3명의 상위 투수들에 대한
# 6개의 성적 정보(Avg_era, Games, Win, Loss, Innings, Hits_Avg)만 수집되어 있다.
# (1)에서 생성된 데이터를 이용하여 위 6개의 성적정보를 독립변수로,
# 연봉상승분을 종속변수로 설정하여 linear regression 모델을 생성하고,
# 이를 이용하여 moneyball2.csv 데이터의 Sosa, Lindblom, Wilson의
# 2012년 연봉상승분 예상값을 순서대로 기재하시오
# - 소수점은 생략(반올림이 아닌 버림)할 것
# - 2012년 연봉이 아닌 연봉상승분을 기재할 것

moneyball2 <- read.csv("moneyball2.csv", header = T, stringsAsFactors = F)
View(moneyball2)
names(moneyball2)[1] <- "Names"

q4_lm <- lm(sal_inc ~ Avg_era + Games + Win + Loss + Innings + Hits_Avg,
            data = moneyball)
summary(q4_lm)
moneyball2$pred <- floor(predict(q4_lm, moneyball2))
moneyball2
