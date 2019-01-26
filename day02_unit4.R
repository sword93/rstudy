# 통계

# match : 앞의 숫자가 뒤에서 몇번째에 존재하는지 위치값 반환
match(c(1,1,1,2,3,4,4), c(1,2,3,4))
match(c(1,1,1,2,3,4,4), c(2,1,3,4))
match(c(1,1,1,2,3,4,4), c(2,1,3))

# tabulate : 특정 숫자에 대한 count값
tabulate(c(1,1,1,2,4,4))
tabulate(c(2,3,5))

# 해당되는 값의 인덱스값 반환
which.max(c(3,1,0,2))
which.max(c(1,0,3,2))

library("MASS")
glimpse(survey)
summary(survey) # 결측치 확인
mean(survey$Height, na.rm = T)
na.omit(survey$Sex) # 결측치 제거
summary(survey$Sex)
summary(na.omit(survey$Sex))

# 정규분포 신뢰구간
library("ggplot2")
nums = seq(-4, 4, length = 100)
ggplot() +
  geom_line(aes(x = nums, y = dnorm(nums)), size = 2) +
  geom_vline(xintercept = c(mean(nums), mean(nums)-1.96, mean(nums)+1.96),
             color = "#FF0000", linetype = 2) +
  geom_vline(xintercept = c(mean(nums), mean(nums)-2.33, mean(nums)+2.33),
             color = "#009900", linetype = 2) +
  geom_vline(xintercept = c(mean(nums), mean(nums)-2.58, mean(nums)+2.58),
             color = "#0000FF", linetype = 2)

# t-test
glimpse(mpg)
hwy <- mpg$hwy
summary(hwy)  
n <- length(hwy)
n
mu0 <- 22.9
t.test(hwy,
       alternative = "greater",
       mu = mu0)
t.test(hwy,
       alternative = "two.sided",
       mu = mu0)


# sleep data
glimpse(sleep)
View(sleep)
unique(sleep$group)
unique(sleep$ID)

# group 1의 약을 먹은 사람들의 수면 증가시간
y <- sleep$extra[sleep$group == 1]
y
summary(y)
sd(y)
hist(y) # y축이 frequency
boxplot(y)
qqnorm(y) # q-q plot
qqline(y) # 데이터가 직선에 가까울수록 정규분포를 따름
hist(y, probability = T) # y축이 density
lines(density(y), lty = 2)

# 수면시간이 줄어든 사람 수
sleep[sleep$extra < 0,]
nrow(sleep[sleep$extra < 0,])

y[y<0]
length(y[y<0]) # 수면시간이 감소
length(y[y<=0]) # 수면시간 증가 효과가 없음

# 가설검정 : group1 수면제의 효과가 있나요?
t.test(y,
       alternative = "greater",
       mu = 0)
# p-value : 0.10 > 0.05 -> cannot reject H0
# 수면제의 수면시간 증가 효과가 있다고 얘기할 수 없다
# 신뢰구간 : 이 수면제는 얼마나 효과가 있나요? -> 의미없음


# 아래 블럭처리 후 한번에 실행해야 seed값 적용됨
set.seed(1606)
y_star <- rnorm(10, mean = 0, sd = 1.8)
mean(y_star)
sd(y_star)
# t통계량 구하는 공식
t_star <- mean(y_star) / sd(y_star) / sqrt(length(y_star))
t_star

# 앞뒤로 괄호 입력하면 값 바로 출력해줌
(y_star <- rnorm(10, mean = 0, sd = 1.8))
mean(y_star)
sd(y_star)
(t_star <- mean(y_star) / sd(y_star) / sqrt(length(y_star)))

(y_star <- rnorm(10, mean = 0, sd = 1.8))
mean(y_star)
sd(y_star)
(t_star <- mean(y_star) / sd(y_star) / sqrt(length(y_star)))


# 이상치 검출 (IQR 이용)
glimpse(mpg)
summary(mpg$hwy)
boxplot(mpg$hwy)
q1 <- quantile(mpg$hwy, 0.25)
q3 <- quantile(mpg$hwy, 0.75)
q1-1.5*IQR(mpg$hwy)
q3+1.5*IQR(mpg$hwy)
mpg$hwy[mpg$hwy<q1-1.5*IQR(mpg$hwy)]
mpg$hwy[mpg$hwy>q3+1.5*IQR(mpg$hwy)] 

# 이상치 값 제거
mpg$hwy[-which(mpg$hwy > q3+1.5*IQR(mpg$hwy))]
boxplot(mpg$hwy[-which(mpg$hwy > q3+1.5*IQR(mpg$hwy))])

mpg1 <- mpg%>%
          filter((hwy <= q3+1.5*IQR(mpg$hwy)) & (hwy >= q1+1.5*IQR(hwy)))
summary(mpg1)

# 이상치 검출 (mean, sd 이용)
mean(mpg$hwy); sd(mpg$hwy)
mpg$hwy > mean(mpg$hwy) + 3*sd(mpg$hwy)
which(mpg$hwy > mean(mpg$hwy) + 3*sd(mpg$hwy))
which(mpg$hwy < mean(mpg$hwy) - 3*sd(mpg$hwy))
# 이상치가 존재할 때 mean/sd값 대신 사용 (로버스트 방식)
median(mpg$hwy); mad(mpg$hwy)

# 결측치 확인
data("Orange")
Orange
str(Orange)
summary(Orange)
nrow(Orange)
ncol(Orange)

# 임의로 NA값으로 대체
sample(1:(nrow(Orange)*ncol(Orange)), size = 10)
Orange_mx <- as.matrix(Orange)
dim(Orange_mx)
Orange_mx[sample(1:(nrow(Orange)*ncol(Orange)), size = 10)] <- NA
Orange_df <- as.data.frame(Orange_mx)
summary(Orange_df)
glimpse(Orange_df$circumference)

# 결측치 확인 : TRUE/FALSE 반환
is.na(Orange_df)
is.na(Orange_df$Tree)
 
Orange_df[is.na(Orange_df$Tree),]
Orange_df[is.na(Orange_df$age),]
Orange_df[is.na(Orange_df$Tree) | is.na(Orange_df$age),] # 연산도 가능
Orange_df[is.na(Orange_df$Tree) & is.na(Orange_df$age),]
Orange_df[is.na(Orange_df$Tree) & is.na(Orange_df$circumference),]
Orange_df[is.na(Orange_df$age) & is.na(Orange_df$circumference),]
Orange_df[is.na(Orange_df$Tree) & is.na(Orange_df$age) & is.na(Orange_df$circumference),]

# Tree와 circumference의 NA는 제거, age의 NA는 평균으로 대체
# 순서 : age의 결측치 먼저 제거하고나서 drop
# 1) age 결측치 제거
Orange_df_new <- Orange_df
Orange_df_new[is.na(Orange_df_new$age),]

Orange_df_new$age <- as.numeric((as.character(Orange_df_new$age)))
Orange_df_new$age[is.na(Orange_df_new$age)] <- mean(as.numeric((as.character(Orange_df_new$age))), na.rm = T)

Orange_df_new

# 2) Tree와 circumference에 NA가 존재하는 데이터 제거
Orange_df_new[is.na(Orange_df_new$Tree) | is.na(Orange_df_new$circumference),]
nrow(Orange_df_new%>%
  drop_na(Tree, circumference))
Orange_df_new_2 <- Orange_df_new%>%
                  drop_na(Tree, circumference)
Orange_df_new_2


# ANOVA(일원분산분석)
data("diamonds")
glimpse(diamonds)
summary(diamonds)

dd <- aov(price ~ cut + clarity, data = diamond)
summary(dd)

# birthwt dataset 이용
library(MASS)
glimpse(birthwt)
summary(birthwt)
unique(birthwt$low)

as.factor(birthwt$race)
# linier model 적합함수
# factor가 0/1로 되어있는 것들은 굳이 factor 변환 안해도 됨
bw.lm <- lm(bwt ~ age + lwt + as.factor(race) + smoke + ptl + ht + ui + ftv, data = birthwt)
# Analysis of Variance Table : 회귀분석 모델의 분산분석표 생성
anova(bw.lm)
# 영향 없는 걸로 판별된 변수 제거
bw.lm2 <- lm(bwt ~ lwt + as.factor(race) + smoke + ht + ui, data = birthwt)
# 두 모델간 비교
anova(bw.lm2, bw.lm)
anova(bw.lm2)
summary(bw.lm2)


