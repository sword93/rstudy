load("exampleData.rData")

# example for custdata
summary(custdata)
View(custdata)

# 1000개의 데이터 중 결측치 확인
# 결측치가 있다고 해서 무조건 제거하면 안됨 -> 건수가 많은 경우 영향도 큼

# 결측치 처리 방법
# 주의! 항상 원본 컬럼은 건드리면 안됨, 새로운 컬럼 생성

# 1. 범주형 데이터의 결측치 처리 : 결측치를 범주형으로 변경
summary(custdata$is.employed) # 결측치 328건
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed), "missing",
                                   ifelse(custdata$is.employed ==TRUE,"employed", "not employed"))

# 2. 수량형 데이터의 결측치 처리 : 요약통계량 중 한 값으로 대체 (median, mean, min, max 등 / 이번엔 mean 사용)
summary(custdata$Income) # 결측치 328건
meanIncome <- mean(custdata$Income, na.rm = T)
custdata$Income.fix <- ifelse(is.na(custdata$Income), meanIncome, custdata$Income)
summary(custdata$Income.fix)

custdata[,c("Income","Income.fix")]

# 3. 결측치를 가진 수량형 데이터의 그룹화
summary(custdata$Income)
custdata$Income.groups <- cut(custdata$Income,
                              breaks = c(0, 10000, 50000, 100000, 250000, 1000000),
                              include.lowest = T) # 기본적으로 가장 작은 값이 빠지는 함수이므로, 해당 값을 포함시키기 위한 조
summary(custdata$Income.groups) # NA가 포함되어 있는 형태

custdata$Income.groups <- as.character(custdata$Income.groups)
custdata$Income.groups <- ifelse(is.na(custdata$Income.groups),"no income", custdata$Income.groups)
summary(as.factor(custdata$Income.groups))


# exmaple for medianincome
summary(medianincome)
View(medianincome)

# 4. 데이터 정규화 : 여기서는 중앙값으로 나눔
custdata <- merge(custdata, medianincome, by.x = "state.of.res", by.y = "State")
summary(custdata[,c("state.of.res","income","Median.Income.y")])

custdata$income.norm.new <- with(custdata, income/Median.Income.y)
summary(custdata$income.norm.new)

# 5. 수량형 데이터를 논리형으로 변환
# income이 8만불 이상/이하 구분
custdata$income.lt.80k <- custdata$income < 80000
summary(custdata$income.lt.80k)

# 예제 : age 그룹화 0~25 / 25~65 / 65 ~
summary(custdata$age)
custdata$age.range.new <- cut(custdata$age,
                               breaks = c(0,25,65,Inf),
                               include.lowest = T)

summary(custdata$age.range.new)

# 예제 : age 데이터 정규화 (평균으로 나누기, 이상치 제거 전)
mean_age <- mean(custdata$age)
custdata$age.norm.new <- custdata$age/mean_age
summary(custdata$age.norm.new)

summary(custdata$age.normalized) # 해당 값에서 평균을 빼고 표준편차로 나누어 정규화 시킴

# 데이터 cleansing
a <- c(rep("A",3), rep("B",3), rep("C",2))
a
b <- c(1,1,2,4,1,1,2,2)
b
length(a)
length(b)

df <- data.frame(a,b)
df # 중복되는 행이 존재하는 dataframe으로 생성됨
df_new <- df %>% distinct() # tidyverse 패키지, 중복행 제거됨
df_new2 <- unique(df) # distinct()와 동일한 기능
duplicated(df) # 중복된 행에 대한 boolean 벡터 반환
df_new3 <- df[!duplicated(df),] # 중복된 행의 index 제거

# psych 패키지 설치 : dummy.code() 제공

df <- data.frame(sno = 1:6,
                 major = c("math","physics","computer"),
                 stringsAsFactors = F)
glimpse(df)
major_dummy <- dummy.code(df$major)
df <- data.frame(df, major_dummy)
df




