# archive.ics.uci.edu/ml/machine-learning-databases/car
# car.data 다운
# car.names 다운


# 데이터 불러오기
uciCar <- read.csv("car.data", header = F, stringsAsFactors = F)

# uciCar <- read.table("car.data",
#                       sep = ",",
#                       header = F,
#                       stringsAsFactors = F)

View(uciCar)
names(uciCar)
names(uciCar) <- c("buying","maint","doors",
                   "persons","lug_boot","safety","classf")

# 데이터 제거
rm(uciCar)

# read.table은 원격지에서 데이터 불러오는 것도 가능, 데이터명에 url 입력
uciCar <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data",
                    header = F,
                    stringsAsFactors = F)

class(uciCar)
summary(uciCar)

# stringAsFactors : 데이터 불러올 때 factor로 읽어올 것인가, 문자열로 읽어올 것인가 선택
uciCar2 <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data",
                   header = F,
                   stringsAsFactors = T)
names(uciCar2) <- c("buying","maint","doors",
                   "persons","lug_boot","safety","classf")
View(uciCar2)
class(uciCar2)
summary(uciCar2)

# dplyr 함수, 요약보기
glimpse(uciCar) #chr
glimpse(uciCar2) #fct

class(uciCar$buying)
class(uciCar2$buying)

uciCar$buying
uciCar2$buying #factor는 level이 존재

# factor 예제
x <- c("Male", "Male", "Male", "Female")
class(x)

xf <- factor(x, levels = c("Male", "Female"),
             labels = c("M", "F"))
xf <- as.factor(x)

datasets::iris
help("iris")
data(iris)
glimpse(iris)

1.5:6

str(iris)

# aws 파일 예제
aws <- read.delim("AWS_sample.txt", sep = "#", stringsAsFactors = F)
View(aws)
glimpse(aws)

colnames(aws)
ncol(aws)
nrow(aws)
aws[5886,]
aws[5886,5]
aws[5886,"X."]
aws[5886,"TA":"X."] # 오류남, ":"는 numeric만 가능
aws[5886,3:5]
aws[5886,c(3,4,5)]
aws[5886,c("TA","Wind","X.")]
aws[5886,-5] # 5열을 제외하고 출력

aaa = aws[10:13, 3:4]
bbb = aws[20:23, 3:4]

cbind(aaa,bbb)
rbind(aaa,bbb)

cbind(1, 1:7)
rbind(1, 1:3)
cbind(0, rbind(1, 1:3))

cbind(1:2, 1:3)
cbind(1:2, 1:4)

# ggplot2 패키지 실습
data("diamonds")
glimpse(diamonds)
diamonds$cut # ordinal : level 간 순서 존재하는 factor (없으면 norminal)

table(diamonds$cut, diamonds$clarity) # 빈도테이블
sum(table(diamonds$cut, diamonds$clarity)) # 모든 빈도의 합

colSums(table(diamonds$cut, diamonds$clarity))
rowSums(table(diamonds$cut, diamonds$clarity))

apply(table(diamonds$cut, diamonds$clarity),1,sum) # 각 행 별 합 = rowSums
apply(table(diamonds$cut, diamonds$clarity),2,sum) # 각 열 별 합 = colSums

# 조건 연산
df_1 = data.frame(aa = c("a","b","c","d"),
                  bb = 1:4)
df_1 = data.frame(aa = c("a","b","c","d"),
                  bb = 1:4, stringsAsFactors = F)

df_1$aa = as.factor(df_1$aa)
df_1$aa = as.character(df_1$aa)

glimpse(df_1)

bool_vec <- df_1$bb >= 3
bool_vec
which(bool_vec) # 중요!! 조건에 맞는 값의 index 위치 찾아내는 함수

df_1[df_1$bb >=3,]

# 속성확인
letters # a~z 알파벳 저장된 벡터
str(letters)

letters[1:5]
col <- letters[1:5]
class(col)
col <- as.factor(col)
col <- as.character(col)
col

glimpse(diamonds)
diamonds[1:5, c("carat","cut","color","clarity")]
dia_tbl <- table(diamonds$cut, diamonds$clarity)
dia_tbl
as.data.frame(dia_tbl) # frequency 데이터 추가됨

t(as.data.frame(dia_tbl)) # 전치행렬 : 행과 열을 반대로

data("iris")
iris
str(iris)

unique(iris$Species)
table(iris$Species)

library(MASS)
data("birthwt")
glimpse(birthwt)

birthwt$race <- as.factor(birthwt$race)
birthwt$smoke <- as.factor(birthwt$smoke)

table(birthwt$race, birthwt$smoke)
as.data.frame(table(birthwt$race, birthwt$smoke)) # 계층구조로 데이터 표현
t(as.data.frame(table(birthwt$race, birthwt$smoke))) # 전치행렬

# Pivoting
set.seed(123) # 난수 발생 시 동일값 재현 가능
sample(1:100,5)

df <- data.frame(Obs = 1:4,
                 A = sample(1:100, 4),
                 B = sample(1:100, 4),
                 C = sample(1:100, 4))
df

# reshape을 이용해서 table을 dataframe 형태로 변환 : cast
df_melt <- melt(df, id.vars = "Obs", variable.name = "Group", value.name = "Count")
df_melt

# molten dataframe 만들기 : table을 다시 dataframe 형태로 변환
# Obs가 종속변수, Group이 설명변수
dcast(df_melt, formula = Obs ~ Group, value.var = "Count")

# if-else 예제
df <- read.csv("rating_ramyun.csv", stringsAsFactors = F)
View(df)
rm(df)

glimpse(df)
unique(df$Country)

# 새로운 컬럼 추가 : df[:,"kr"]와 같은 기능
df$kr <- ifelse(df$Country == "South Korea",1,0)

# apply() 함수 예제 : 과목별 평균, 최대값, 최소값 dataframe에 넣기
score <- read.csv("class_scores.csv", stringsAsFactors = F)
str(score)
glimpse(score)
View(score)
head(score)

colnames(score)[5:ncol(score)]

apply(X = score[,5:ncol(score)], MARGIN = 2, FUN = "mean")
apply(X = score[,5:ncol(score)], MARGIN = 2, FUN = "max")
apply(X = score[,5:ncol(score)], MARGIN = 2, FUN = "min")

df_subject <- data.frame(subject = colnames(score)[5:ncol(score)],
                         mean = apply(X = score[,5:ncol(score)], MARGIN = 2, FUN = "mean"),
                         max = apply(X = score[,5:ncol(score)], MARGIN = 2, FUN = "max"),
                         min = apply(X = score[,5:ncol(score)], MARGIN = 2, FUN = "min")
                         )
rownames(df_subject) <- NULL # row name 삭제
df_subject


# file merge : sql join
source("data_generator_join.R", encoding = "utf-8")
View(df_room)
View(df_list)



