library: # archive.ics.uci.edu/ml/machine-learning-databases/car
library: # car.data 다운
library: # car.names 다운
library: 
library: 
library: # 데이터 불러오기
library: uciCar <- read.csv("car.data", header = F, stringsAsFactors = F)
library: 
library: # uciCar <- read.table("car.data",
library: #                       sep = ",",
library: #                       header = F,
library: #                       stringsAsFactors = F)
library: 
library: View(uciCar)
library: names(uciCar)
library: names(uciCar) <- c("buying","maint","doors",
library:                    "persons","lug_boot","safety","classf")
library: 
library: # 데이터 제거
library: rm(uciCar)
library: 
library: # read.table은 원격지에서 데이터 불러오는 것도 가능, 데이터명에 url 입력
library: uciCar <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data",
library:                     header = F,
library:                     stringsAsFactors = F)
library: 
library: class(uciCar)
library: summary(uciCar)
library: 
library: # stringAsFactors : 데이터 불러올 때 factor로 읽어올 것인가, 문자열로 읽어올 것인가 선택
library: uciCar2 <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data",
library:                    header = F,
library:                    stringsAsFactors = T)
library: names(uciCar2) <- c("buying","maint","doors",
library:                    "persons","lug_boot","safety","classf")
library: View(uciCar2)
library: class(uciCar2)
library: summary(uciCar2)
library: 
library: # dplyr 함수, 요약보기
library: glimpse(uciCar) #chr
library: glimpse(uciCar2) #fct
library: 
library: class(uciCar$buying)
library: class(uciCar2$buying)
library: 
library: uciCar$buying
library: uciCar2$buying #factor는 level이 존재
library: 
library: # factor 예제
library: x <- c("Male", "Male", "Male", "Female")
library: class(x)
library: 
library: xf <- factor(x, levels = c("Male", "Female"),
library:              labels = c("M", "F"))
library: xf <- as.factor(x)
library: 
library: datasets::iris
library: help("iris")
library: data(iris)
library: glimpse(iris)
library: 
library: 1.5:6
library: 
library: str(iris)
library: 
library: # aws 파일 예제
library: aws <- read.delim("AWS_sample.txt", sep = "#", stringsAsFactors = F)
library: View(aws)
library: glimpse(aws)
library: 
library: colnames(aws)
library: ncol(aws)
library: nrow(aws)
library: aws[5886,]
library: aws[5886,5]
library: aws[5886,"X."]
library: aws[5886,"TA":"X."] # 오류남, ":"는 numeric만 가능
library: aws[5886,3:5]
library: aws[5886,c(3,4,5)]
library: aws[5886,c("TA","Wind","X.")]
library: aws[5886,-5] # 5열을 제외하고 출력
library: 
library: aaa = aws[10:13, 3:4]
library: bbb = aws[20:23, 3:4]
library: 
library: cbind(aaa,bbb)
library: rbind(aaa,bbb)
library: 
library: cbind(1, 1:7)
library: rbind(1, 1:3)
library: cbind(0, rbind(1, 1:3))
library: 
library: cbind(1:2, 1:3)
library: cbind(1:2, 1:4)
library: 
library: # ggplot2 패키지 실습
library: data("diamonds")
library: glimpse(diamonds)
library: diamonds$cut # ordinal : level 간 순서 존재하는 factor (없으면 norminal)
library: 
library: table(diamonds$cut, diamonds$clarity) # 빈도테이블
library: sum(table(diamonds$cut, diamonds$clarity)) # 모든 빈도의 합
library: 
library: colSums(table(diamonds$cut, diamonds$clarity))
library: rowSums(table(diamonds$cut, diamonds$clarity))
library: 
library: apply(table(diamonds$cut, diamonds$clarity),1,sum) # 각 행 별 합 = rowSums
library: apply(table(diamonds$cut, diamonds$clarity),2,sum) # 각 열 별 합 = colSums
library: 
library: # 조건 연산
library: df_1 = data.frame(aa = c("a","b","c","d"),
library:                   bb = 1:4)
library: df_1 = data.frame(aa = c("a","b","c","d"),
library:                   bb = 1:4, stringsAsFactors = F)
library: 
library: df_1$aa = as.factor(df_1$aa)
library: df_1$aa = as.character(df_1$aa)
library: 
library: glimpse(df_1)
library: 
library: bool_vec <- df_1$bb >= 3
library: bool_vec
library: which(bool_vec) # 중요!! 조건에 맞는 값의 index 위치 찾아내는 함수
library: 
library: df_1[df_1$bb >=3,]
library: 
library: # 속성확인
library: letters # a~z 알파벳 저장된 벡터
library: str(letters)
library: 
library: letters[1:5]
library: col <- letters[1:5]
library: class(col)
library: col <- as.factor(col)
library: col <- as.character(col)
library: col
library: 
library: glimpse(diamonds)
library: diamonds[1:5, c("carat","cut","color","clarity")]
library: dia_tbl <- table(diamonds$cut, diamonds$clarity)
library: dia_tbl
library: as.data.frame(dia_tbl) # frequency 데이터 추가됨
library: 
library: t(as.data.frame(dia_tbl)) # 전치행렬 : 행과 열을 반대로
library: 
library: data("iris")
library: iris
library: str(iris)
library: 
library: unique(iris$Species)
library: table(iris$Species)
library: 
library: library(MASS)
library: data("birthwt")
library: glimpse(birthwt)
library: 
library: birthwt$race <- as.factor(birthwt$race)
library: birthwt$smoke <- as.factor(birthwt$smoke)
library: 
library: table(birthwt$race, birthwt$smoke)
library: as.data.frame(table(birthwt$race, birthwt$smoke)) # 계층구조로 데이터 표현
library: t(as.data.frame(table(birthwt$race, birthwt$smoke))) # 전치행렬
library: 
library: # Pivoting
library: set.seed(123) # 난수 발생 시 동일값 재현 가능
library: sample(1:100,5)
library: 
library: df <- data.frame(Obs = 1:4,
library:                  A = sample(1:100, 4),
library:                  B = sample(1:100, 4),
library:                  C = sample(1:100, 4))
library: df
library: 
library: # reshape을 이용해서 table을 dataframe 형태로 변환 : cast
library: df_melt <- melt(df, id.vars = "Obs", variable.name = "Group", value.name = "Count")
library: df_melt
library: 
library: # molten dataframe 만들기 : table을 다시 dataframe 형태로 변환
library: # Obs가 종속변수, Group이 설명변수
library: dcast(df_melt, formula = Obs ~ Group, value.var = "Count")
library: 
library: # if-else 예제
library: df <- read.csv("rating_ramyun.csv", stringsAsFactors = F)
library: View(df)
library: rm(df)
library: 
library: glimpse(df)
library: unique(df$Country)
library: 
library: # 새로운 컬럼 추가 : df[:,"kr"]와 같은 기능
library: df$kr <- ifelse(df$Country == "South Korea",1,0)
library: 
library: # apply() 함수 예제 : 과목별 평균, 최대값, 최소값 dataframe에 넣기
library: score <- read.csv("class_scores.csv", stringsAsFactors = F)
library: str(score)
library: glimpse(score)
library: View(score)
library: head(score)
library: 
library: colnames(score)[5:ncol(score)]
library: 
library: apply(X = score[,5:ncol(score)], MARGIN = 2, FUN = "mean")
library: apply(X = score[,5:ncol(score)], MARGIN = 2, FUN = "max")
library: apply(X = score[,5:ncol(score)], MARGIN = 2, FUN = "min")
library: 
library: df_subject <- data.frame(subject = colnames(score)[5:ncol(score)],
library:                          mean = apply(X = score[,5:ncol(score)], MARGIN = 2, FUN = "mean"),
library:                          max = apply(X = score[,5:ncol(score)], MARGIN = 2, FUN = "max"),
library:                          min = apply(X = score[,5:ncol(score)], MARGIN = 2, FUN = "min")
library:                          )
library: rownames(df_subject) <- NULL # row name 삭제
library: df_subject
library: 
library: 
library: # file merge : sql join
library: source("data_generator_join.R", encoding = "utf-8")
library: View(df_room)
library: View(df_list)
