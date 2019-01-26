data(iris)
glimpse(iris)

aggregate(data = iris, Sepal.Length ~ Species, mean)
iris %>%
  group_by(Species) %>%
    summarise(SL_mean = mean(Sepal.Length))

# 데이터 분할
# 수치형 변수를 코드화 할 때 사용 가능
df <- data.frame(obs = 1:100,
                nums = runif(100))
df[,"bin"] <- cut(df$nums, breaks = seq(0, 1, by = 0.05))
df
# class_num 변수 생성하여 df와 join
sapply(df, "class")
df$bin[1]
df_class <- data.frame(class = levels(df$bin),
                       class_num = 1:20)
df_class
df_join <- left_join(x = df, y = df_class, by = c("bin" = "class"))
df_join




