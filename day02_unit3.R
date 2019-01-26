# 시각화

# 복원추출
sample(1:10, 10, replace = T)
# 비복원추출
sample(1:10, 10, replace = F)

data_point <- data.frame(xx = 1:10,
                         yy = sample(1:10, 10, replace = T))
data_point

# 산점도
data_point %>%
  ggplot(aes(x = xx, y = yy)) + geom_point()
# 선그래프
data_point %>%
  ggplot(aes(x = xx, y = yy)) + geom_line()

line_df <- data.frame(obs = 1:30,
                      var_1 = rep(c("A","B","C"), 10),
                      value = sample(1:100, size = 10),
                      stringsAsFactors = F
                      )

View(line_df)

line_df %>%
  ggplot(aes(x = obs,
             y = value,
             group = var_1,
             color = var_1
             )) + geom_line()

# 막대그래프
bar_df <- data.frame(obs = 1:10,
                     var_1 = rep(c("A","B","C"), length.out = 10),
                     value = sample(1:100, size = 10),
                     stringsAsFactors = F
                     )

bar_df %>%
  ggplot(aes(x = obs, y = value, fill = value)) + geom_bar(stat = "identity")

bar_df %>%
  ggplot(aes(x = obs, y = value, fill = as.factor(value))) + geom_bar(stat = "identity")

# 다차원시각화
datata("mtcars")
glimpse(mtcars)

# dotplot은 histogram보다 상대적으로 비교가 쉬움
# 덩어리 하나가 나타내는 단위가 있어 좀 더 직관적
mtcars %>%
  # miles per gallon : 수치형
  ggplot(aes(x = mpg)) + geom_histogram(binwidth = 1.5)

mtcars %>%
  ggplot(aes(x = mpg)) + geom_dotplot(binwidth = 1.5)

mtcars %>%
  ggplot(aes(x = disp,
             y = hp,
             color = as.factor(cyl),
             size = as.factor(gear))) + geom_point()

unique(mtcars$gear)

# 1인당 국민소득을 히스토그램으로 시각화
glimpse(gapminder)
summary(gapminder)

gapminder %>%
  ggplot(aes(x = gdpPercap,
             )) + geom_histogram() + scale_x_log10()

gapminder %>%
  ggplot(aes(x = gdpPercap,
            )) + geom_freqpoly() + scale_x_log10()

gapminder %>%
  ggplot(aes(x = gdpPercap,
            )) + geom_density() + scale_x_log10()

# 범주형 데이터는 무조건 막대그래프

# 한 개의 수량형 데이터 시각화
# 각 cut 별로 수치데이터 뽑고 전체의 몇 퍼센트인가
glimpse(diamonds)
diamonds %>%
  ggplot(aes(x = cut)) + geom_bar()

head(diamonds)
unique(diamonds$cut)

diamonds %>%
  group_by(cut) %>%
    summarize(n = n()) # tally와 동일한 결과

diamonds %>%
  group_by(cut) %>%
    tally() %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>% # 새로운 변수 생성
        ggplot(aes(x = cut, y = pct)) + geom_bar(stat = "identity")

# 두 개의 수량형 데이터 시각화
glimpse(diamonds)
diamonds %>%
  ggplot(aes(x = carat, y = price)) + geom_point()
diamonds %>%
  ggplot(aes(x = carat, y = price)) + geom_point(size = 0.5) # point의 사이즈
diamonds %>%
  ggplot(aes(x = carat, y = price, size = 0.5)) + geom_point() # 범례가 만들어짐
diamonds %>%
  ggplot(aes(x = carat, y = price)) + geom_point(alpha = 0.01)
# cut 별로 색상 지정 -> 한번에 보여 지저분함
diamonds %>%
  ggplot(aes(x = carat, y = price, color = cut)) + geom_point()
# cut 별로 데이터를 분리하여 보여
diamonds %>%
  ggplot(aes(x = carat, y = price, color = cut)) + geom_point() + facet_wrap(~ cut, ncol = 3)

glimpse(mpg)
mpg %>%
  ggplot(aes(x = cyl, y = hwy)) + geom_point()
# jitter : 산점도에서 겹쳐있는 데이터를 옆으로 분산시켜주는 효과
mpg %>%
  ggplot(aes(x = cyl, y = hwy)) + geom_jitter() 

# 한 개의 수량형 변수와 한 개의 범주형 변수 시각화
glimpse(mpg)
unique(mpg$class)
# boxplot으로 median, outlier 확인 가능
mpg %>%
  ggplot(aes(x = class, y = hwy)) + geom_boxplot()
mpg %>%
  ggplot(aes(x = class, y = hwy)) + geom_jitter(color = "grey") + geom_boxplot(alpha = 0.5)
mpg %>%
  ggplot(aes(x = class, y = hwy)) + geom_point() + geom_boxplot()

# class를 char -> ordinal factor로 변경
# 기존 데이터 변경하지 않고 잠시 변경해서 사용할 경우 mutate 사용
mpg %>%
  mutate(class2 = factor(class,
                         levels = c("2seater","subcompact","compact","midsize","minivan","suv","pickup"))) %>%
    ggplot(aes(x = class2, y = hwy)) + geom_jitter(color = "grey") + geom_boxplot(alpha = 0.5)

# 2007년도 GDP와 기대수명간의 관계를 나타내는 시각화 using gapminder
glimpse(gapminder)

gapminder %>%
  filter(year == 2007) %>%
    ggplot(aes(x = gdpPercap,
               y = lifeExp)) + geom_point() + scale_x_log10()

# 2007년도 GDP, 기대수명, 대륙, 인구 간의 관계를 나타내는 시각화 using gapminder
gapminder %>%
  filter(year == 2007) %>%
    ggplot(aes(x = gdpPercap,
               y = lifeExp)) + geom_point(aes(color = continent, size = pop)) + scale_x_log10()

rnorm(100, mean = 0, sd = 1)
rnorm(100, mean = 70, sd = 3)
