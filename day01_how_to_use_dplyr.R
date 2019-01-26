glimpse(gapminder)
unique(gapminder$country)

which(gapminder$country == "Korea, Rep.")
which((gapminder$country == "Korea, Rep.") & (gapminder$year == 2007))

gapminder[which(gapminder$country == "Korea, Rep."),c("pop","gdpPercap")]
gapminder[which((gapminder$country == "Korea, Rep.") & (gapminder$year == 2007)),
          c("pop","gdpPercap")]

# dplyr 패키지 사용 예제
# %>% 파이프 연산 : 명령문을 연결해서 사용할 때 인자값 연속해서 넘겨줄 수 있음
gapminder %>% 
  filter(country == "Korea, Rep." & year == 2007) %>% #조건 제약
    select(pop, gdpPercap) # 열 제약

gapminder %>% 
  filter(country == "Korea, Rep.") %>%
    select(year, pop, gdpPercap) %>%
      arrange(year) # 연도별 오름차순

gapminder %>% 
  filter(country == "Korea, Rep.") %>%
    select(year, pop, gdpPercap) %>%
      arrange(-year) # 연도별 내림차순

gapminder %>% 
  filter(country == "Korea, Rep.") %>%
    select(year, pop, gdpPercap) %>%
      arrange(-gdpPercap, -year)

# 기존 변수 이용해서 새로운 변수 생성
gapminder %>%
  mutate(total_gdp = pop * gdpPercap,
         le_gdp_ratio = lifeExp / gdpPercap)

gapminder <- gapminder %>%
                mutate(total_gdp = pop * gdpPercap,
                       le_gdp_ratio = lifeExp / gdpPercap)성

# 요약정보 생성
gapminder %>%
  summarise(ods = n(),
            countries = n_distinct(country),
            year = n_distinct(year),
            pop_mean = mean(pop),
            gdp_max = max(gdpPercap),
            gdp_iqr = IQR(gdpPercap)
            )

# 아래 두개는 같은 statement
unique(gapminder$continent)
gapminder %>%
  select(continent) %>%
    distinct()

# 2007년의 대륙별 기대수명 중앙값 및 평균값
gapminder %>%
  filter(year == 2007) %>%
    group_by(continent) %>%
      summarise(le_median = median(lifeExp),
                le_mean = mean(lifeExp))


df1 <- data.frame(x = c(1,2), y = 2:1)
df1
df2 <- data.frame(x = c(1,3), a = 10, b = "a")
df2
df3 <- data.frame(x1 = c(1,3), y = 2:1)
df3

# join
df1 %>%
  inner_join(df2) # x를 기준으로 조인

df1 %>%
  left_join(df2) # df1의 x를 기준으로 left outer join

df1 %>%
  right_join(df2) # df2의 x를 기준으로 right outer join

df1 %>%
  full_join(df2) # 양쪽 결측치를 모두 보여줌

df2 %>%
  inner_join(df3) # 공통된 컬럼 이름이 없어서 에러발생, by를 이용해서 조건 명시 필요

df2 %>%
  inner_join(df3, by = c("x"="x1"))

df1 %>% union(df3) # union을 위해서는 변수명이 일치해야 함
colnames(df3) <- colnames(df1)
df3
df1 %>% union(df3)
df1 %>% union_all(df3) # 중복을 제거하지 않음


# ggplot 이용한 시각화
gapminder %>%
  ggplot(aes(x=lifeExp)) + geom_histogram()

gapminder %>%
  ggplot(aes(x=lifeExp)) + geom_histogram(bins = 10)

gapminder %>%
  ggplot(aes(x=lifeExp)) + geom_histogram(bins = 30)

gapminder %>%
  ggplot(aes(x=lifeExp)) + geom_histogram(bins = 50)

gapminder %>%
  ggplot(aes(x=gdpPercap)) + geom_histogram()

# y값의 스케일 줄이기 위해 x에 log scale 적용
gapminder %>%
  ggplot(aes(x=gdpPercap)) + geom_histogram() + scale_x_log10()

