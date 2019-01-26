h_info <- read.csv("NHIS_2016_info.csv", 
                   stringsAsFactors = F)
h_result <- read.csv("NHIS_2016_result.csv", 
                     stringsAsFactors = F)

h_data <- merge(h_info, h_result, 
                by = c("HCHK_YEAR", "IDV_ID"))

glimpse(h_data)
summary(h_data)

range(h_data$AGE_GROUP)
quantile(h_data$AGE_GROUP)
h_data$AGE_GROUP <- floor((h_data$AGE_GROUP*5-1)/10) * 10
View(h_data)

idx <-
  which(is.na(h_data$HEIGHT) | 
        is.na(h_data$BP_HIGH) |
        is.na(h_data$BP_LWST) |
        is.na(h_data$HDL_CHOLE) |
        is.na(h_data$LDL_CHOLE) |
        is.na(h_data$SMK_STAT_TYPE_CD))

length(idx)
h_data <- h_data[-idx, ]

glimpse(h_data)

# 1.
male30_idx <- which(h_data$SEX == "M" & h_data$AGE_GROUP == 30)
length(male30_idx)
h_male30 <- h_data[male30_idx, ]
head(h_male30)

bph_mean <- floor(mean(h_male30$BP_HIGH)*100)/100
bph_sd <- floor(sd(h_male30$BP_HIGH)*100)/100

outlier_idx <- 
  which(h_male30$BP_HIGH > bph_mean + 3 * bph_sd | 
        h_male30$BP_HIGH < bph_mean - 3 * bph_sd)

length(outlier_idx)


# 2. 

h_male <- h_data[h_data$SEX == "M", ]
summary(h_male$HDL_CHOLE)

h_male$HDL_CHOLE < 40

h_male$LHDL_C <- factor(as.numeric(h_male$HDL_CHOLE < 40), levels = c(0, 1))
head(h_male$LHDL_C)
glimpse(h_male)
unique(h_male$SMK_STAT_TYPE_CD)
h_male$SMK_STAT_TYPE_CD <- factor(h_male$SMK_STAT_TYPE_CD)
glimpse(h_male)
(t <- table(h_male$SMK_STAT_TYPE_CD, h_male$LHDL_C))
chisq.test(t)
summary(t)


# 3. 
(sex <- dummy.code(h_data$SEX))
h_data3 <- data.frame(h_data, sex)
glimpse(h_data3)
summary(h_data3)
range(h_data3[is.na(h_data3$WAIST), "IDV_ID"])

h_data3[is.na(h_data3$SIGHT_LEFT), c("SIGHT_RIGHT", "HEAR_LEFT", "HEAR_RIGHT")]        
idx_na <- which(is.na(h_data3$SIGHT_LEFT) |
                is.na(h_data3$SIGHT_RIGHT) |
                is.na(h_data3$HEAR_LEFT) |
                is.na(h_data3$HEAR_RIGHT))
length(idx_na)
h_data3 <- h_data3[-idx_na, ]
nrow(h_data3)

h_data3$BMI <- h_data3$WEIGHT / (h_data3$HEIGHT / 100) ^ 2
glimpse(h_data3)

h_data3_u9k <- h_data3[h_data3$IDV_ID <= 9000, ]
h_data3_o9k <- h_data3[h_data3$IDV_ID > 9000, ]

n_col <- ncol(h_data3_u9k)
cor(h_data3_u9k[, 4:n_col])[, "WAIST"]
idx_col <- which(abs(cor(h_data3_u9k[, 4:n_col])[, "WAIST"]) >= 0.1)
idx_col
names(h_data3_u9k[, 4:n_col])[idx_col]
train <- h_data3_u9k[, 4:n_col][idx_col]
names(train)
train <- train[, -(1:2)]
glimpse(train)
test <- h_data3_o9k[, 4:n_col][idx_col]
test <- test[, -(1:2)]
glimpse(test)

h_data3_lm <- lm(WAIST ~ ., data = train)
summary(h_data3_lm)

pred_waist<- predict(h_data3_lm, test)
round(mean(pred_waist), 2)
round(var(pred_waist), 2)


# 4.

smoke <- dummy.code(h_data3$SMK_STAT_TYPE_CD)
h_data4 <- data.frame(h_data3, smoke)
glimpse(h_data4)

glm_x1 <- glm(X1 ~ HDL_CHOLE, data = h_data4, family = binomial)
glm_x2 <- glm(X2 ~ HDL_CHOLE, data = h_data4, family = binomial)
glm_x3 <- glm(X3 ~ HDL_CHOLE, data = h_data4, family = binomial)

round(exp(coef(glm_x1)[2]), 2)
round(exp(coef(glm_x2)[2]), 2)
round(exp(coef(glm_x3)[2]), 2)

glimpse(h_data4)
h_data4$SMK_EX_CODE <- ifelse(h_data4$SMK_STAT_TYPE_CD == 1, 1, 0)
smoke_ex <- dummy.code(h_data4$SMK_EX_CODE)
h_data4 <- data.frame(h_data4, smoke_ex)
glimpse(h_data4)

glm_x0 <- glm(X0 ~ HDL_CHOLE, data = h_data4, family = binomial)
glm_x1.1 <- glm(X1.1 ~ HDL_CHOLE, data = h_data4, family = binomial)
round(exp(coef(glm_x0)[2]), 2)
round(exp(coef(glm_x1.1)[2]), 2)
