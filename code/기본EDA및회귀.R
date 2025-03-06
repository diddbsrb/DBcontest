df = read.csv("C:/동국대학교/공모전/DB손해보험/data/전체전처리.csv")
df

library(dplyr)
library(tidyr)
library(car)

df <- df %>%
  # Group 열에서 첫 글자를 Sex 열로, 나머지를 Age 열로 추출
  mutate(
    Sex = substr(Group, 1, 1),
    Age = substr(Group, 2, nchar(Group))
  ) %>%
  # Age 열을 '-' 기준으로 두 열로 분리
  separate(Age, into = c("age_low", "age_high"), sep = "_") %>%
  # 각 값을 숫자로 변환한 후 평균을 구해 AgeInt 열 생성
  mutate(Age_mid = (as.numeric(age_low) + as.numeric(age_high)) / 2)

df <- df %>%
  mutate(
    Age_mid_sc = as.numeric(scale(Age_mid)),
    Year_sc = as.numeric(scale(Year)),
    Population_sc = as.numeric(scale(Population)),
    IncidenceRate_sc = as.numeric(scale(IncidenceRate))
  )

df
hist(df$Age_mid)

df$Sex = factor(df$Sex)

lm1 = lm(IncidenceRate_sc~Age_mid_sc+Sex+Year_sc+Population_sc+Sex:Age_mid_sc, data=df)
summary(lm1)
Anova(lm1)

lm2 = lm(IncidenceRate~Age_mid+Sex+Year+Population+Sex:Age_mid, data=df)
summary(lm2)

df

plot(df$Age_mid, df$IncidenceRate, col=df$Sex)

library(ggplot2)

ggplot(df, aes(x = Age_mid, y = IncidenceRate, color = Sex)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Age_mid vs Incidence Rate by Sex",
    x = "Age_mid",
    y = "Incidence Rate",
    color = "Sex"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


ggplot(df, aes(x = Age_mid, y = IncidenceRate, fill = Sex)) +
  geom_jitter(shape = 21, 
              size = 5, 
              width = 0.2, 
              height = 0.2, 
              color = "black",  # 점 테두리 색상
              stroke = 1) +     # 테두리 두께
  labs(
    title = "Age vs Incidence Rate by Sex",
    x = "Age_mid",
    y = "Incidence Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 12)
  )


df
