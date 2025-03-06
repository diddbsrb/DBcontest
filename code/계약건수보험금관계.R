library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# 주어진 데이터
price <- c(5000000,15000000,25000000,35000000,45000000,75000000,
           150000000,250000000) #,350000000,450000000,750000000,1250000000
totalM <- c(9196562,6029989,4543743,1798156,3948728,3458918,
            694396,171714) #,50919,51691,52923,14253
totalF <- c(12244473,9185758,5531680,2083052,3771447,2981033,
            599917,118649) #,34977,31663,29981,6983

# 데이터 프레임 생성
df <- data.frame(
  price = price,
  totalM = totalM,
  totalF = totalF
)

# 산점도용 long format 데이터: totalM과 totalF를 "Gender" 열로 변환하고 "M", "F"로 이름 변경
df_long <- df %>%
  pivot_longer(
    cols = c(totalM, totalF),
    names_to = "Gender",
    values_to = "Total"
  ) %>%
  mutate(Gender = if_else(Gender == "totalM", "M", "F"))

# 로그-로그 회귀 모델 적합
fitM_log <- lm(log(totalM) ~ log(price), data = df)
fitF_log <- lm(log(totalF) ~ log(price), data = df)

summary(fitM_log)
summary(fitF_log)

# 로그-로그 회귀 모델을 표준화된 변수로 적합
fitM_log_std <- lm(scale(log(totalM)) ~ scale(log(price)), data = df)
fitF_log_std <- lm(scale(log(totalF)) ~ scale(log(price)), data = df)

# 모델 요약 출력
summary(fitM_log_std)
summary(fitF_log_std)


# 예측선 작성을 위한 price sequence
price_seq <- seq(min(price), max(price), length.out = 100)

# 회귀계수를 이용하여 원래 단위로 예측값 계산: Total = exp(intercept) * price^(slope)
pred_totalM <- exp(coef(fitM_log)[1]) * price_seq^(coef(fitM_log)[2])
pred_totalF <- exp(coef(fitF_log)[1]) * price_seq^(coef(fitF_log)[2])

# 예측값 데이터 프레임 생성 (Gender: "M"와 "F")
pred_df <- data.frame(
  price = rep(price_seq, 2),
  Predicted = c(pred_totalM, pred_totalF),
  Gender = rep(c("M", "F"), each = length(price_seq))
)

# ggplot을 이용한 산점도와 회귀선 플롯
ggplot() +
  # 산점도: shape = 21, size = 5, 내부 채움은 Gender에 따라, 테두리는 검은색
  geom_point(data = df_long, 
             aes(x = price, y = Total, fill = Gender, color = Gender),
             shape = 21, size = 4, stroke = 1, alpha = 1, color='black') +
  # 회귀선: Gender별로 선을 그립니다.
  geom_line(data = pred_df,
            aes(x = price, y = Predicted, color = Gender),
            size = 1) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Price vs Num by Gender with Log-Log Regression",
    x = "Price",
    y = "Num",
    color = "Gender",
    fill = "Gender"
  ) +
  coord_cartesian(xlim = c(0, 150000000), ylim = c(0, 10000000)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 12)
  )