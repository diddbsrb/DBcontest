# (1) 기존의 연령구간 데이터 (남성, 여성)
male_intervals <- data.frame(
  group = c("M20_24", "M25_29", "M30_34", "M35_39", "M40_44", "M45_49"),
  lower = c(20, 25, 30, 35, 40, 45),
  upper = c(25, 30, 35, 40, 45, 50),
  inc = c(30.2, 67.5, 112.4, 157.2, 217.2, 271.2)
)

female_intervals <- data.frame(
  group = c("F20_24", "F25_29", "F30_34", "F35_39", "F40_44", "F45_49"),
  lower = c(20, 25, 30, 35, 40, 45),
  upper = c(25, 30, 35, 40, 45, 50),
  inc = c(66.9, 148.7, 250.7, 357.6, 531.8, 624.8)
)

# (2) 누적 위험률 계산 함수 (면책기간 이후의 관찰기간에 대해 계산)
computeCumRisk <- function(ageStart, followUp, intervals) {
  # ageStart: 위험관찰 시작 시 나이 (면책기간 후)
  # followUp: 위험관찰기간 (20 - 면책기간)
  # intervals: 해당 성별의 연령구간 데이터프레임
  current_age <- ageStart
  remaining <- followUp
  surv <- 1  # 누적 생존확률
  
  for (i in 1:nrow(intervals)) {
    if (remaining <= 0) break
    # 현재 나이가 해당 구간을 이미 지난 경우 건너뜀
    if (current_age >= intervals$upper[i]) next
    # 만약 현재 나이가 구간 하한보다 작으면 구간 하한부터 계산
    if (current_age < intervals$lower[i]) {
      current_age <- intervals$lower[i]
    }
    # 이 구간에서 적용할 기간: 구간의 남은 기간과 남은 위험관찰기간 중 작은 값
    available <- min(remaining, intervals$upper[i] - current_age)
    lambda <- intervals$inc[i] / 100000  # 연간 위험률
    surv_interval <- exp(-lambda * available)
    surv <- surv * surv_interval
    current_age <- current_age + available
    remaining <- remaining - available
  }
  # 누적 위험률 = 1 - 누적 생존확률
  return(1 - surv)
}

# (3) 그룹별, 면책기간별 누적 위험률 및 K, S, N, P 값을 계산하여 데이터프레임 생성
# 기본 출발 나이: M20_24, F20_24는 22세 / M25_29, F25_29는 27세 (면책기간 적용 전)
groups <- c("M20_24", "F20_24", "M25_29", "F25_29")
base_start_age <- c(22, 22, 27, 27)  # 각 그룹의 기본 출발 나이

results_df <- data.frame()

# 면책기간 (Grace) 0년부터 10년까지
for (g in 0:10) {
  followUp <- 20 - g  # 면책기간 후의 위험관찰기간
  for (i in 1:length(groups)) {
    grp <- groups[i]
    startAge <- base_start_age[i] + g  # 면책기간이 지나면 시작 나이 증가
    # 성별에 따라 해당하는 연령구간 데이터 사용
    if (grp %in% c("M20_24", "M25_29")) {
      risk <- computeCumRisk(startAge, followUp, male_intervals)
    } else {
      risk <- computeCumRisk(startAge, followUp, female_intervals)
    }
    # 새로운 계산: 
    # K에는 누적 위험률 (risk)을 넣음
    K_val <- risk
    # S는 50,000,000로 고정
    S_val <- 20000000
    # N은 0.5로 고정
    N_val <- 0.5
    # P = (K / N) * S
    P_val <- (K_val / N_val) * S_val
    
    results_df <- rbind(results_df,
                        data.frame(Group = grp,
                                   Grace = g,
                                   StartAge = startAge,
                                   FollowUp = followUp,
                                   CumRisk = risk,
                                   K = K_val,
                                   S = S_val,
                                   N = N_val,
                                   P = P_val))
  }
}

# 결과 확인
print(results_df)


dfM20_24 = results_df[results_df$Group=='M20_24',]
dfF20_24 = results_df[results_df$Group=='F20_24',]
dfM25_29 = results_df[results_df$Group=='M25_29',]
dfF25_29 = results_df[results_df$Group=='F25_29',]

#plot
plot(results_df[results_df$Group=='M20_24',]$Grace,results_df[results_df$Group=='M20_24',]$P)

Mrate = -0.9183
Frate = -1.1086


dfM20_24$P[1]
dfM20_24$N_new = N_val*(dfM20_24$P/dfM20_24$P[1])^(Mrate)
dfF20_24$N_new = N_val*(dfF20_24$P/dfF20_24$P[1])^(Frate)
dfM25_29$N_new = N_val*(dfM25_29$P/dfM25_29$P[1])^(Mrate)
dfF25_29$N_new = N_val*(dfF25_29$P/dfF25_29$P[1])^(Frate)

dfM20_24$P_new = (dfM20_24$K/dfM20_24$N_new)*S_val
dfF20_24$P_new = (dfF20_24$K/dfF20_24$N_new)*S_val
dfM25_29$P_new = (dfM25_29$K/dfM25_29$N_new)*S_val
dfF25_29$P_new = (dfF25_29$K/dfF25_29$N_new)*S_val

dfM20_24
dfF20_24
dfM25_29
dfF25_29

dfM20_24$revenue <- dfM20_24$N_new * dfM20_24$P_new
# which.max(revenue)를 구해본다
grace_opt <- dfM20_24$Grace[ which.max(dfM20_24$revenue) ]
grace_opt





######################
#plotting
library(ggplot2)
library(dplyr)

# 사용자 지정 색상: 필요에 따라 원하는 색상 코드로 변경 가능
custom_fill_colors <- c("M20_24" = "#6b6cff", 
                        "F20_24" = "#ff5c5c", 
                        "M25_29" = "#16cc00", 
                        "F25_29" = "#ffa723")

# 모든 데이터프레임을 하나로 결합
all_df <- bind_rows(dfM20_24, dfF20_24, dfM25_29, dfF25_29)

# Plot 1: Grace vs K by Group
ggplot(all_df, aes(x = Grace, y = K, group = Group, color = Group, fill = Group)) +
  geom_line(size = 1) +
  geom_point(shape = 21, size = 3, stroke = 1, color = "black", alpha = 0.7) +
  scale_color_manual(values = custom_fill_colors) +
  scale_fill_manual(values = custom_fill_colors) +
  labs(title = "Period vs K_new by Group", x = "Period", y = "K_new") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(size = 12))

# Plot 2: Grace vs N_new by Group
ggplot(all_df, aes(x = Grace, y = N_new, group = Group, color = Group, fill = Group)) +
  geom_line(size = 1) +
  geom_point(shape = 21, size = 3, stroke = 1, color = "black", alpha = 0.7) +
  scale_color_manual(values = custom_fill_colors) +
  scale_fill_manual(values = custom_fill_colors) +
  labs(title = "Period vs N_new by Group", x = "Period", y = "N_new") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(size = 12))

# Plot 3: Grace vs P_new by Group
ggplot(all_df, aes(x = Grace, y = P_new, group = Group, color = Group, fill = Group)) +
  geom_line(size = 1) +
  geom_point(shape = 21, size = 3, stroke = 1, color = "black", alpha = 0.7) +
  scale_color_manual(values = custom_fill_colors) +
  scale_fill_manual(values = custom_fill_colors) +
  labs(title = "Period vs P_new by Group", x = "Period", y = "P_new") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(size = 12))
