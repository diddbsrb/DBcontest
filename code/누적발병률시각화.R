# (1) 남/여 연령구간 데이터
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

# (2) 누적 위험률 계산 함수
computeCumRisk <- function(ageStart, followUp, intervals) {
  # ageStart: 위험관찰 시작 시 나이 (출발 나이)
  # followUp: 관찰(위험)기간 (년)
  # intervals: 해당 성별의 연령구간 데이터프레임
  current_age <- ageStart
  remaining <- followUp
  surv <- 1  # 누적 생존확률
  
  for (i in 1:nrow(intervals)) {
    if (remaining <= 0) break
    # 이미 해당 구간을 지난 경우 건너뜁니다.
    if (current_age >= intervals$upper[i]) next
    # 현재 나이가 구간 하한보다 작으면, 구간 하한부터 시작
    if (current_age < intervals$lower[i]) {
      current_age <- intervals$lower[i]
    }
    # 이 구간에서 적용할 기간 = 구간의 잔여 기간과 남은 위험관찰기간 중 작은 값
    available <- min(remaining, intervals$upper[i] - current_age)
    lambda <- intervals$inc[i] / 100000  # 연간 위험률
    surv_interval <- exp(-lambda * available)
    surv <- surv * surv_interval
    current_age <- current_age + available
    remaining <- remaining - available
  }
  # 누적 위험률 = 1 - (전체 생존확률)
  return(1 - surv)
}

# (3) 각 그룹별 기본 출발 나이 설정
# M20_24, F20_24: 22세, M25_29, F25_29: 27세
groups <- c("M20_24", "F20_24", "M25_29", "F25_29")
base_start_age <- c(22, 22, 27, 27)

# (4) 0년부터 20년까지 각 그룹별 누적 위험률 계산 및 데이터프레임 생성
risk_df <- data.frame(FollowUp = 0:20)

for(i in 1:length(groups)) {
  grp <- groups[i]
  start_age <- base_start_age[i]
  # 남성 그룹은 male_intervals, 여성 그룹은 female_intervals 사용
  intervals <- if(grp %in% c("M20_24", "M25_29")) male_intervals else female_intervals
  
  # follow-up 0년일 경우 위험률은 0
  cum_risk <- sapply(0:20, function(t) {
    if(t == 0) return(0)
    computeCumRisk(ageStart = start_age, followUp = t, intervals = intervals)
  })
  risk_df[[grp]] <- cum_risk
}

# 결과 확인
print(risk_df)

risk_df


# 데이터프레임을 long format으로 변환 (FollowUp 열 제외한 나머지 컬럼들을 Group과 IncidenceRate로)
risk_df_long <- risk_df %>%
  pivot_longer(
    cols = -FollowUp,
    names_to = "Group",
    values_to = "IncidenceRate"
  )

# 각 그룹에 할당할 사용자 지정 색상 (원하는 색상으로 변경 가능)
custom_colors <- c("M20_24" = "#6b6cff",  # 파란색
                   "F20_24" = "#ff5c5c",  # 주황색
                   "M25_29" = "#16cc00",  # 녹색
                   "F25_29" = "#ffa723")  # 빨간색

# ggplot을 이용한 꺾은선 그래프 및 점 표시
ggplot(risk_df_long, aes(x = FollowUp, y = IncidenceRate, color = Group, group = Group)) +
  geom_line(size = 1.5) +
  geom_point(shape = 21, 
             size = 3, 
             fill = "white", 
             color = "black",  # 점 테두리 색상
             stroke = 1) +     # 테두리 두께
  scale_color_manual(values = custom_colors) +  # 사용자 지정 색상 적용
  labs(
    title = "보장기간에 따른 그룹별 누적발병률률 변화",
    x = "보장기간",
    y = "CumHaz(K)",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    aspect.ratio = 0.4,  # 원하는 비율로 조정 가능
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 12)
  )
  )
