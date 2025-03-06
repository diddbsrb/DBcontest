# 1. 각 연령구간 및 발생률 데이터프레임 (남성, 여성)
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

# 2. 누적 생존확률(누적 위험률)을 구하는 함수
computeCumRisk <- function(ageStart, followUp, intervals) {
  # ageStart: 위험기간 시작시 나이 (면책기간 후)
  # followUp: 위험기간 (20 - 면책기간)
  # intervals: 해당 성별의 연령구간 데이터프레임
  current_age <- ageStart
  remaining <- followUp
  surv <- 1  # 누적 생존확률
  
  for (i in 1:nrow(intervals)) {
    if (remaining <= 0) break  # 위험기간 종료
    # 현재 나이가 해당 구간보다 이미 큰 경우 건너뛰기
    if (current_age >= intervals$upper[i]) next
    # 만약 현재 나이가 구간 하한보다 작으면, 구간 시작부터 적용
    if (current_age < intervals$lower[i]) {
      current_age <- intervals$lower[i]
    }
    # 이 구간에서 적용할 기간 = 구간의 남은 기간과 남은 위험기간 중 작은 값
    available <- min(remaining, intervals$upper[i] - current_age)
    # 연간 위험률 (λ)
    lambda <- intervals$inc[i] / 100000
    # 해당 구간에서 available년 동안 생존할 확률
    surv_interval <- exp(-lambda * available)
    surv <- surv * surv_interval
    current_age <- current_age + available
    remaining <- remaining - available
  }
  # 누적 위험률 = 1 - 누적 생존확률
  return(1 - surv)
}

# 3. 면책기간(g) = 0부터 10년까지 각 그룹별 누적 위험률 계산  
#    - M20_24, F20_24: 기본 출발나이 22세 (중간값 가정)
#    - M25_29, F25_29: 기본 출발나이 27세

grace_vec <- 0:10
results <- data.frame(grace = grace_vec,
                      M20_24 = NA,
                      F20_24 = NA,
                      M25_29 = NA,
                      F25_29 = NA)

for (g in grace_vec) {
  followUp <- 20 - g  # 위험기간
  # 면책기간 g년 후, 출발 나이는 기본 출발나이에 g를 더한 값
  risk_M20_24 <- computeCumRisk(22 + g, followUp, male_intervals)
  risk_F20_24 <- computeCumRisk(22 + g, followUp, female_intervals)
  risk_M25_29 <- computeCumRisk(27 + g, followUp, male_intervals)
  risk_F25_29 <- computeCumRisk(27 + g, followUp, female_intervals)
  
  results[results$grace == g, "M20_24"] <- risk_M20_24
  results[results$grace == g, "F20_24"] <- risk_F20_24
  results[results$grace == g, "M25_29"] <- risk_M25_29
  results[results$grace == g, "F25_29"] <- risk_F25_29
}

print(results)


df = read.csv("C:/동국대학교/공모전/DB손해보험/R코드/전체전처리_2022.csv")
df
df_group = df[c(4,5,21,22),]
df_group
