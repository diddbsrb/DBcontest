df = read.csv("C:/동국대학교/공모전/DB손해보험/R코드/전체전처리_2022.csv")
df

# incidence rates (10만명 당) from the data:
# 남성
inc_M20_24 <- 30.2
inc_M25_29 <- 67.5
inc_M30_34 <- 112.4
inc_M35_39 <- 157.2
inc_M40_44 <- 217.2
inc_M45_49 <- 271.2

# 여성
inc_F20_24 <- 66.9
inc_F25_29 <- 148.7
inc_F30_34 <- 250.7
inc_F35_39 <- 357.6
inc_F40_44 <- 531.8
inc_F45_49 <- 624.8

# 생존확률을 계산하는 함수 (연간 위험이 일정하다고 가정)
surv <- function(inc, years) {
  lambda <- inc / 100000
  exp(-lambda * years)
}

# 1. M20_24 그룹에서 시작 (중간연령 22세, 20년 후 42세)
S_M20_24 <- surv(inc_M20_24, 3)   # 22~24세: 3년
S_M25_29 <- surv(inc_M25_29, 5)   # 25~29세: 5년
S_M30_34 <- surv(inc_M30_34, 5)   # 30~34세: 5년
S_M35_39 <- surv(inc_M35_39, 5)   # 35~39세: 5년
S_M40_44 <- surv(inc_M40_44, 2)   # 40~42세: 2년

overall_surv_M20_24 <- S_M20_24 * S_M25_29 * S_M30_34 * S_M35_39 * S_M40_44
cumRisk_M20_24 <- 1 - overall_surv_M20_24
cumRisk_M20_24  # 약 2.2% 정도

# 2. F20_24 그룹에서 시작 (중간연령 22세)
S_F20_24 <- surv(inc_F20_24, 3)   # 22~24세: 3년
S_F25_29 <- surv(inc_F25_29, 5)   # 25~29세: 5년
S_F30_34 <- surv(inc_F30_34, 5)   # 30~34세: 5년
S_F35_39 <- surv(inc_F35_39, 5)   # 35~39세: 5년
S_F40_44 <- surv(inc_F40_44, 2)   # 40~42세: 2년

overall_surv_F20_24 <- S_F20_24 * S_F25_29 * S_F30_34 * S_F35_39 * S_F40_44
cumRisk_F20_24 <- 1 - overall_surv_F20_24
cumRisk_F20_24  # 약 5.0% 정도

# 3. M25_29 그룹에서 시작 (중간연령 27세, 20년 후 47세)
S_M25_29_start <- surv(inc_M25_29, 3)  # 27~29세: 3년
S_M30_34_2 <- surv(inc_M30_34, 5)        # 30~34세: 5년
S_M35_39_2 <- surv(inc_M35_39, 5)        # 35~39세: 5년
S_M40_44_2 <- surv(inc_M40_44, 5)        # 40~44세: 5년
S_M45_49 <- surv(inc_M45_49, 2)          # 45~46세: 2년 (20년 합계: 3+5+5+5+2 = 20년)

overall_surv_M25_29 <- S_M25_29_start * S_M30_34_2 * S_M35_39_2 * S_M40_44_2 * S_M45_49
cumRisk_M25_29 <- 1 - overall_surv_M25_29
cumRisk_M25_29  # 약 3.2% 정도

# 4. F25_29 그룹에서 시작 (중간연령 27세)
S_F25_29_start <- surv(inc_F25_29, 3)  # 27~29세: 3년
S_F30_34_2 <- surv(inc_F30_34, 5)        # 30~34세: 5년
S_F35_39_2 <- surv(inc_F35_39, 5)        # 35~39세: 5년
S_F40_44_2 <- surv(inc_F40_44, 5)        # 40~44세: 5년
S_F45_49 <- surv(inc_F45_49, 2)          # 45~46세: 2년

overall_surv_F25_29 <- S_F25_29_start * S_F30_34_2 * S_F35_39_2 * S_F40_44_2 * S_F45_49
cumRisk_F25_29 <- 1 - overall_surv_F25_29
cumRisk_F25_29  # 약 7.1% 정도

# 결과 출력
cat("M20_24 그룹 20년 누적 발병률:", round(cumRisk_M20_24*100,2), "%\n")
cat("F20_24 그룹 20년 누적 발병률:", round(cumRisk_F20_24*100,2), "%\n")
cat("M25_29 그룹 20년 누적 발병률:", round(cumRisk_M25_29*100,2), "%\n")
cat("F25_29 그룹 20년 누적 발병률:", round(cumRisk_F25_29*100,2), "%\n")

df_group = df[c(4,5,21,22),]

df_group$K = c(cumRisk_M20_24,cumRisk_M25_29,cumRisk_F20_24,cumRisk_F25_29)
df_group$N = rep(0.5,4)
df_group$S = rep(20000000,4)
df_group
df_group$P = (df_group$K/df_group$N)*df_group$S
df_group



