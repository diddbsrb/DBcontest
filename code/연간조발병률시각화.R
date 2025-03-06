df = read.csv("C:/동국대학교/공모전/DB손해보험/data/전체전처리.csv")
df

library(ggplot2)
library(dplyr)

# 네 그룹만 선택 (필요하다면)
library(ggplot2)
library(dplyr)

# 필요한 그룹만 필터링
df_subset <- df %>% 
  filter(Group %in% c("M20_24", "F20_24", "M25_29", "F25_29"))

# 각 그룹에 할당할 색상을 지정 (원하는 색상 코드로 변경 가능)
custom_colors <- c("M20_24" = "#6b6cff",  # 파란색
                   "F20_24" = "#ff5c5c",  # 주황색
                   "M25_29" = "#16cc00",  # 녹색
                   "F25_29" = "#ffa723")  # 빨간색

ggplot(df_subset, aes(x = Year, y = IncidenceRate, color = Group, group = Group)) +
  geom_line(linewidth = 1.5) +
  geom_point(shape = 21, 
             size = 3, 
             fill = "white", 
             color = "black",  # 점 테두리 색상
             stroke = 1) +     # 테두리 두께
  labs(
    title = "Yearly Incidence Rate by Group",
    x = "Year",
    y = "Incidence Rate",
    color = "Group"
  ) +
  scale_color_manual(values = custom_colors) +  # 사용자 지정 색상 적용
  theme_minimal() +
  theme(
    aspect.ratio = 0.3,  # 원하는 비율로 조정 가능
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 12)
  )
  )

