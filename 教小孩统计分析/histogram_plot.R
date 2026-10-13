# 学生数学分数直方图
# 数据：20名学生的数学考试分数

# 读取Parquet数据
library(arrow)
library(ggplot2)

df_scores <- read_parquet("student_math_scores.parquet")

# 绘制直方图
ggplot(df_scores, aes(x = math_score)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(
    title = "学生数学考试分数分布",
    x = "分数",
    y = "学生人数"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(20, 100, 10))

# 直方图参数说明：
# binwidth = 10：每个矩形条代表10分的区间
# fill = "steelblue"：矩形条填充颜色
# color = "white"：矩形条边框颜色
# alpha = 0.7：透明度设置
