# 5个组的箱线图 - 教师反馈方式对学生成绩的影响
# 数据：45名学生的测试结果，分为5个组别

# 读取Parquet数据
library(arrow)
library(ggplot2)

df_45students <- read_parquet("test_results_45students.parquet")

# 绘制箱线图
ggplot(df_45students, aes(x = group_name, y = test_score, fill = group)) +
  geom_boxplot(alpha = 0.7, outlier.size = 2) +
  labs(
    title = "不同教师反馈方式对学生测试成绩的影响",
    x = "教师反馈方式",
    y = "测试分数"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")

# 箱线图参数说明：
# geom_boxplot()：绘制箱线图
# alpha = 0.7：透明度设置
# outlier.size = 2：异常值点的大小
# scale_fill_brewer()：使用ColorBrewer配色方案
# 
# 箱线图解读：
# - 中间的线（median）：表示中位数
# - 箱子下边界（Q1）：第一四分位数
# - 箱子上边界（Q3）：第三四分位数
# - 箱子高度（IQR）：四分位数间距，包含中间50%的数据
# - 上下须（whisker）：正常数据范围的边界
# - 圆点：异常值（超出1.5倍IQR范围的数据）

# 组别说明：
# A组（控制组）：对照组，未指定具体反馈
# B组（控制组）：对照组，未指定具体反馈
# C组（赞赏组）：接受积极赞赏反馈
# D组（责骂/贬抑组）：接受批评/贬抑反馈
# E组（不理睬组）：接受冷淡/忽视反馈
