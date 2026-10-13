# 学生数学分数茎叶图
# 数据：20名学生的数学考试分数

# 读取Parquet数据
library(arrow)
df_scores <- read_parquet("student_math_scores.parquet")

# 绘制茎叶图
# scale = 2 表示扩大显示范围，使图表更清晰
stem(df_scores$math_score, scale = 2)

# 茎叶图解读说明：
# 小数点在 | 右侧1位数字处
# 例如：3 | 057 表示 30, 35, 37
#      4 | 009 表示 40, 40, 49
#      5 | 144578 表示 51, 54, 54, 55, 57, 58
#      等等
