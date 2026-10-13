# 29名厌食症女孩的平均体重变化数据
# 数据来源：营养学研究

# 方法1: 简单向量格式
weight_change <- c(
  1.7, 0.7, -0.1, -0.7, -3.5, 14.9, 3.5, 1.71, -7.6, 1.6,
  11.7, 2.6, 1.1, -4.0, 20.9, -9.1, 2.1, 1.4, -0.3, -3.7,
  -1.4, -0.8, 2.4, 12.6, 1.9, 3.9, 0.1, 15.4, -0.7
)

# 方法2: 数据框格式
anorexia <- data.frame(
  patient_id = 1:29,
  weight_change = c(
    1.7, 0.7, -0.1, -0.7, -3.5, 14.9, 3.5, 1.71, -7.6, 1.6,
    11.7, 2.6, 1.1, -4.0, 20.9, -9.1, 2.1, 1.4, -0.3, -3.7,
    -1.4, -0.8, 2.4, 12.6, 1.9, 3.9, 0.1, 15.4, -0.7
  )
)

# ============================================
# 1. 数据预览
# ============================================
head(anorexia)
summary(anorexia$weight_change)

# ============================================
# 2. 描述性统计分析
# ============================================
cat("\n=== 描述性统计 ===\n")
cat("样本量:", nrow(anorexia), "\n")
cat("平均体重变化:", round(mean(anorexia$weight_change), 2), "kg\n")
cat("中位数:", median(anorexia$weight_change), "kg\n")
cat("标准差:", round(sd(anorexia$weight_change), 2), "kg\n")
cat("方差:", round(var(anorexia$weight_change), 2), "kg²\n")
cat("最小值:", min(anorexia$weight_change), "kg\n")
cat("最大值:", max(anorexia$weight_change), "kg\n")
cat("极差:", max(anorexia$weight_change) - min(anorexia$weight_change), "kg\n")
cat("四分位数:\n")
print(quantile(anorexia$weight_change))

# ============================================
# 3. 体重变化分类统计
# ============================================
cat("\n=== 体重变化分类 ===\n")
positive <- sum(anorexia$weight_change > 0)
negative <- sum(anorexia$weight_change < 0)
zero <- sum(anorexia$weight_change == 0)
cat("体重增加的人数:", positive, "人 (", 
    round(positive/nrow(anorexia)*100, 1), "%)\n", sep = "")
cat("体重减少的人数:", negative, "人 (", 
    round(negative/nrow(anorexia)*100, 1), "%)\n", sep = "")
cat("体重不变的人数:", zero, "人\n")

# ============================================
# 4. 体重变化幅度分类
# ============================================
cat("\n=== 体重变化幅度分类 ===\n")
large_change <- sum(abs(anorexia$weight_change) >= 10)
small_change <- sum(abs(anorexia$weight_change) < 10)
cat("体重变化幅度≥10的人数:", large_change, "人 (", 
    round(large_change/nrow(anorexia)*100, 1), "%)\n", sep = "")
cat("体重变化幅度<10的人数:", small_change, "人 (", 
    round(small_change/nrow(anorexia)*100, 1), "%)\n", sep = "")

# ============================================
# 5. 数据可视化
# ============================================
library(ggplot2)

# 直方图
p1 <- ggplot(anorexia, aes(x = weight_change)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(weight_change)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(weight_change)), color = "green", linetype = "dashed", linewidth = 1) +
  labs(title = "29名厌食症女孩体重变化分布",
       x = "体重变化 (kg)",
       y = "人数",
       subtitle = "红虚线:均值(2.36kg), 绿虚线:中位数(1.4kg)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

print(p1)

# 箱线图
p2 <- ggplot(anorexia, aes(y = weight_change)) +
  geom_boxplot(fill = "lightblue", color = "black", width = 0.5) +
  geom_jitter(aes(x = 0), width = 0.1, alpha = 0.6, size = 3, color = "steelblue") +
  labs(title = "体重变化箱线图及散点",
       y = "体重变化 (kg)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())

print(p2)

# ============================================
# 6. 分组统计分析
# ============================================
cat("\n=== 分组统计 ===\n")

# 按体重变化方向分组
positive_data <- anorexia$weight_change[anorexia$weight_change > 0]
negative_data <- anorexia$weight_change[anorexia$weight_change < 0]

cat("\n体重增加的患者统计：\n")
cat("  人数:", length(positive_data), "\n")
cat("  平均增重:", round(mean(positive_data), 2), "kg\n")
cat("  中位数:", median(positive_data), "kg\n")
cat("  标准差:", round(sd(positive_data), 2), "kg\n")
cat("  范围:", min(positive_data), "~", max(positive_data), "kg\n")

cat("\n体重减少的患者统计：\n")
cat("  人数:", length(negative_data), "\n")
cat("  平均减重:", round(mean(negative_data), 2), "kg\n")
cat("  中位数:", median(negative_data), "kg\n")
cat("  标准差:", round(sd(negative_data), 2), "kg\n")
cat("  范围:", min(negative_data), "~", max(negative_data), "kg\n")

# ============================================
# 7. 分布特征分析
# ============================================
cat("\n=== 分布特征 ===\n")

# 峰度和偏度（需要moments包或计算）
mean_val <- mean(anorexia$weight_change)
sd_val <- sd(anorexia$weight_change)
n <- length(anorexia$weight_change)

# 计算偏度（Skewness）
skewness <- sum((anorexia$weight_change - mean_val)^3) / (n * sd_val^3)
cat("偏度 (Skewness):", round(skewness, 3), 
    ifelse(skewness > 0.5, "（右偏：正常值较多）", 
           ifelse(skewness < -0.5, "（左偏：异常值较多）", "（近似对称）")), "\n")

# 计算峰度（Kurtosis）
kurtosis <- sum((anorexia$weight_change - mean_val)^4) / (n * sd_val^4) - 3
cat("峰度 (Kurtosis):", round(kurtosis, 3), 
    ifelse(kurtosis > 0, "（比正态分布尖峰）", "（比正态分布平缓）"), "\n")

# 变异系数（Coefficient of Variation）
cv <- (sd_val / abs(mean_val)) * 100
cat("变异系数 (CV):", round(cv, 2), "%\n")

# ============================================
# 8. 离群值检测
# ============================================
cat("\n=== 离群值检测 ===\n")

Q1 <- quantile(anorexia$weight_change, 0.25)
Q3 <- quantile(anorexia$weight_change, 0.75)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR

cat("第一四分位数 (Q1):", Q1, "\n")
cat("第三四分位数 (Q3):", Q3, "\n")
cat("四分位距 (IQR):", IQR, "\n")
cat("下限:", lower_fence, "\n")
cat("上限:", upper_fence, "\n")

outliers <- anorexia$weight_change[anorexia$weight_change < lower_fence | anorexia$weight_change > upper_fence]
if(length(outliers) > 0) {
  cat("检测到", length(outliers), "个离群值:", round(outliers, 2), "\n")
  outlier_ids <- which(anorexia$weight_change < lower_fence | anorexia$weight_change > upper_fence)
  cat("患者ID:", outlier_ids, "\n")
} else {
  cat("未检测到离群值\n")
}

# ============================================
# 9. 百分位数分析
# ============================================
cat("\n=== 百分位数分析 ===\n")
percentiles <- c(10, 25, 50, 75, 90)
for(p in percentiles) {
  val <- quantile(anorexia$weight_change, p/100)
  cat("第", p, "百分位数:", round(val, 2), "kg\n", sep = "")
}

# ============================================
# 10. 分组柱状图
# ============================================
p3 <- ggplot(anorexia, aes(x = factor(ifelse(weight_change > 0, "增重", "减重")), 
                             fill = factor(ifelse(weight_change > 0, "增重", "减重")))) +
  geom_bar(color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("增重" = "green", "减重" = "red")) +
  labs(title = "患者体重变化方向分布",
       x = "体重变化方向",
       y = "人数",
       fill = "类型") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 5) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "none")

print(p3)

# ============================================
# 11. Q-Q图（检查正态性）
# ============================================
p4 <- ggplot(anorexia, aes(sample = weight_change)) +
  stat_qq(color = "steelblue", size = 3, alpha = 0.7) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title = "Q-Q图（正态性检验）",
       x = "理论分位数",
       y = "样本分位数") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(p4)

# ============================================
# 12. 密度图
# ============================================
p5 <- ggplot(anorexia, aes(x = weight_change)) +
  geom_density(fill = "steelblue", alpha = 0.5, color = "black") +
  geom_rug(color = "steelblue", alpha = 0.7) +
  labs(title = "体重变化的概率密度分布",
       x = "体重变化 (kg)",
       y = "密度") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(p5)

# ============================================
# 13. 置信区间计算
# ============================================
cat("\n=== 95% 置信区间分析 ===\n")

n <- length(anorexia$weight_change)
mean_val <- mean(anorexia$weight_change)
sd_val <- sd(anorexia$weight_change)
se <- sd_val / sqrt(n)  # 标准误

# t检验的临界值（df = n-1 = 28, α = 0.05，双尾）
alpha <- 0.05
df <- n - 1
t_critical <- qt(1 - alpha/2, df)

# 计算置信区间
ci_lower <- mean_val - t_critical * se
ci_upper <- mean_val + t_critical * se

cat("样本量 (n):", n, "\n")
cat("样本均值:", round(mean_val, 3), "kg\n")
cat("样本标准差:", round(sd_val, 3), "kg\n")
cat("标准误 (SE):", round(se, 3), "kg\n")
cat("自由度 (df):", df, "\n")
cat("t临界值 (α=0.05, 双尾):", round(t_critical, 4), "\n")
cat("\n95% 置信区间:\n")
cat("  下限:", round(ci_lower, 3), "kg\n")
cat("  上限:", round(ci_upper, 3), "kg\n")
cat("  CI [", round(ci_lower, 3), ", ", round(ci_upper, 3), "]\n", sep = "")

# 解释
cat("\n解释：我们有95%的把握相信，整个厌食症患者群体的平均体重变化\n")
cat("在", round(ci_lower, 2), "kg 到", round(ci_upper, 2), "kg 之间。\n")

# ============================================
# 14. 分别计算增重和减重患者的置信区间
# ============================================
cat("\n=== 分组置信区间 ===\n")

# 增重患者的95%置信区间
n_pos <- length(positive_data)
mean_pos <- mean(positive_data)
sd_pos <- sd(positive_data)
se_pos <- sd_pos / sqrt(n_pos)
df_pos <- n_pos - 1
t_crit_pos <- qt(1 - alpha/2, df_pos)
ci_lower_pos <- mean_pos - t_crit_pos * se_pos
ci_upper_pos <- mean_pos + t_crit_pos * se_pos

cat("\n体重增加患者 (n=", n_pos, "):\n", sep = "")
cat("  平均增重:", round(mean_pos, 3), "kg\n")
cat("  标准差:", round(sd_pos, 3), "kg\n")
cat("  95% CI [", round(ci_lower_pos, 3), ", ", round(ci_upper_pos, 3), "]\n", sep = "")

# 减重患者的95%置信区间
n_neg <- length(negative_data)
mean_neg <- mean(negative_data)
sd_neg <- sd(negative_data)
se_neg <- sd_neg / sqrt(n_neg)
df_neg <- n_neg - 1
t_crit_neg <- qt(1 - alpha/2, df_neg)
ci_lower_neg <- mean_neg - t_crit_neg * se_neg
ci_upper_neg <- mean_neg + t_crit_neg * se_neg

cat("\n体重减少患者 (n=", n_neg, "):\n", sep = "")
cat("  平均减重:", round(mean_neg, 3), "kg\n")
cat("  标准差:", round(sd_neg, 3), "kg\n")
cat("  95% CI [", round(ci_lower_neg, 3), ", ", round(ci_upper_neg, 3), "]\n", sep = "")

# ============================================
# 15. 置信区间可视化
# ============================================
cat("\n生成置信区间可视化图表...\n")

# 创建置信区间数据框
ci_data <- data.frame(
  group = c("全体患者", "体重增加", "体重减少"),
  mean = c(mean_val, mean_pos, mean_neg),
  lower = c(ci_lower, ci_lower_pos, ci_lower_neg),
  upper = c(ci_upper, ci_upper_pos, ci_upper_neg),
  n = c(n, n_pos, n_neg)
)

p6 <- ggplot(ci_data, aes(x = reorder(group, mean), y = mean)) +
  geom_point(size = 4, color = "steelblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                color = "steelblue", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  coord_flip() +
  labs(title = "体重变化的95%置信区间",
       x = "",
       y = "体重变化 (kg)",
       subtitle = "点代表样本均值，竖线代表95%置信区间") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11))

print(p6)
