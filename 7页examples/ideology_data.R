# 政治意识形态数据分析
# 数据来源：社会调查问卷
# 变量说明：不同种族受访者对政治意识形态的回答分布

# ============================================
# 1. 数据创建
# ============================================

ideology <- data.frame(
  response = c(
    "1. 极端自由派",
    "2. 自由派",
    "3. 销售自由派",
    "4. 中间、中庸",
    "5. 销售保守派",
    "6. 保守派",
    "7. 极端保守派"
  ),
  black = c(16, 52, 42, 182, 43, 25, 11),
  white = c(73, 209, 190, 705, 260, 314, 84),
  hispanic = c(5, 49, 46, 155, 50, 50, 14)
)

# 添加总数列
ideology$total <- ideology$black + ideology$white + ideology$hispanic

# 查看数据
head(ideology)
summary(ideology[, 2:5])

# ============================================
# 2. 基础统计信息
# ============================================
cat("\n=== 基础统计信息 ===\n")
cat("受访总人数: ", sum(ideology$total), "\n")
cat("黑人受访者: ", sum(ideology$black), " (", 
    round(sum(ideology$black)/sum(ideology$total)*100, 1), "%)\n", sep = "")
cat("白人受访者: ", sum(ideology$white), " (", 
    round(sum(ideology$white)/sum(ideology$total)*100, 1), "%)\n", sep = "")
cat("西班牙裔受访者: ", sum(ideology$hispanic), " (", 
    round(sum(ideology$hispanic)/sum(ideology$total)*100, 1), "%)\n", sep = "")

# ============================================
# 3. 各族群的意识形态分布
# ============================================
cat("\n=== 各族群意识形态分布百分比 ===\n")

# 黑人
cat("\n黑人受访者分布:\n")
black_pct <- ideology$black / sum(ideology$black) * 100
for(i in 1:nrow(ideology)) {
  cat(sprintf("%s: %d (%.1f%%)\n", ideology$response[i], ideology$black[i], black_pct[i]))
}

# 白人
cat("\n白人受访者分布:\n")
white_pct <- ideology$white / sum(ideology$white) * 100
for(i in 1:nrow(ideology)) {
  cat(sprintf("%s: %d (%.1f%%)\n", ideology$response[i], ideology$white[i], white_pct[i]))
}

# 西班牙裔
cat("\n西班牙裔受访者分布:\n")
hispanic_pct <- ideology$hispanic / sum(ideology$hispanic) * 100
for(i in 1:nrow(ideology)) {
  cat(sprintf("%s: %d (%.1f%%)\n", ideology$response[i], ideology$hispanic[i], hispanic_pct[i]))
}

# ============================================
# 4. 意识形态的整体分布
# ============================================
cat("\n=== 意识形态整体分布 ===\n")

ideology_total <- ideology$total / sum(ideology$total) * 100
ideology_summary <- data.frame(
  ideology = ideology$response,
  count = ideology$total,
  percentage = round(ideology_total, 1)
)

print(ideology_summary)

# ============================================
# 5. 数据可视化
# ============================================
library(ggplot2)

# 堆积柱状图（种族比较）
p1 <- ggplot(ideology, aes(x = factor(response, levels = response))) +
  geom_col(aes(y = black, fill = "黑人"), alpha = 0.8) +
  geom_col(aes(y = white, fill = "白人"), alpha = 0.8) +
  geom_col(aes(y = hispanic, fill = "西班牙裔"), alpha = 0.8) +
  labs(title = "政治意识形态分布（按种族）",
       x = "意识形态",
       y = "人数",
       fill = "种族") +
  scale_fill_manual(values = c("黑人" = "#1f77b4", "白人" = "#ff7f0e", "西班牙裔" = "#2ca02c")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  )

print(p1)

# 分组柱状图对比
p2 <- ggplot(ideology) +
  geom_col(aes(x = factor(response, levels = response), y = black, fill = "黑人"), 
           position = position_dodge(width = 0.8), alpha = 0.8) +
  geom_col(aes(x = factor(response, levels = response), y = white, fill = "白人"), 
           position = position_dodge(width = 0.8), alpha = 0.8) +
  geom_col(aes(x = factor(response, levels = response), y = hispanic, fill = "西班牙裔"), 
           position = position_dodge(width = 0.8), alpha = 0.8) +
  labs(title = "政治意识形态分布（分组对比）",
       x = "意识形态",
       y = "人数",
       fill = "种族") +
  scale_fill_manual(values = c("黑人" = "#1f77b4", "白人" = "#ff7f0e", "西班牙裔" = "#2ca02c")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  )

print(p2)

# 百分比堆积条形图
ideology$black_pct <- ideology$black / ideology$total * 100
ideology$white_pct <- ideology$white / ideology$total * 100
ideology$hispanic_pct <- ideology$hispanic / ideology$total * 100

# 转换为长格式
ideology_long <- data.frame(
  response = rep(ideology$response, 3),
  race = rep(c("黑人", "白人", "西班牙裔"), each = nrow(ideology)),
  percentage = c(ideology$black_pct, ideology$white_pct, ideology$hispanic_pct)
)

p3 <- ggplot(ideology_long, aes(x = factor(response, levels = ideology$response), 
                                y = percentage, fill = race)) +
  geom_col(position = "stack") +
  labs(title = "各意识形态中的种族构成（绝对比例）",
       x = "意识形态",
       y = "百分比 (%)",
       fill = "种族") +
  scale_fill_manual(values = c("黑人" = "#1f77b4", "白人" = "#ff7f0e", "西班牙裔" = "#2ca02c")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  ) +
  ylim(0, 105)

print(p3)

# ============================================
# 6. 西班牙裔意识形态的95%置信区间分析
# ============================================
cat("\n=== 西班牙裔意识形态 95% 置信区间分析 ===\n")

# 西班牙裔数据
hispanic_data <- ideology$hispanic
scores <- 1:7

# 样本统计
n_hispanic <- sum(hispanic_data)
mean_hispanic <- sum(hispanic_data * scores) / n_hispanic
variance_hispanic <- sum(hispanic_data * (scores - mean_hispanic)^2) / n_hispanic
sd_hispanic <- sqrt(variance_hispanic)

# 标准误
se_hispanic <- sd_hispanic / sqrt(n_hispanic)

# 使用Z分布（样本量大）
alpha <- 0.05
z_critical <- qnorm(1 - alpha/2)

# 计算置信区间
ci_lower_z <- mean_hispanic - z_critical * se_hispanic
ci_upper_z <- mean_hispanic + z_critical * se_hispanic

cat("\n--- 基本统计信息 ---\n")
cat("样本量 (n):", n_hispanic, "\n")
cat("样本均值 (意识形态得分):", round(mean_hispanic, 4), "\n")
cat("样本标准差:", round(sd_hispanic, 4), "\n")
cat("标准误 (SE):", round(se_hispanic, 4), "\n")

cat("\n--- 95% 置信区间 (使用Z分布) ---\n")
cat("Z临界值 (α=0.05, 双尾):", round(z_critical, 4), "\n")
cat("下限:", round(ci_lower_z, 4), "\n")
cat("上限:", round(ci_upper_z, 4), "\n")
cat("CI [", round(ci_lower_z, 4), ", ", round(ci_upper_z, 4), "]\n", sep = "")

cat("\n--- 解释 ---\n")
cat("西班牙裔受访者的平均意识形态得分为", round(mean_hispanic, 2), "\n")
cat("(1=极端自由派, 4=中间, 7=极端保守派)\n")
cat("我们有95%的把握相信，整个西班牙裔群体的平均意识形态得分\n")
cat("在", round(ci_lower_z, 2), "到", round(ci_upper_z, 2), "之间。\n")
cat("这表明西班牙裔整体倾向于中间偏自由派。\n")

# ============================================
# 7. 西班牙裔各意识形态成分的95%置信区间
# ============================================
cat("\n=== 西班牙裔各意识形态成分的95%置信区间 ===\n")

hispanic_proportions <- hispanic_data / n_hispanic

ci_results <- data.frame(
  ideology = ideology$response,
  count = hispanic_data,
  proportion = round(hispanic_proportions * 100, 2),
  se_prop = NA,
  ci_lower = NA,
  ci_upper = NA
)

for(i in 1:length(hispanic_data)) {
  p <- hispanic_proportions[i]
  # 标准误
  se_p <- sqrt(p * (1 - p) / n_hispanic)
  ci_results$se_prop[i] <- se_p
  
  # 使用正态近似
  ci_results$ci_lower[i] <- (p - z_critical * se_p) * 100
  ci_results$ci_upper[i] <- (p + z_critical * se_p) * 100
}

cat("\n")
for(i in 1:nrow(ci_results)) {
  cat(sprintf("%s: %.2f%% [%.2f%%, %.2f%%]\n", 
              ci_results$ideology[i], 
              ci_results$proportion[i],
              ci_results$ci_lower[i],
              ci_results$ci_upper[i]))
}

# ============================================
# 8. 各族群意识形态得分的95%置信区间对比
# ============================================
cat("\n=== 各族群意识形态得分的95%置信区间对比 ===\n")

# 黑人
black_data <- ideology$black
n_black <- sum(black_data)
mean_black <- sum(black_data * scores) / n_black
sd_black <- sqrt(sum(black_data * (scores - mean_black)^2) / n_black)
se_black <- sd_black / sqrt(n_black)
ci_lower_black <- mean_black - z_critical * se_black
ci_upper_black <- mean_black + z_critical * se_black

# 白人
white_data <- ideology$white
n_white <- sum(white_data)
mean_white <- sum(white_data * scores) / n_white
sd_white <- sqrt(sum(white_data * (scores - mean_white)^2) / n_white)
se_white <- sd_white / sqrt(n_white)
ci_lower_white <- mean_white - z_critical * se_white
ci_upper_white <- mean_white + z_critical * se_white

comparison <- data.frame(
  race = c("黑人", "白人", "西班牙裔"),
  n = c(n_black, n_white, n_hispanic),
  mean = round(c(mean_black, mean_white, mean_hispanic), 4),
  sd = round(c(sd_black, sd_white, sd_hispanic), 4),
  ci_lower = round(c(ci_lower_black, ci_lower_white, ci_lower_z), 4),
  ci_upper = round(c(ci_upper_black, ci_upper_white, ci_upper_z), 4)
)

print(comparison)

# ============================================
# 9. 族群比较可视化
# ============================================

# 置信区间对比图表
comparison_ci <- data.frame(
  race = c("黑人", "白人", "西班牙裔"),
  mean = c(mean_black, mean_white, mean_hispanic),
  lower = c(ci_lower_black, ci_lower_white, ci_lower_z),
  upper = c(ci_upper_black, ci_upper_white, ci_upper_z),
  n = c(n_black, n_white, n_hispanic)
)

p4 <- ggplot(comparison_ci, aes(x = reorder(race, mean), y = mean)) +
  geom_point(size = 5, color = "steelblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                color = "steelblue", linewidth = 1.2) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red", alpha = 0.5, 
             linewidth = 1) +
  scale_y_continuous(breaks = 1:7, 
                     labels = c("极端\n自由派", "自由派", "销售\n自由派", 
                               "中间\n中庸", "销售\n保守派", "保守派", "极端\n保守派")) +
  coord_flip() +
  labs(title = "各族群政治意识形态的95%置信区间对比",
       x = "族群",
       y = "意识形态得分",
       subtitle = "点代表样本均值，竖线代表95%置信区间\n红虚线表示中间位置(4)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 11)
  )

print(p4)

# 西班牙裔各成分的置信区间柱状图
ci_results_sorted <- ci_results[order(ci_results$proportion, decreasing = TRUE), ]

p5 <- ggplot(ci_results_sorted, aes(x = reorder(ideology, proportion), y = proportion)) +
  geom_col(fill = "steelblue", alpha = 0.7, color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, 
                color = "darkred", linewidth = 1, alpha = 0.8) +
  coord_flip() +
  labs(title = "西班牙裔政治意识形态的95%置信区间",
       x = "意识形态",
       y = "比例 (%)",
       subtitle = "柱高表示样本比例，误差线表示95%置信区间") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

print(p5)
