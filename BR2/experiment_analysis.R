# 实验：C程序 vs Python程序性能比较
# 实验设计：28个Python程序，用C重写，对比性能
# 结果：C程序比Python快的案例数 = 17

# 数据设定
n <- 28  # 总实验数
x <- 17  # C程序比Python快的次数
p0 <- 0.5  # 零假设：两种语言性能无差异，各占50%

# ============= 二项分布检验 =============

# 方法1：使用二项检验（精确检验）
binom_test <- binom.test(x, n, p0, alternative = "two.sided")
cat("二项检验（精确检验）结果：\n")
cat("p值：", binom_test$p.value, "\n")
cat("95% 置信区间：", binom_test$conf.int, "\n\n")

# 方法2：正态近似（z检验）
# 样本比例
p_hat <- x / n  # 样本比例
cat("样本比例（C程序更快）：", p_hat, "\n")
cat("样本比例（百分比）：", p_hat * 100, "%\n\n")

# z值计算：z = (p_hat - p0) / sqrt(p0*(1-p0)/n)
se <- sqrt(p0 * (1 - p0) / n)  # 标准误
z_value <- (p_hat - p0) / se

cat("=== 正态近似Z检验 ===\n")
cat("标准误（SE）：", se, "\n")
cat("z值：", z_value, "\n")

# 双尾p值
p_value <- 2 * pnorm(-abs(z_value))  # 双尾
cat("p值（双尾）：", p_value, "\n\n")

# ============= 可视化：正态分布图 =============
library(ggplot2)

# 生成正态分布曲线数据
x_seq <- seq(-4, 4, length.out = 300)
y_density <- dnorm(x_seq, mean = 0, sd = 1)
df_norm <- data.frame(x = x_seq, y = y_density)

# 设置z值
z_actual <- round(z_value, 4)
z_critical <- 1.65

# 创建绘图，添加详细标注
p <- ggplot(df_norm, aes(x = x, y = y)) +
  geom_line(color = "steelblue", linewidth = 1) +
  # 拒绝域（z > 1.65或z < -1.65）
  geom_area(data = df_norm[df_norm$x >= z_critical | df_norm$x <= -z_critical, ],
            aes(x = x, y = y), fill = "red", alpha = 0.3) +
  # 接受域
  geom_area(data = df_norm[df_norm$x < z_critical & df_norm$x > -z_critical, ],
            aes(x = x, y = y), fill = "lightblue", alpha = 0.3) +
  # 实验的z值线
  geom_vline(xintercept = z_actual, color = "darkred", linetype = "solid", linewidth = 1.2) +
  geom_vline(xintercept = -z_actual, color = "darkred", linetype = "solid", linewidth = 1.2) +
  # 临界值线
  geom_vline(xintercept = z_critical, color = "darkgreen", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = -z_critical, color = "darkgreen", linetype = "dashed", linewidth = 1) +
  
  # 标注实验z值
  annotate("text", x = z_actual, y = 0.42, label = paste0("z = ", z_actual),
           color = "darkred", fontface = "bold", size = 4.5, hjust = -0.1) +
  annotate("text", x = -z_actual, y = 0.42, label = paste0("z = ", -z_actual),
           color = "darkred", fontface = "bold", size = 4.5, hjust = 1.1) +
  
  # 标注临界值
  annotate("text", x = z_critical, y = 0.38, label = paste0("z = ", z_critical),
           color = "darkgreen", fontface = "bold", size = 4.5, hjust = -0.1) +
  annotate("text", x = -z_critical, y = 0.38, label = paste0("z = ", -z_critical),
           color = "darkgreen", fontface = "bold", size = 4.5, hjust = 1.1) +
  
  # 标注区域
  annotate("text", x = 0, y = 0.25, label = "接受域\n(p > 0.05)",
           color = "darkblue", fontface = "bold", size = 4.5, hjust = 0.5) +
  annotate("text", x = 2.8, y = 0.08, label = "拒绝域\n(p < 0.05)",
           color = "darkred", fontface = "bold", size = 4, hjust = 0.5) +
  annotate("text", x = -2.8, y = 0.08, label = "拒绝域\n(p < 0.05)",
           color = "darkred", fontface = "bold", size = 4, hjust = 0.5) +
  
  labs(title = "C语言 vs Python性能比较：正态分布Z检验",
       subtitle = sprintf("n=28, x=17 | 实验Z值=%.4f | 临界值(α=0.10)=±%.2f", z_actual, z_critical),
       x = "Z值",
       y = "概率密度",
       caption = "——— 实验Z值  - - - - 临界值  |  蓝色：接受域  红色：拒绝域") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10, color = "gray50"),
    axis.title = element_text(size = 11)
  )

print(p)

# 保存图表
ggsave("experiment_normal_distribution.png", p, width = 12, height = 6.5, dpi = 300)

# ============= 结论总结 =============
cat("\n========== 实验结论 ==========\n")
cat(sprintf("实验总数：%d\n", n))
cat(sprintf("C程序较快次数：%d（%.2f%%）\n", x, p_hat * 100))
cat(sprintf("Python程序较快次数：%d（%.2f%%）\n", n - x, (1 - p_hat) * 100))
cat("\n统计检验结果：\n")
cat(sprintf("标准误（SE）= %.6f\n", se))
cat(sprintf("Z值 = %.4f\n", z_actual))
cat(sprintf("p值（双尾）= %.4f\n", p_value))
cat(sprintf("临界值（z_0.05）= ±1.96\n"))
cat(sprintf("临界值（z_0.10）= ±1.65\n"))

cat("\n决策：\n")
if (p_value < 0.05) {
  cat("✓ 在显著性水平 α=0.05 下，拒绝零假设\n")
  cat("  C程序的性能显著优于Python程序\n")
} else {
  cat("✗ 在显著性水平 α=0.05 下，无法拒绝零假设\n")
  cat("  C程序与Python程序的性能无显著差异\n")
}

cat("\n说明：\n")
cat("- 实验Z值（1.1339）< 临界值（1.65）\n")
cat("- 虽然C程序比例为60.71%，高于50%，但差异不具有统计显著性\n")
cat("- 这种差异可能是由随机波动造成，而非C语言的真实优势\n")
cat("=============================\n")
