# ╔═══════════════════════════════════════════════════════════════════╗
# ║     Erdogmus 7次重复实验 - 完整综合分析                           ║
# ║                                                                   ║
# ║ 包含三部分：                                                     ║
# ║ 1. 频率主义Z检验分析                                             ║
# ║ 2. 贝叶斯Beta-Bernoulli分析                                      ║
# ║ 3. 综合结果表格汇总                                              ║
# ║                                                                   ║
# ║ 依赖包：ggplot2, gridExtra, dplyr                                ║
# ╚═══════════════════════════════════════════════════════════════════╝

library(ggplot2)
library(gridExtra)
library(dplyr)

cat("\n╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║           Erdogmus 7次重复实验 - 完整综合分析                   ║\n")
cat("║                                                                   ║\n")
cat("║  分析内容：                                                     ║\n")
cat("║  1️⃣  频率主义Z检验（Z值、p值、显著性判断）                   ║\n")
cat("║  2️⃣  贝叶斯分析（后验分布、Mode、95% HDI）                   ║\n")
cat("║  3️⃣  综合表格（包含所有关键统计量）                          ║\n")
cat("║  4️⃣  可视化图表（后验分布对比）                              ║\n")
cat("║                                                                   ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

# ========== 实验数据 ==========
experiments_data <- data.frame(
  exp_name = c("实验1", "重复1", "重复2", "重复3", "重复4", "重复5", "重复6", "重复7"),
  n = c(28, 25, 12, 12, 11, 10, 10, 5),
  x = c(17, 19, 8, 9, 8, 7, 8, 4),
  stringsAsFactors = FALSE
)

# 参数设定
p0 <- 0.5  # 零假设概率（无差异）
a_prior <- 1  # Beta先验参数（无信息先验）
b_prior <- 1

# ════════════════════════════════════════════════════════════════════
# 第一部分：频率主义Z检验分析
# ════════════════════════════════════════════════════════════════════

cat("■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■\n")
cat("第一部分：频率主义Z检验分析\n")
cat("■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■\n\n")

frequentist_results <- experiments_data %>%
  mutate(
    # 基本统计
    p_hat = x / n,
    p_pct = sprintf("%.2f%%", p_hat * 100),
    
    # Z检验
    se = sqrt(p0 * (1 - p0) / n),
    z_value = (p_hat - p0) / se,
    
    # p值（右尾）- 单侧检验（C比Python快）
    p_value_right = 1 - pnorm(z_value),
    
    # p值（双尾）- 仅供参考
    p_value_two = 2 * (1 - pnorm(abs(z_value))),
    
    # 显著性判断
    significant = ifelse(p_value_right < 0.05, "Yes", "No")
  )

# 打印频率主义结果
cat("频率主义Z检验结果表：\n\n")
freq_table <- frequentist_results %>%
  select(exp_name, n, x, p_pct, z_value, p_value_right, significant)
colnames(freq_table) <- c("表号", "N", "x", "x/N", "Z值", "p值(右尾)", "显著?")
print(freq_table)

cat("\n频率主义统计汇总：\n")
cat(sprintf("  Z值范围：%.4f ~ %.4f\n", min(frequentist_results$z_value), max(frequentist_results$z_value)))
cat(sprintf("  p值范围：%.4f ~ %.4f\n", min(frequentist_results$p_value_right), max(frequentist_results$p_value_right)))
cat(sprintf("  显著的实验：%d/8 (%.1f%%)\n", sum(frequentist_results$significant == "Yes"), 
            sum(frequentist_results$significant == "Yes")/8*100))
cat(sprintf("  平均Z值：%.4f\n", mean(frequentist_results$z_value)))
cat(sprintf("  平均p值：%.4f\n\n", mean(frequentist_results$p_value_right)))

# ════════════════════════════════════════════════════════════════════
# 第二部分：贝叶斯分析
# ════════════════════════════════════════════════════════════════════

cat("■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■\n")
cat("第二部分：贝叶斯Beta-Bernoulli分析\n")
cat("■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■\n\n")

bayesian_results <- experiments_data %>%
  mutate(
    # 后验参数
    a_posterior = a_prior + x,
    b_posterior = b_prior + (n - x),
    
    # 后验统计量
    posterior_mode = (a_posterior - 1) / (a_posterior + b_posterior - 2),
    posterior_mean = a_posterior / (a_posterior + b_posterior),
    
    # 后验HDI (95%)
    hdi_lower = qbeta(0.025, a_posterior, b_posterior),
    hdi_upper = qbeta(0.975, a_posterior, b_posterior),
    hdi_width = hdi_upper - hdi_lower,
    
    # 边际似然 p(D)
    p_D = beta(a_posterior, b_posterior) / beta(a_prior, b_prior)
  )

# 打印贝叶斯结果
cat("贝叶斯后验分析结果表：\n\n")
bayes_table <- bayesian_results %>%
  select(exp_name, n, x, a_posterior, b_posterior, posterior_mode, hdi_lower, hdi_upper)
colnames(bayes_table) <- c("表号", "N", "x", "后验a", "后验b", "Mode", "HDI_lower", "HDI_upper")
print(bayes_table)

cat("\n贝叶斯统计汇总：\n")
cat(sprintf("  Mode范围：%.4f ~ %.4f (%.2f%% ~ %.2f%%)\n", 
            min(bayesian_results$posterior_mode), max(bayesian_results$posterior_mode),
            min(bayesian_results$posterior_mode)*100, max(bayesian_results$posterior_mode)*100))
cat(sprintf("  Mode平均：%.4f (%.2f%%)\n", mean(bayesian_results$posterior_mode), 
            mean(bayesian_results$posterior_mode)*100))
cat(sprintf("  HDI平均宽度：%.4f\n", mean(bayesian_results$hdi_width)))
cat(sprintf("  HDI最窄：%.4f (N=%d)\n", min(bayesian_results$hdi_width), 
            bayesian_results$n[which.min(bayesian_results$hdi_width)]))
cat(sprintf("  HDI最宽：%.4f (N=%d)\n\n", max(bayesian_results$hdi_width),
            bayesian_results$n[which.max(bayesian_results$hdi_width)]))

# 逐个实验的详细输出
cat("各实验的贝叶斯分析详情：\n\n")
for(i in 1:nrow(bayesian_results)) {
  cat(sprintf("【%s】 N=%d, x=%d\n", bayesian_results$exp_name[i], 
              bayesian_results$n[i], bayesian_results$x[i]))
  cat(sprintf("  后验分布：Beta(%d, %d)\n", bayesian_results$a_posterior[i], 
              bayesian_results$b_posterior[i]))
  cat(sprintf("  Mode：%.4f  95%% HDI：[%.4f, %.4f]\n\n", 
              bayesian_results$posterior_mode[i],
              bayesian_results$hdi_lower[i],
              bayesian_results$hdi_upper[i]))
}

# ════════════════════════════════════════════════════════════════════
# 第三部分：综合结果表格
# ════════════════════════════════════════════════════════════════════

cat("■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■\n")
cat("第三部分：综合结果表格\n")
cat("■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■\n\n")

# 合并频率主义和贝叶斯结果
combined_results <- data.frame(
  "表号" = experiments_data$exp_name,
  "N" = experiments_data$n,
  "x" = experiments_data$x,
  "x/N" = frequentist_results$p_pct,
  "Z值" = round(frequentist_results$z_value, 4),
  "p值" = round(frequentist_results$p_value_right, 4),
  "后验Beta" = sprintf("(%d,%d)", bayesian_results$a_posterior, bayesian_results$b_posterior),
  "Mode" = round(bayesian_results$posterior_mode, 4),
  "95%HDI" = sprintf("[%.3f,%.3f]", bayesian_results$hdi_lower, bayesian_results$hdi_upper),
  check.names = FALSE
)

cat("完整综合表格：\n\n")
print(combined_results)

# 保存综合表格为CSV
write.csv(combined_results, "Erdogmus_综合分析结果表.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✓ 已保存为CSV：Erdogmus_综合分析结果表.csv\n\n")

# 详细表格（包含所有中间量）
detailed_results <- data.frame(
  "表号" = experiments_data$exp_name,
  "N" = experiments_data$n,
  "x" = experiments_data$x,
  "样本比例" = frequentist_results$p_pct,
  "SE" = round(frequentist_results$se, 6),
  "Z值" = round(frequentist_results$z_value, 4),
  "p值(右尾)" = round(frequentist_results$p_value_right, 4),
  "p值(双尾)" = round(frequentist_results$p_value_two, 4),
  "显著?" = frequentist_results$significant,
  "后验Beta" = sprintf("(%d,%d)", bayesian_results$a_posterior, bayesian_results$b_posterior),
  "Mode" = round(bayesian_results$posterior_mode, 4),
  "Mode%" = sprintf("%.2f%%", bayesian_results$posterior_mode * 100),
  "HDI下界" = round(bayesian_results$hdi_lower, 4),
  "HDI上界" = round(bayesian_results$hdi_upper, 4),
  "HDI宽度" = round(bayesian_results$hdi_width, 4),
  "p(D)" = sprintf("%.2e", bayesian_results$p_D),
  check.names = FALSE
)

cat("详细数据表（包含所有中间量）：\n\n")
print(detailed_results)

write.csv(detailed_results, "Erdogmus_详细分析数据.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✓ 已保存为CSV：Erdogmus_详细分析数据.csv\n\n")

# ════════════════════════════════════════════════════════════════════
# 第四部分：可视化
# ════════════════════════════════════════════════════════════════════

cat("■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■\n")
cat("第四部分：生成可视化图表\n")
cat("■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■\n\n")

# 生成后验分布数据
theta <- seq(0, 1, length.out = 1000)
posterior_data_list <- list()

for(i in 1:nrow(bayesian_results)) {
  posterior <- dbeta(theta, bayesian_results$a_posterior[i], bayesian_results$b_posterior[i])
  posterior_data_list[[i]] <- data.frame(
    theta = theta,
    density = posterior,
    exp_name = bayesian_results$exp_name[i]
  )
}

posterior_data <- do.call(rbind, posterior_data_list)

# 图1：后验分布叠加
p1 <- ggplot(posterior_data, aes(x = theta, y = density, color = exp_name, fill = exp_name)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_area(alpha = 0.1) +
  xlim(0, 1) +
  labs(title = "所有实验的后验分布对比（叠加）",
       x = "θ (C程序较快的概率)",
       y = "概率密度",
       color = "实验",
       fill = "实验") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "right")

# 图2：后验分布分面
p2 <- ggplot(posterior_data, aes(x = theta, y = density, fill = exp_name)) +
  geom_area(alpha = 0.7) +
  geom_line(color = "black", linewidth = 0.5) +
  facet_wrap(~exp_name, ncol = 4) +
  xlim(0, 1) +
  labs(title = "每次实验的后验分布（分面）",
       x = "θ",
       y = "密度") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 9))

# 图3：Mode对比
p3 <- ggplot(bayesian_results, aes(x = reorder(exp_name, posterior_mode), 
                                   y = posterior_mode, fill = posterior_mode)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", linewidth = 1, alpha = 0.5) +
  scale_fill_gradient(low = "#FFA07A", high = "#FF4500") +
  coord_flip() +
  labs(title = "各实验的后验Mode对比",
       x = "实验",
       y = "后验Mode（C程序较快的概率）") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right")

# 图4：Z值对比
p4 <- ggplot(frequentist_results, aes(x = reorder(exp_name, z_value), 
                                      y = z_value, fill = z_value)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = 1.645, linetype = "dashed", color = "red", linewidth = 1, alpha = 0.5) +
  scale_fill_gradient(low = "#87CEEB", high = "#4169E1") +
  coord_flip() +
  labs(title = "各实验的Z值对比",
       x = "实验",
       y = "Z值") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right")

# 生成并显示所有图表
cat("生成后验分布叠加图...\n")
print(p1)
ggsave("01_后验分布叠加.png", p1, width = 12, height = 6, dpi = 300)

cat("生成后验分布分面图...\n")
print(p2)
ggsave("02_后验分布分面.png", p2, width = 14, height = 8, dpi = 300)

cat("生成Mode对比图...\n")
print(p3)
ggsave("03_Mode对比.png", p3, width = 10, height = 6, dpi = 300)

cat("生成Z值对比图...\n")
print(p4)
ggsave("04_Z值对比.png", p4, width = 10, height = 6, dpi = 300)

# 综合图表
combined_plot <- gridExtra::grid.arrange(p1, p3, p4, ncol = 1, heights = c(1.2, 1, 1))
cat("生成综合汇总图...\n")
print(combined_plot)
ggsave("05_综合汇总图.png", combined_plot, width = 12, height = 12, dpi = 300)

cat("\n✓ 所有可视化图表已生成并保存\n\n")

# ════════════════════════════════════════════════════════════════════
# 总结
# ════════════════════════════════════════════════════════════════════

cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                     分析完成总结                                  ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

cat("生成的输出文件：\n\n")
cat("📊 CSV数据表：\n")
cat("  ✓ Erdogmus_综合分析结果表.csv (推荐)\n")
cat("  ✓ Erdogmus_详细分析数据.csv\n\n")

cat("📈 可视化图表：\n")
cat("  ✓ 01_后验分布叠加.png\n")
cat("  ✓ 02_后验分布分面.png\n")
cat("  ✓ 03_Mode对比.png\n")
cat("  ✓ 04_Z值对比.png\n")
cat("  ✓ 05_综合汇总图.png\n\n")

cat("主要统计结果：\n\n")
cat("频率主义（Z检验）：\n")
cat(sprintf("  • Z值平均：%.4f\n", mean(frequentist_results$z_value)))
cat(sprintf("  • p值平均：%.4f\n", mean(frequentist_results$p_value_right)))
cat(sprintf("  • 显著实验：%d/8 (%.1f%%)\n\n", 
            sum(frequentist_results$significant == "Yes"),
            sum(frequentist_results$significant == "Yes")/8*100))

cat("贝叶斯分析：\n")
cat(sprintf("  • Mode平均：%.4f (%.2f%%)\n", mean(bayesian_results$posterior_mode),
            mean(bayesian_results$posterior_mode)*100))
cat(sprintf("  • HDI平均宽度：%.4f\n", mean(bayesian_results$hdi_width)))
cat(sprintf("  • Mode范围：%.4f ~ %.4f\n\n", 
            min(bayesian_results$posterior_mode),
            max(bayesian_results$posterior_mode)))

cat("════════════════════════════════════════════════════════════════════\n")
cat("✨ 完整分析已完成！所有结果已保存。\n\n")
