# ╔═══════════════════════════════════════════════════════════════════╗
# ║     对图中7行数据分别进行7次贝叶斯分析                            ║
# ║     每行数据为独立的实验，得到独立的后验分布和HDI              ║
# ║                                                                   ║
# ║ 对比Prior 0.5（无信息先验）和Prior 0.75（有信息先验）           ║
# ╚═══════════════════════════════════════════════════════════════════╝

library(dplyr)

cat("\n╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║   7次独立贝叶斯分析 - 对图中7行数据分别分析                      ║\n")
cat("║   Prior 0.5 (无信息) vs Prior 0.75 (有信息)                   ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

# ========== 图中的7行原始数据 ==========
# 这些是图中显示的原始实验数据
raw_data <- data.frame(
  exp_name = c("实验1", "重复1", "重复2", "重复3", "重复4", "重复5", "重复6", "重复7"),
  n = c(28, 25, 12, 12, 11, 10, 10, 5),
  x = c(17, 19, 8, 9, 8, 7, 8, 4),
  stringsAsFactors = FALSE
)

cat("【原始实验数据（来自图表）】\n")
print(raw_data)
cat("\n")

# ========== 两个先验设置 ==========
prior_05_a <- 1
prior_05_b <- 1
prior_75_a <- 6
prior_75_b <- 2

cat("【贝叶斯先验设置】\n")
cat(sprintf("Prior 0.5: Beta(%.0f, %.0f) - 无信息先验，Mean = 0.50\n", prior_05_a, prior_05_b))
cat(sprintf("Prior 0.75: Beta(%.0f, %.0f) - 有信息先验，Mean = 0.75\n\n", prior_75_a, prior_75_b))

# ========== 进行7次贝叶斯分析 ==========
# 每一行数据进行一次独立的贝叶斯分析

bayesian_results <- raw_data %>%
  mutate(
    # ===== 第一次贝叶斯分析（Prior 0.5）=====
    post_05_a = prior_05_a + x,
    post_05_b = prior_05_b + (n - x),
    hdi_05_lower = qbeta(0.025, post_05_a, post_05_b),
    hdi_05_upper = qbeta(0.975, post_05_a, post_05_b),
    hdi_05_width = hdi_05_upper - hdi_05_lower,
    
    # ===== 第二次贝叶斯分析（Prior 0.75）=====
    post_75_a = prior_75_a + x,
    post_75_b = prior_75_b + (n - x),
    hdi_75_lower = qbeta(0.025, post_75_a, post_75_b),
    hdi_75_upper = qbeta(0.975, post_75_a, post_75_b),
    hdi_75_width = hdi_75_upper - hdi_75_lower,
    
    # 样本比例
    p_hat = x / n,
    p_pct = sprintf("%.2f%%", p_hat * 100)
  )

# ========== 输出完整的分析结果表 ==========
cat("╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║              表3：CMU实验 - 7次贝叶斯分析汇总                          ║\n")
cat("╚════════════════════════════════════════════════════════════════════════╝\n\n")

summary_table <- bayesian_results %>%
  select(exp_name, n, x, p_pct, hdi_05_lower, hdi_05_upper, hdi_75_lower, hdi_75_upper) %>%
  mutate(
    hdi_05 = sprintf("%.3f - %.3f", hdi_05_lower, hdi_05_upper),
    hdi_75 = sprintf("%.3f - %.3f", hdi_75_lower, hdi_75_upper)
  ) %>%
  select(exp_name, n, x, p_pct, hdi_05, hdi_75)

colnames(summary_table) <- c("表号", "总数(N)", "快数(x)", "x/N", "HDI(95%) Prior 0.5", "HDI(95%) Prior 0.75")

# 打印表格
print(summary_table, right = FALSE)

# 保存为CSV
write.csv(summary_table, "表3_7次贝叶斯分析汇总.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✓ 已保存：表3_7次贝叶斯分析汇总.csv\n\n")

# ========== 详细分析表 ==========
cat("╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║           详细分析表：7次贝叶斯分析的完整统计结果                      ║\n")
cat("╚════════════════════════════════════════════════════════════════════════╝\n\n")

detailed_table <- bayesian_results %>%
  select(
    exp_name, n, x, p_pct,
    post_05_a, post_05_b, hdi_05_lower, hdi_05_upper, hdi_05_width,
    post_75_a, post_75_b, hdi_75_lower, hdi_75_upper, hdi_75_width
  ) %>%
  mutate(
    hdi_05 = sprintf("%.3f - %.3f", hdi_05_lower, hdi_05_upper),
    hdi_75 = sprintf("%.3f - %.3f", hdi_75_lower, hdi_75_upper)
  ) %>%
  select(
    exp_name, n, x, p_pct,
    post_05_a, post_05_b, hdi_05, hdi_05_width,
    post_75_a, post_75_b, hdi_75, hdi_75_width
  )

colnames(detailed_table) <- c(
  "实验号", "样本量(N)", "成功数(x)", "比例(%)",
  "后验α(0.5)", "后验β(0.5)", "HDI(95%)(0.5)", "宽度(0.5)",
  "后验α(0.75)", "后验β(0.75)", "HDI(95%)(0.75)", "宽度(0.75)"
)

print(detailed_table, right = FALSE)

# 保存详细表
write.csv(detailed_table, "表_7次贝叶斯分析详细.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✓ 已保存：表_7次贝叶斯分析详细.csv\n\n")

# ========== HDI宽度对比分析 ==========
cat("╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║              HDI宽度对比：衡量不确定性大小                            ║\n")
cat("╚════════════════════════════════════════════════════════════════════════╝\n\n")

width_comparison <- bayesian_results %>%
  select(exp_name, n, x, hdi_05_width, hdi_75_width) %>%
  mutate(
    reduction = hdi_05_width - hdi_75_width,
    reduction_pct = sprintf("%.2f%%", (reduction / hdi_05_width) * 100)
  )

cat("HDI宽度对比（越窄代表不确定性越小）：\n\n")
cat("| 实验 | N | x | Prior0.5宽度 | Prior0.75宽度 | 缩减幅度 | 缩减百分比 |\n")
cat("|------|---|---|------------|------------|--------|----------|\n")
for(i in 1:nrow(width_comparison)) {
  with(width_comparison[i,], {
    cat(sprintf("| %s | %d | %d | %.4f | %.4f | %.4f | %s |\n",
                exp_name, n, x, hdi_05_width, hdi_75_width, reduction, reduction_pct))
  })
}

cat("\n")
avg_width_05 <- mean(bayesian_results$hdi_05_width)
avg_width_75 <- mean(bayesian_results$hdi_75_width)
avg_reduction <- avg_width_05 - avg_width_75
avg_reduction_pct <- (avg_reduction / avg_width_05) * 100

cat(sprintf("平均HDI宽度(Prior 0.5)：%.4f\n", avg_width_05))
cat(sprintf("平均HDI宽度(Prior 0.75)：%.4f\n", avg_width_75))
cat(sprintf("平均缩减幅度：%.4f (%.2f%%)\n\n", avg_reduction, avg_reduction_pct))

# ========== 总体统计汇总 ==========
cat("╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    7次贝叶斯分析总体统计                              ║\n")
cat("╚════════════════════════════════════════════════════════════════════════╝\n\n")

cat(sprintf("总实验次数：%d\n", nrow(bayesian_results)))
cat(sprintf("总样本量(ΣN)：%d\n", sum(bayesian_results$n)))
cat(sprintf("总成功次数(Σx)：%d\n", sum(bayesian_results$x)))
cat(sprintf("综合成功率：%.2f%%\n\n", (sum(bayesian_results$x) / sum(bayesian_results$n)) * 100))

cat("【关键发现】\n")
cat("✓ 7次独立的贝叶斯分析已完成\n")
cat("✓ 每次分析都使用两个不同的先验（Prior 0.5 和 Prior 0.75）\n")
cat("✓ Prior 0.75（有信息先验）的HDI平均更窄\n")
cat(sprintf("✓ HDI宽度平均缩减 %.2f%%\n", avg_reduction_pct))
cat("✓ 有信息先验包含了先前的知识，导致不确定性降低\n\n")

cat("════════════════════════════════════════════════════════════════════════\n\n")
cat("✨ 7次贝叶斯分析完成！\n")
cat("📊 所有表格已生成并保存\n\n")
