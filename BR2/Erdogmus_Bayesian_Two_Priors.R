# ╔═══════════════════════════════════════════════════════════════════╗
# ║     Erdogmus 7次重复实验 - 贝叶斯分析（两个先验对比）            ║
# ║                                                                   ║
# ║ 对比Prior 0.5（无信息先验）和Prior 0.75（有信息先验）           ║
# ║ 计算每个实验的95% HDI                                           ║
# ╚═══════════════════════════════════════════════════════════════════╝

library(dplyr)

cat("\n╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║   贝叶斯分析：两个先验对比 - Erdogmus 7次重复实验             ║\n")
cat("║   Prior 0.5 (无信息) vs Prior 0.75 (有信息)                   ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

# ========== 实验数据 ==========
experiments <- data.frame(
  exp_name = c("实验1", "重复1", "重复2", "重复3", "重复4", "重复5", "重复6", "重复7"),
  n = c(28, 25, 12, 12, 11, 10, 10, 5),
  x = c(17, 19, 8, 9, 8, 7, 8, 4),
  stringsAsFactors = FALSE
)

# ========== 两个先验设置 ==========
# Prior 0.5: 无信息先验 - Beta(1, 1)
prior_05_a <- 1
prior_05_b <- 1

# Prior 0.75: 有信息先验 - Beta(6, 2)
# Mean = 6/8 = 0.75, Mode = 5/6 ≈ 0.833
prior_75_a <- 6
prior_75_b <- 2

cat("先验设置：\n")
cat(sprintf("Prior 0.5: Beta(%.1f, %.1f) - 无信息先验\n", prior_05_a, prior_05_b))
cat(sprintf("  Mean = %.4f, Mode = %.4f\n", 
            prior_05_a/(prior_05_a+prior_05_b),
            (prior_05_a-1)/(prior_05_a+prior_05_b-2)))
cat(sprintf("\nPrior 0.75: Beta(%.1f, %.1f) - 有信息先验\n", prior_75_a, prior_75_b))
cat(sprintf("  Mean = %.4f, Mode = %.4f\n\n", 
            prior_75_a/(prior_75_a+prior_75_b),
            (prior_75_a-1)/(prior_75_a+prior_75_b-2)))

# ========== 计算两个先验的后验HDI ==========
results <- experiments %>%
  mutate(
    # Prior 0.5的后验
    post_05_a = prior_05_a + x,
    post_05_b = prior_05_b + (n - x),
    hdi_05_lower = qbeta(0.025, post_05_a, post_05_b),
    hdi_05_upper = qbeta(0.975, post_05_a, post_05_b),
    hdi_05 = sprintf("%.3f - %.3f", hdi_05_lower, hdi_05_upper),
    
    # Prior 0.75的后验
    post_75_a = prior_75_a + x,
    post_75_b = prior_75_b + (n - x),
    hdi_75_lower = qbeta(0.025, post_75_a, post_75_b),
    hdi_75_upper = qbeta(0.975, post_75_a, post_75_b),
    hdi_75 = sprintf("%.3f - %.3f", hdi_75_lower, hdi_75_upper),
    
    # 样本比例
    p_hat = x / n,
    p_pct = sprintf("%.2f%%", p_hat * 100)
  )

# ========== 创建表3：与用户图表格式完全匹配 ==========
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║              表3：CMU实验 - 贝叶斯分析（两个先验）              ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

table3 <- data.frame(
  "表号" = results$exp_name,
  "CMU实验" = "",
  "总数(N)" = results$n,
  "快数(x)" = results$x,
  "x/N" = results$p_pct,
  "HDI(95%) Prior 0.5" = results$hdi_05,
  "HDI(95%) Prior 0.75" = results$hdi_75,
  check.names = FALSE
)

print(table3)

write.csv(table3, "表3_贝叶斯两个先验对比.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✓ 已保存：表3_贝叶斯两个先验对比.csv\n\n")

# ========== 详细分析表 ==========
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║              详细分析：每个实验的完整贝叶斯统计                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

detailed <- data.frame(
  "表号" = results$exp_name,
  "N" = results$n,
  "x" = results$x,
  "x/N" = results$p_pct,
  "Prior0.5_a" = results$post_05_a,
  "Prior0.5_b" = results$post_05_b,
  "Prior0.5_HDI" = results$hdi_05,
  "Prior0.75_a" = results$post_75_a,
  "Prior0.75_b" = results$post_75_b,
  "Prior0.75_HDI" = results$hdi_75,
  check.names = FALSE
)

print(detailed)

write.csv(detailed, "表_贝叶斯详细数据两个先验.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✓ 已保存：表_贝叶斯详细数据两个先验.csv\n\n")

# ========== 统计汇总 ==========
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                    HDI宽度对比分析                              ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

summary_stats <- results %>%
  mutate(
    hdi_05_width = hdi_05_upper - hdi_05_lower,
    hdi_75_width = hdi_75_upper - hdi_75_lower,
    width_diff = hdi_75_width - hdi_05_width
  )

cat("HDI宽度（不确定性大小）：\n\n")
cat("| 实验 | Prior 0.5宽度 | Prior 0.75宽度 | 差异 |\n")
cat("|------|-------------|-------------|-------|\n")
for(i in 1:nrow(summary_stats)) {
  cat(sprintf("| %s | %.4f | %.4f | %+.4f |\n",
              summary_stats$exp_name[i],
              summary_stats$hdi_05_width[i],
              summary_stats$hdi_75_width[i],
              summary_stats$width_diff[i]))
}

cat("\n整体统计：\n")
cat(sprintf("Prior 0.5平均HDI宽度：%.4f\n", mean(summary_stats$hdi_05_width)))
cat(sprintf("Prior 0.75平均HDI宽度：%.4f\n", mean(summary_stats$hdi_75_width)))
cat(sprintf("平均差异：%.4f\n\n", mean(summary_stats$width_diff)))

cat("说明：\n")
cat("• Prior 0.75（有信息先验）通常导致更窄的HDI\n")
cat("• 因为有信息先验包含了关于参数的先前知识\n")
cat("• 特别是样本量小时，差异更明显\n")
cat("• Prior 0.5（无信息先验）更客观，完全由数据驱动\n\n")

cat("════════════════════════════════════════════════════════════════════\n\n")
cat("✨ 贝叶斯两个先验分析完成！\n")
cat("📊 所有表格已生成并保存\n\n")
