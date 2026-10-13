# ╔═══════════════════════════════════════════════════════════════════╗
# ║  贝叶斯分析 + 频率主义Z检验 - 完整修正版本                        ║
# ║  既包含贝叶斯的后验Mode、HDI                                     ║
# ║  也包含频率主义的Z值和p值                                        ║
# ╚═══════════════════════════════════════════════════════════════════╝

library(dplyr)

# ========== 实验数据 ==========
experiments_data <- data.frame(
  exp_name = c("实验1", "重复1", "重复2", "重复3", "重复4", "重复5", "重复6", "重复7"),
  n = c(28, 25, 12, 12, 11, 10, 10, 5),
  x = c(17, 19, 8, 9, 8, 7, 8, 4),
  stringsAsFactors = FALSE
)

# ========== 计算所有统计量 ==========
p0 <- 0.5
a_prior <- 1
b_prior <- 1

results <- experiments_data %>%
  mutate(
    # 频率主义统计量
    p_hat = x / n,
    p_pct = sprintf("%.2f%%", p_hat * 100),
    se = sqrt(p0 * (1 - p0) / n),
    z_value = (p_hat - p0) / se,
    p_value = 1 - pnorm(z_value),  # 右尾p值
    
    # 贝叶斯统计量
    a_posterior = a_prior + x,
    b_posterior = b_prior + (n - x),
    posterior_mode = (a_posterior - 1) / (a_posterior + b_posterior - 2),
    hdi_lower = qbeta(0.025, a_posterior, b_posterior),
    hdi_upper = qbeta(0.975, a_posterior, b_posterior)
  )

# ========== 打印主表格（频率+贝叶斯） ==========
cat("\n╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║  表1：频率主义Z检验 + 贝叶斯分析 - 完整对比表                   ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

table1 <- data.frame(
  "表号" = results$exp_name,
  "N" = results$n,
  "x" = results$x,
  "x/N" = results$p_pct,
  "Z值" = round(results$z_value, 4),
  "p值" = round(results$p_value, 4),
  "后验Beta" = sprintf("(%d,%d)", results$a_posterior, results$b_posterior),
  "Mode" = round(results$posterior_mode, 4),
  "95%HDI" = sprintf("[%.3f,%.3f]", results$hdi_lower, results$hdi_upper),
  check.names = FALSE
)

print(table1)

# ========== 保存为CSV（频率+贝叶斯） ==========
write.csv(table1, "表1_频率和贝叶斯对比.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✓ 已保存为CSV：表1_频率和贝叶斯对比.csv\n\n")

# ========== 创建纯贝叶斯表格（带Z和p值） ==========
table_bayesian <- data.frame(
  "表号" = results$exp_name,
  "总数(N)" = results$n,
  "快数(x)" = results$x,
  "x/N" = results$p_pct,
  "Z值" = round(results$z_value, 4),
  "p值" = round(results$p_value, 4),
  "后验Beta" = sprintf("(%d,%d)", results$a_posterior, results$b_posterior),
  "Mode" = round(results$posterior_mode, 4),
  "95%HDI" = sprintf("[%.3f,%.3f]", results$hdi_lower, results$hdi_upper),
  check.names = FALSE
)

cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║  表2：贝叶斯分析 + 频率主义Z检验（格式2）                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

print(table_bayesian)

write.csv(table_bayesian, "表2_贝叶斯带Z和p值.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✓ 已保存为CSV：表2_贝叶斯带Z和p值.csv\n\n")

# ========== 详细统计表 ==========
table_detailed <- data.frame(
  "表号" = results$exp_name,
  "N" = results$n,
  "x" = results$x,
  "样本比例" = results$p_pct,
  "标准误(SE)" = round(results$se, 6),
  "Z值" = round(results$z_value, 4),
  "p值(右尾)" = round(results$p_value, 4),
  "p值(双尾)" = round(2 * results$p_value, 4),
  "后验a" = results$a_posterior,
  "后验b" = results$b_posterior,
  "Mode" = round(results$posterior_mode, 4),
  "HDI宽度" = round(results$hdi_upper - results$hdi_lower, 4),
  check.names = FALSE
)

cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║  表3：完整详细数据（包含所有计算中间量）                        ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

print(table_detailed)

write.csv(table_detailed, "表3_完整详细数据.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✓ 已保存为CSV：表3_完整详细数据.csv\n\n")

# ========== 统计总结 ==========
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                    统计总结                                      ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

cat("Z值统计：\n")
cat(sprintf("  最大：%.4f (重复1)\n", max(results$z_value)))
cat(sprintf("  最小：%.4f (实验1)\n", min(results$z_value)))
cat(sprintf("  平均：%.4f\n", mean(results$z_value)))

cat("\np值统计（右尾）：\n")
cat(sprintf("  最小：%.4f (重复1 - 最显著)\n", min(results$p_value)))
cat(sprintf("  最大：%.4f (实验1 - 最不显著)\n", max(results$p_value)))
cat(sprintf("  平均：%.4f\n", mean(results$p_value)))
cat(sprintf("  p < 0.05的实验：%d次\n", sum(results$p_value < 0.05)))

cat("\nMode统计：\n")
cat(sprintf("  最高：%.4f (重复6/重复7)\n", max(results$posterior_mode)))
cat(sprintf("  最低：%.4f (实验1)\n", min(results$posterior_mode)))
cat(sprintf("  平均：%.4f\n", mean(results$posterior_mode)))

cat("\nHDI宽度统计：\n")
results$hdi_width <- results$hdi_upper - results$hdi_lower
cat(sprintf("  最宽：%.4f (重复7)\n", max(results$hdi_width)))
cat(sprintf("  最窄：%.4f (重复1)\n", min(results$hdi_width)))
cat(sprintf("  平均：%.4f\n", mean(results$hdi_width)))

cat("\n")

# ========== 对比分析 ==========
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║              Z值与Mode的关系分析                                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

comparison <- data.frame(
  "表号" = results$exp_name,
  "Z值" = round(results$z_value, 4),
  "p值" = round(results$p_value, 4),
  "显著?" = ifelse(results$p_value < 0.05, "Yes", "No"),
  "Mode" = round(results$posterior_mode, 4),
  "Mode%" = sprintf("%.2f%%", results$posterior_mode * 100),
  "N" = results$n,
  check.names = FALSE
)

print(comparison)

write.csv(comparison, "表4_Z值与Mode的对比.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✓ 已保存为CSV：表4_Z值与Mode的对比.csv\n\n")

# ========== 关键发现 ==========
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                  关键发现与总结                                  ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

cat("1️⃣  频率主义（Z检验）结果：\n")
cat(sprintf("   • Z值范围：%.4f ~ %.4f\n", min(results$z_value), max(results$z_value)))
cat(sprintf("   • p值范围：%.4f ~ %.4f\n", min(results$p_value), max(results$p_value)))
cat(sprintf("   • 显著实验数：%d/8 (%.1f%%)\n", sum(results$p_value < 0.05), 
            sum(results$p_value < 0.05)/8*100))
cat(sprintf("   • 平均Z值：%.4f\n\n", mean(results$z_value)))

cat("2️⃣  贝叶斯分析结果：\n")
cat(sprintf("   • Mode范围：%.4f ~ %.4f (%.2f%% ~ %.2f%%)\n", 
            min(results$posterior_mode), max(results$posterior_mode),
            min(results$posterior_mode)*100, max(results$posterior_mode)*100))
cat(sprintf("   • Mode平均：%.4f (%.2f%%)\n", mean(results$posterior_mode), 
            mean(results$posterior_mode)*100))
cat(sprintf("   • HDI平均宽度：%.4f\n", mean(results$hdi_width)))
cat(sprintf("   • 规律：N越大，HDI越窄（确定性越高）\n\n"))

cat("3️⃣  两种方法的一致性：\n")
cat(sprintf("   • Z值大的实验，Mode也往往更高\n"))
cat(sprintf("   • p值小的实验（显著），Mode也在较高水平\n"))
cat(sprintf("   • 样本量N影响Z值和HDI宽度，但不是Mode的唯一决定因素\n\n"))

cat("════════════════════════════════════════════════════════════════════\n\n")

cat("✓ 所有表格已生成并保存！\n\n")
