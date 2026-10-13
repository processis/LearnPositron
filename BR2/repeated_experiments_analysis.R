# ╔═══════════════════════════════════════════════════════════════════╗
# ║    Erdogmus 重复实验：CMU实验与古典统计Z检验分析                  ║
# ║                                                                   ║
# ║ 实验背景：在不同场景下重复进行Python vs C性能对比实验             ║
# ║ 统计方法：古典假设检验（Z检验）                                   ║
# ║ 零假设H₀：p = 0.5（C与Python性能无差异）                         ║
# ╚═══════════════════════════════════════════════════════════════════╝

library(dplyr)
library(xtable)

# ========== 实验数据 ==========
# 根据表1中的数据

experiments <- tibble(
  exp_name = c("实验1", "重复1", "重复2", "重复3", "重复4", "重复5", "重复6", "重复7"),
  n = c(28, 25, 12, 12, 11, 10, 10, 5),
  x = c(17, 19, 8, 9, 8, 7, 8, 4)
)

# ========== 计算统计量 ==========
p0 <- 0.5  # 零假设中的概率

results <- experiments %>%
  mutate(
    # 样本比例
    p_hat = x / n,
    p_pct = p_hat * 100,
    
    # 标准误
    se = sqrt(p0 * (1 - p0) / n),
    
    # Z统计量
    z_value = (p_hat - p0) / se,
    
    # p值（单尾，右侧）
    # p_value = P(Z > z_value) = 1 - Φ(z_value)
    p_value = 1 - pnorm(z_value),
    
    # 是否拒绝H₀（α = 0.05）
    reject_h0 = if_else(p_value < 0.05, "Yes", "No")
  ) %>%
  select(exp_name, n, x, p_pct, z_value, p_value, reject_h0)

# 四舍五入到合理的小数位
results_display <- results %>%
  mutate(
    p_pct = sprintf("%.2f%%", p_pct),
    z_value = round(z_value, 4),
    p_value = round(p_value, 4),
    # 处理特殊情况（z值过大时标记为N/A）
    z_value_display = if_else(abs(z_value) > 999, "N/A", as.character(z_value)),
    p_value_display = if_else(p_value < 0.0001, sprintf("< 0.0001*", p_value), 
                              if_else(p_value > 0.1875, sprintf("%.4f*", p_value),
                                     sprintf("%.4f", p_value)))
  ) %>%
  select(exp_name, n, x, p_pct, z_value_display, p_value_display, reject_h0)

# ========== 打印结果 ==========
cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║              表1：CMU实验 - 古典统计分析结果                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

# 创建表格
table_df <- data.frame(
  "表号" = results_display$exp_name,
  "CMU实验" = "",
  "占比" = results_display$p_pct,
  "总数(N)" = results_display$n,
  "快数(x)" = results_display$x,
  "x/N" = results_display$p_pct,
  "Z值" = results_display$z_value_display,
  "p值" = results_display$p_value_display,
  "拒绝H₀" = results_display$reject_h0,
  check.names = FALSE
)

# 重新整理为表格格式
final_table <- data.frame(
  "表号" = c("实验1", "重复1", "重复2", "重复3", "重复4", "重复5", "重复6", "重复7"),
  "CMU实验" = "",
  "总数(N)" = results$n,
  "快数(x)" = results$x,
  "x/N" = sprintf("%.2f%%", results$p_pct),
  "Z值" = results_display$z_value_display,
  "p值" = results_display$p_value_display,
  "拒绝H₀" = results_display$reject_h0,
  check.names = FALSE
)

print(final_table)

# ========== 详细分析 ==========
cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                    详细统计分析                                   ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n")

for(i in 1:nrow(results)) {
  cat(sprintf("\n【%s】\n", results$exp_name[i]))
  cat(sprintf("  样本量：N = %d\n", results$n[i]))
  cat(sprintf("  C程序较快：x = %d\n", results$x[i]))
  cat(sprintf("  样本比例：x/N = %.4f (%.2f%%)\n", results$p_hat[i], results$p_pct[i]))
  cat(sprintf("  标准误：SE = %.4f\n", results$se[i]))
  cat(sprintf("  Z统计量：Z = %.4f\n", results$z_value[i]))
  cat(sprintf("  p值（右尾）：p = %.4f\n", results$p_value[i]))
  
  if(results$p_value[i] < 0.05) {
    cat(sprintf("  ✓ 结论：在α=0.05水平下，拒绝H₀ (p = %.4f < 0.05)\n", results$p_value[i]))
    cat(sprintf("         C程序显著快于Python\n"))
  } else {
    cat(sprintf("  ✗ 结论：在α=0.05水平下，无法拒绝H₀ (p = %.4f > 0.05)\n", results$p_value[i]))
    cat(sprintf("         C程序与Python无显著差异\n"))
  }
}

# ========== 汇总统计 ==========
cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                     汇总统计                                      ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

significant <- sum(results$p_value < 0.05)
total <- nrow(results)

cat(sprintf("总实验次数：%d\n", total))
cat(sprintf("显著的实验（p < 0.05）：%d 次 (%.1f%%)\n", significant, significant/total*100))
cat(sprintf("不显著的实验（p ≥ 0.05）：%d 次 (%.1f%%)\n", total-significant, (total-significant)/total*100))

cat("\n样本量统计：\n")
cat(sprintf("  最大：N = %d (实验%s)\n", max(results$n), results$exp_name[which.max(results$n)]))
cat(sprintf("  最小：N = %d (实验%s)\n", min(results$n), results$exp_name[which.min(results$n)]))
cat(sprintf("  平均：N = %.1f\n", mean(results$n)))

cat("\nC程序快的比例统计：\n")
cat(sprintf("  最高：%.2f%% (实验%s)\n", max(results$p_pct), results$exp_name[which.max(results$p_pct)]))
cat(sprintf("  最低：%.2f%% (实验%s)\n", min(results$p_pct), results$exp_name[which.min(results$p_pct)]))
cat(sprintf("  平均：%.2f%%\n", mean(results$p_pct)))

cat("\nZ值统计：\n")
cat(sprintf("  最大：Z = %.4f (实验%s)\n", max(results$z_value), results$exp_name[which.max(results$z_value)]))
cat(sprintf("  最小：Z = %.4f (实验%s)\n", min(results$z_value), results$exp_name[which.min(results$z_value)]))
cat(sprintf("  平均：Z = %.4f\n", mean(results$z_value)))

cat("\np值统计：\n")
cat(sprintf("  最小：p = %.4f (最显著)\n", min(results$p_value)))
cat(sprintf("  最大：p = %.4f (最不显著)\n", max(results$p_value)))
cat(sprintf("  平均：p = %.4f\n", mean(results$p_value)))

# ========== 保存为CSV ==========
write.csv(results, "repeated_experiments_results.csv", row.names = FALSE)
cat("\n✓ 详细结果已保存到：repeated_experiments_results.csv\n")

# ========== 保存为表格 ==========
final_output <- data.frame(
  "试验" = c("实验1", "重复1", "重复2", "重复3", "重复4", "重复5", "重复6", "重复7"),
  "总数(N)" = results$n,
  "快数(x)" = results$x,
  "x/N" = sprintf("%.2f%%", results$p_pct),
  "Z值" = results_display$z_value_display,
  "p值" = results_display$p_value_display,
  "拒绝H₀" = results_display$reject_h0,
  check.names = FALSE
)

write.csv(final_output, "Erdogmus_CMU_experiment_table.csv", row.names = FALSE)
cat("✓ 表格已保存到：Erdogmus_CMU_experiment_table.csv\n")

cat("\n════════════════════════════════════════════════════════════════════\n\n")
