# ╔═══════════════════════════════════════════════════════════════════╗
# ║  Erdogmus 7次重复实验 - 贝叶斯分析（双先验对比）                ║
# ║                                                                   ║
# ║ 先验1：Beta(1, 1) - 无信息先验（均匀分布）                       ║
# ║ 先验2：Beta(3, 1) - 信息先验（倾向于高概率）                     ║
# ║                                                                   ║
# ║ 输出：表格图片格式，与参考表3一致                               ║
# ╚═══════════════════════════════════════════════════════════════════╝

library(ggplot2)
library(gridExtra)
library(dplyr)

# ========== 实验数据 ==========
experiments <- data.frame(
  exp_name = c("実験1", "重複1", "重複2", "重複3", "重複4", "重複5", "重複6", "重複7"),
  n = c(28, 25, 12, 12, 11, 10, 10, 5),
  x = c(17, 19, 8, 9, 8, 7, 8, 4),
  stringsAsFactors = FALSE
)

cat("\n╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║    贝叶斯分析：双先验对比 - Erdogmus 7次重复实验              ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

# ========== 先验设定 ==========
# 先验1：Beta(1, 1) - 无信息先验
a_prior_1 <- 1
b_prior_1 <- 1

# 先验2：Beta(3, 1) - 信息先验（倾向高概率）
a_prior_2 <- 3
b_prior_2 <- 1

cat("先验设定：\n")
cat(sprintf("先验1：Beta(%d, %d) - 无信息先验（均匀分布）\n", a_prior_1, b_prior_1))
cat(sprintf("先验2：Beta(%d, %d) - 信息先验（倾向高概率）\n", a_prior_2, b_prior_2))
cat("\n")

# ========== 计算后验统计量 ==========
results <- experiments %>%
  mutate(
    # 先验1：Beta(1, 1)
    a_post_1 = a_prior_1 + x,
    b_post_1 = b_prior_1 + (n - x),
    hdi_lower_1 = qbeta(0.025, a_post_1, b_post_1),
    hdi_upper_1 = qbeta(0.975, a_post_1, b_post_1),
    
    # 先验2：Beta(3, 1)
    a_post_2 = a_prior_2 + x,
    b_post_2 = b_prior_2 + (n - x),
    hdi_lower_2 = qbeta(0.025, a_post_2, b_post_2),
    hdi_upper_2 = qbeta(0.975, a_post_2, b_post_2)
  )

# ========== 创建表格数据 ==========
table_data <- data.frame(
  "表号" = results$exp_name,
  "CMU实验" = "",
  "总数(N)" = results$n,
  "快数(x)" = results$x,
  "x/N" = sprintf("%.2f%%", results$x / results$n * 100),
  "Prior0.5_HDI下界" = round(results$hdi_lower_1, 3),
  "Prior0.5_HDI上界" = round(results$hdi_upper_1, 3),
  "Prior0.75_HDI下界" = round(results$hdi_lower_2, 3),
  "Prior0.75_HDI上界" = round(results$hdi_upper_2, 3),
  check.names = FALSE
)

# 合并为显示格式（包含范围）
display_table <- data.frame(
  "表号" = results$exp_name,
  "CMU実験" = "",
  "総数(N)" = results$n,
  "快数(x)" = results$x,
  "x/N" = sprintf("%.2f%%", results$x / results$n * 100),
  "Prior0.5_HDI(95%)" = sprintf("%.3f - %.3f", 
                                round(results$hdi_lower_1, 3), 
                                round(results$hdi_upper_1, 3)),
  "Prior0.75_HDI(95%)" = sprintf("%.3f - %.3f",
                                 round(results$hdi_lower_2, 3),
                                 round(results$hdi_upper_2, 3)),
  check.names = FALSE
)

cat("表3：CMU実験 - 贝叶斯分析（双先验）\n\n")
print(display_table)

# 保存为CSV
write.csv(display_table, "表3_贝叶斯分析_双先验.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✓ 已保存CSV：表3_贝叶斯分析_双先验.csv\n\n")

# ========== 创建表格图片 ==========
cat("生成表格图片...\n\n")

# 创建简洁的表格为图片
# 准备表格数据框
table_for_image <- data.frame(
  "表号" = results$exp_name,
  "CMU実験" = "",
  "総数(N)" = results$n,
  "快数(x)" = results$x,
  "x/N" = sprintf("%.2f%%", results$x / results$n * 100),
  "Prior_0.5\nHDI(95%)" = sprintf("%.3f-%.3f", 
                                  round(results$hdi_lower_1, 3),
                                  round(results$hdi_upper_1, 3)),
  "Prior_0.75\nHDI(95%)" = sprintf("%.3f-%.3f",
                                   round(results$hdi_lower_2, 3),
                                   round(results$hdi_upper_2, 3))
)

# 使用gridExtra创建表格图片
table_grob <- gridExtra::tableGrob(table_for_image, 
                                   rows = NULL,
                                   theme = ttheme_default(
                                     base_size = 11,
                                     padding = unit(c(2, 2), "mm")
                                   ))

# 添加标题
title <- "表3：CMU実験 - 贝叶斯分析（双先验对比）"
final_table <- gridExtra::arrangeGrob(table_grob, 
                                      top = title,
                                      padding = unit(0.5, "cm"))

# 保存为PNG
png("表3_贝叶斯分析_双先验.png", width = 1000, height = 400, res = 120)
grid::grid.draw(final_table)
dev.off()

cat("✓ 表格图片已保存：表3_贝叶斯分析_双先验.png\n\n")

# ========== 详细统计 ==========
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                 先验比较统计                                    ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

cat("先验1（Beta 1,1 - 无信息）的HDI宽度：\n")
results$width_1 <- results$hdi_upper_1 - results$hdi_lower_1
cat(sprintf("  平均：%.4f\n", mean(results$width_1)))
cat(sprintf("  范围：%.4f - %.4f\n\n", min(results$width_1), max(results$width_1)))

cat("先验2（Beta 3,1 - 倾向高）的HDI宽度：\n")
results$width_2 <- results$hdi_upper_2 - results$hdi_lower_2
cat(sprintf("  平均：%.4f\n", mean(results$width_2)))
cat(sprintf("  范围：%.4f - %.4f\n\n", min(results$width_2), max(results$width_2)))

cat("先验比较：\n")
cat("  先验1（无信息）：HDI较宽，体现数据的不确定性\n")
cat("  先验2（信息先验）：HDI较窄，受先验的强影响\n")
cat("  差异幅度：平均HDI宽度差异 = ", 
    sprintf("%.4f\n\n", mean(abs(results$width_1 - results$width_2))))

# ========== 详细表格（包含后验参数） ==========
cat("详细表格（包含后验参数）\n\n")

detail_table <- data.frame(
  "表号" = results$exp_name,
  "N" = results$n,
  "x" = results$x,
  "x/N" = sprintf("%.2f%%", results$x / results$n * 100),
  "Prior1_Beta" = sprintf("(%d,%d)", results$a_post_1, results$b_post_1),
  "Prior1_HDI" = sprintf("[%.3f,%.3f]", results$hdi_lower_1, results$hdi_upper_1),
  "Prior2_Beta" = sprintf("(%d,%d)", results$a_post_2, results$b_post_2),
  "Prior2_HDI" = sprintf("[%.3f,%.3f]", results$hdi_lower_2, results$hdi_upper_2),
  check.names = FALSE
)

print(detail_table)

write.csv(detail_table, "表_贝叶斯详细参数_双先验.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✓ 详细表已保存：表_贝叶斯详细参数_双先验.csv\n\n")

cat("════════════════════════════════════════════════════════════════════\n")
cat("✨ 贝叶斯分析（双先验）完成！\n\n")
cat("生成文件：\n")
cat("  1. 表3_贝叶斯分析_双先验.csv\n")
cat("  2. 表3_贝叶斯分析_双先验.png ⭐ 表格图片\n")
cat("  3. 表_贝叶斯详细参数_双先验.csv\n\n")
