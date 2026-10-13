# ╔═══════════════════════════════════════════════════════════════════╗
# ║  Erdogmus CMU 重复实验结果表 - 格式化输出                          ║
# ╚═══════════════════════════════════════════════════════════════════╝

library(knitr)

# ========== 原始数据 ==========
data <- data.frame(
  "试验" = c("实验1", "重复1", "重复2", "重复3", "重复4", "重复5", "重复6", "重复7"),
  "总数(N)" = c(28, 25, 12, 12, 11, 10, 10, 5),
  "快数(x)" = c(17, 19, 8, 9, 8, 7, 8, 4),
  check.names = FALSE
)

# ========== 计算统计量 ==========
p0 <- 0.5

data$`x/N` <- sprintf("%.2f%%", (data$`快数(x)` / data$`总数(N)`) * 100)

data$Z值 <- sapply(1:nrow(data), function(i) {
  n <- data$`总数(N)`[i]
  x <- data$`快数(x)`[i]
  p_hat <- x / n
  se <- sqrt(p0 * (1 - p0) / n)
  z <- (p_hat - p0) / se
  round(z, 4)
})

data$p值 <- sapply(1:nrow(data), function(i) {
  z <- data$Z值[i]
  p <- 1 - pnorm(z)
  round(p, 4)
})

data$`拒绝H₀` <- ifelse(data$p值 < 0.05, "Yes", "No")

# ========== 重新排列列顺序 ==========
table_output <- data[, c("试验", "总数(N)", "快数(x)", "x/N", "Z值", "p值", "拒绝H₀")]

# ========== 打印为Markdown表格 ==========
cat("\n")
cat("# 表1：CMU实验 - 古典统计分析（重复实验结果）\n\n")

cat("| 试验 | 总数(N) | 快数(x) | x/N | Z值 | p值 | 拒绝H₀ |\n")
cat("|------|--------|--------|-----|-----|-----|--------|\n")

for(i in 1:nrow(table_output)) {
  row <- table_output[i, ]
  cat(sprintf("| %s | %d | %d | %s | %.4f | %.4f | %s |\n",
              row$试验, row$`总数(N)`, row$`快数(x)`, 
              row$`x/N`, row$Z值, row$p值, row$`拒绝H₀`))
}

cat("\n**注**：p值为右尾概率，α = 0.05\n\n")

# ========== 保存为CSV ==========
write.csv(table_output, "Erdogmus_CMU_experiments_table.csv", row.names = FALSE)
cat("✓ 表格已保存为CSV格式\n")

# ========== 统计汇总 ==========
cat("\n")
cat("## 实验统计汇总\n\n")

cat(sprintf("- **总实验数**：%d次\n", nrow(table_output)))
cat(sprintf("- **显著结果**（p < 0.05）：%d次 (%.1f%%)\n", 
            sum(table_output$`拒绝H₀` == "Yes"),
            sum(table_output$`拒绝H₀` == "Yes") / nrow(table_output) * 100))
cat(sprintf("- **不显著结果**（p ≥ 0.05）：%d次 (%.1f%%)\n",
            sum(table_output$`拒绝H₀` == "No"),
            sum(table_output$`拒绝H₀` == "No") / nrow(table_output) * 100))
cat(sprintf("- **平均样本量**：%.1f\n", mean(table_output$`总数(N)`)))
cat(sprintf("- **C快的平均比例**：%.2f%%\n", mean(as.numeric(gsub("%", "", table_output$`x/N`)))))

cat("\n")
cat("## 解释\n\n")
cat("- p值 < 0.05：在显著性水平α=0.05下，**拒绝零假设**，认为C程序显著快于Python\n")
cat("- p值 ≥ 0.05：在显著性水平α=0.05下，**无法拒绝零假设**，无足够证据说明差异显著\n")
cat("- Z值：标准化统计量，绝对值越大说明差异越明显\n")

cat("\n════════════════════════════════════════════════════════════════════\n\n")
