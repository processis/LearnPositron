# ╔═══════════════════════════════════════════════════════════════════╗
# ║         C语言 vs Python性能比较：完整实验统计分析                  ║
# ║                  Bayesian & Frequentist Analysis                    ║
# ╚═══════════════════════════════════════════════════════════════════╝

# 实验设计：
# - 样本量：N = 28 对应的Python-C程序对
# - 结果：C程序较快的次数 z = 17（60.71%）
# - 问题：C程序是否真的比Python快？

library(ggplot2)
library(gridExtra)

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║              C程序 vs Python程序性能对比分析                       ║\n")
cat("║                    (28个程序配对实验)                              ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n")

# ========== 参数设定 ==========
n <- 28
z <- 17
p0 <- 0.5

# 先验参数（贝叶斯分析）
a_prior <- 8.5
b_prior <- 3.5
a_posterior <- a_prior + z
b_posterior <- b_prior + (n - z)

# ROPE区间
rope_lower <- 0.45
rope_upper <- 0.55

# ═══════════════════════════════════════════════════════════════════════
# 第一部分：频率主义Z检验
# ═══════════════════════════════════════════════════════════════════════

cat("\n【第一部分：频率主义推断（Z检验）】\n")
cat("─────────────────────────────────────────────\n")

p_hat <- z / n
se <- sqrt(p0 * (1 - p0) / n)
z_value <- (p_hat - p0) / se
p_value_freq <- 2 * pnorm(-abs(z_value))

# 二项检验
binom_test_result <- binom.test(z, n, p0, alternative = "two.sided")

cat(sprintf("样本比例（p̂）：%.4f (%.2f%%)\n", p_hat, p_hat * 100))
cat(sprintf("标准误（SE）：%.6f\n", se))
cat(sprintf("Z统计量：%.4f\n", z_value))
cat(sprintf("p值（双尾）：%.4f\n", p_value_freq))
cat(sprintf("\n决策（α = 0.05）：\n"))
if (p_value_freq < 0.05) {
  cat("  ✓ 拒绝零假设：C程序显著快于Python\n")
} else {
  cat("  ✗ 无法拒绝零假设：C程序与Python无显著差异\n")
}

# ═══════════════════════════════════════════════════════════════════════
# 第二部分：贝叶斯分析
# ═══════════════════════════════════════════════════════════════════════

cat("\n\n【第二部分：贝叶斯推断（Beta-Bernoulli模型）】\n")
cat("─────────────────────────────────────────────\n")

# 计算模式和均值
prior_mode <- (a_prior - 1) / (a_prior + b_prior - 2)
prior_mean <- a_prior / (a_prior + b_prior)
prior_quantiles <- qbeta(c(0.025, 0.975), a_prior, b_prior)

posterior_mode <- (a_posterior - 1) / (a_posterior + b_posterior - 2)
posterior_mean <- a_posterior / (a_posterior + b_posterior)
posterior_quantiles <- qbeta(c(0.025, 0.975), a_posterior, b_posterior)

# 边际似然
p_D <- integrate(function(x) dbeta(x, a_prior, b_prior) * dbinom(z, n, x), 0, 1)$value

# 似然比较
likelihood_max <- z / n

cat(sprintf("\n先验分布：Beta(%.1f, %.1f)\n", a_prior, b_prior))
cat(sprintf("  Mode：%.4f\n", prior_mode))
cat(sprintf("  均值：%.4f\n", prior_mean))
cat(sprintf("  95%% CI：[%.4f, %.4f]\n", prior_quantiles[1], prior_quantiles[2]))

cat(sprintf("\n后验分布：Beta(%.1f, %.1f)\n", a_posterior, b_posterior))
cat(sprintf("  Mode：%.4f\n", posterior_mode))
cat(sprintf("  均值：%.4f\n", posterior_mean))
cat(sprintf("  95%% HDI：[%.4f, %.4f]\n", posterior_quantiles[1], posterior_quantiles[2]))
cat(sprintf("\n边际似然 p(D)：%.4e\n", p_D))
cat(sprintf("最大似然估计 MLE：%.4f\n", likelihood_max))

cat(sprintf("\n贝叶斯推论：\n"))
cat(sprintf("  先验 → 后验的变化：%.4f → %.4f\n", prior_mode, posterior_mode))
cat(sprintf("  可信度区间变窄：宽度从 %.4f → %.4f\n", 
    prior_quantiles[2] - prior_quantiles[1],
    posterior_quantiles[2] - posterior_quantiles[1]))

# ═══════════════════════════════════════════════════════════════════════
# 第三部分：可视化
# ═══════════════════════════════════════════════════════════════════════

cat("\n\n【第三部分：生成可视化图表】\n")
cat("─────────────────────────────────────────────\n")

# 生成θ序列
theta <- seq(0, 1, length.out = 1000)

# 计算密度
prior <- dbeta(theta, a_prior, b_prior)
likelihood_raw <- theta^z * (1 - theta)^(n - z)
likelihood <- likelihood_raw / max(likelihood_raw)
posterior <- dbeta(theta, a_posterior, b_posterior)

df_prior <- data.frame(theta = theta, density = prior)
df_likelihood <- data.frame(theta = theta, density = likelihood)
df_posterior <- data.frame(theta = theta, density = posterior)

# 图1：先验分布
p1 <- ggplot(df_prior, aes(x = theta, y = density)) +
  geom_area(fill = "#5BA3D0", alpha = 0.7, color = NA) +
  geom_line(color = "#2E5090", linewidth = 0.9) +
  annotate("text", x = 0.5, y = max(prior) * 0.85,
           label = "ROPE", color = "#B2412C", fontface = "bold", size = 4.5, hjust = 0.5) +
  geom_segment(aes(x = rope_lower, xend = rope_upper, y = max(prior) * 0.70, yend = max(prior) * 0.70),
               color = "#B2412C", linewidth = 2, arrow = arrow(ends = "both", length = unit(0.08, "cm"))) +
  xlim(0, 1) + ylim(0, max(prior) * 1.05) +
  labs(title = "Prior (beta)",
       x = "θ",
       y = sprintf("dbeta(θ|%.1f, %.1f)", a_prior, b_prior)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    axis.title = element_text(size = 9.5),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75)
  )

# 图2：似然函数
p2 <- ggplot(df_likelihood, aes(x = theta, y = density)) +
  geom_area(fill = "#5BA3D0", alpha = 0.7, color = NA) +
  geom_line(color = "#2E5090", linewidth = 0.9) +
  annotate("text", x = 0.12, y = max(likelihood) * 0.88,
           label = sprintf("Data: z=%d,N=%d", z, n),
           color = "black", fontface = "bold", size = 3.8, hjust = 0) +
  annotate("text", x = 0.12, y = max(likelihood) * 0.73,
           label = sprintf("max at %.3f", likelihood_max),
           color = "black", fontface = "bold", size = 3.8, hjust = 0) +
  xlim(0, 1) + ylim(0, max(likelihood) * 1.05) +
  labs(title = "Likelihood (Bernoulli)",
       x = "θ",
       y = "p(D|θ) / max") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    axis.title = element_text(size = 9.5),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75)
  )

# 图3：后验分布
p3 <- ggplot(df_posterior, aes(x = theta, y = density)) +
  geom_area(fill = "#5BA3D0", alpha = 0.7, color = NA) +
  geom_line(color = "#2E5090", linewidth = 0.9) +
  geom_area(data = df_posterior[df_posterior$theta >= posterior_quantiles[1] & 
                                df_posterior$theta <= posterior_quantiles[2], ],
            fill = "#5BA3D0", alpha = 0.95, color = NA) +
  geom_vline(xintercept = posterior_mode, color = "#B2412C", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 0.12, y = max(posterior) * 0.90,
           label = sprintf("mode=%.3f", posterior_mode),
           color = "black", fontface = "bold", size = 3.8, hjust = 0) +
  annotate("text", x = 0.12, y = max(posterior) * 0.73,
           label = sprintf("p(D)=%.2e", p_D),
           color = "black", fontface = "bold", size = 3.8, hjust = 0) +
  geom_segment(aes(x = posterior_quantiles[1], xend = posterior_quantiles[2], 
                   y = max(posterior) * 0.48, yend = max(posterior) * 0.48),
               color = "#B2412C", linewidth = 2) +
  annotate("text", x = (posterior_quantiles[1] + posterior_quantiles[2])/2, y = max(posterior) * 0.56,
           label = "95% HDI", color = "#B2412C", fontface = "bold", size = 3.5, hjust = 0.5) +
  annotate("text", x = posterior_quantiles[1] - 0.01, y = max(posterior) * 0.35,
           label = sprintf("%.3f", posterior_quantiles[1]),
           color = "#B2412C", fontface = "bold", size = 3.5, hjust = 1) +
  annotate("text", x = posterior_quantiles[2] + 0.01, y = max(posterior) * 0.35,
           label = sprintf("%.3f", posterior_quantiles[2]),
           color = "#B2412C", fontface = "bold", size = 3.5, hjust = 0) +
  xlim(0, 1) + ylim(0, max(posterior) * 1.05) +
  labs(title = "Posterior (beta)",
       x = "θ",
       y = sprintf("dbeta(θ|%.1f, %.1f)", a_posterior, b_posterior)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    axis.title = element_text(size = 9.5),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75)
  )

# 组合三个图表
combined_plot <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1, heights = c(1, 1, 1))
ggsave("complete_experiment_analysis.png", combined_plot, width = 8.5, height = 10.5, dpi = 300)

print(combined_plot)

cat("\n✓ 贝叶斯分析图表已保存：complete_experiment_analysis.png\n")

# ═══════════════════════════════════════════════════════════════════════
# 总结
# ═══════════════════════════════════════════════════════════════════════

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                         结论总结                                   ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n")

cat(sprintf("1. 频率主义检验结论：\n"))
cat(sprintf("   - Z = %.4f, p = %.4f (> 0.05)\n", z_value, p_value_freq))
cat(sprintf("   - 在α=0.05显著性水平下，无法拒绝零假设\n"))
cat(sprintf("   - C程序与Python程序性能无统计显著差异\n\n"))

cat(sprintf("2. 贝叶斯分析结论：\n"))
cat(sprintf("   - 先验认为：C程序有 %.1f%% 概率更快\n", prior_mode * 100))
cat(sprintf("   - 数据显示：C程序有 %.2f%% 概率更快（样本）\n", likelihood_max * 100))
cat(sprintf("   - 后验认为：C程序有 %.1f%% 概率更快\n", posterior_mode * 100))
cat(sprintf("   - 可信度提高：从 [%.1f%%, %.1f%%] 到 [%.1f%%, %.1f%%]\n",
            prior_quantiles[1] * 100, prior_quantiles[2] * 100,
            posterior_quantiles[1] * 100, posterior_quantiles[2] * 100))

cat(sprintf("\n3. 实际意义：\n"))
cat(sprintf("   - 虽然样本中C程序快60.71%%，但这个差异可能来自随机波动\n"))
cat(sprintf("   - 在实验设计上需要：\n"))
cat(sprintf("     * 更大的样本量（n > 28）\n"))
cat(sprintf("     * 更严格的测试控制\n"))
cat(sprintf("     * 多次重复实验验证\n"))

cat("\n════════════════════════════════════════════════════════════════════\n\n")
