# ╔═══════════════════════════════════════════════════════════════════╗
# ║  C程序 vs Python程序性能比较 - 交互式分析脚本                      ║
# ║  可修改的参数快速分析版本                                           ║
# ╚═══════════════════════════════════════════════════════════════════╝

# ════════════════════════════════════════════════════════════════════
# 【用户可修改参数】
# ════════════════════════════════════════════════════════════════════

# 实验参数
n <- 28              # 总样本量
z <- 17              # C程序较快的次数
p0 <- 0.5            # 零假设的概率

# 贝叶斯先验参数（Beta分布）
a_prior <- 1         # Beta分布形状参数α (1=无信息先验)
b_prior <- 1         # Beta分布形状参数β (1=无信息先验)

# ROPE区间（实际意义的等价区间）
rope_lower <- 0.45
rope_upper <- 0.55

# 输出选项
save_plots <- TRUE   # 是否保存图表
plot_width <- 8.5    # 图表宽度
plot_height <- 10.5  # 图表高度
plot_dpi <- 300      # 图表分辨率

# ════════════════════════════════════════════════════════════════════
# 【分析代码 - 通常不需要修改】
# ════════════════════════════════════════════════════════════════════

library(ggplot2)
library(gridExtra)

# 计算后验参数
a_posterior <- a_prior + z
b_posterior <- b_prior + (n - z)

# 打印标题
cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║              C程序 vs Python程序性能对比分析                       ║\n")
cat(sprintf("║                    (样本量：%d 对程序)\n", n))
cat("╚═══════════════════════════════════════════════════════════════════╝\n")

# ─────────────────────────────────────────────────────────────────────
# 1. 频率主义分析
# ─────────────────────────────────────────────────────────────────────

cat("\n【频率主义分析】\n")
cat("─────────────────────────────────────────────\n")

p_hat <- z / n
se <- sqrt(p0 * (1 - p0) / n)
z_value <- (p_hat - p0) / se
p_value <- 2 * pnorm(-abs(z_value))

cat(sprintf("样本比例：%.4f (%d/%d = %.2f%%)\n", p_hat, z, n, p_hat * 100))
cat(sprintf("标准误：%.6f\n", se))
cat(sprintf("Z统计量：%.4f\n", z_value))
cat(sprintf("p值（双尾）：%.4f\n", p_value))
cat(sprintf("结论：%s\n", ifelse(p_value < 0.05, "✓ 拒绝H₀ - 显著差异", "✗ 无法拒绝H₀ - 无显著差异")))

# 二项检验
binom_result <- binom.test(z, n, p0, alternative = "two.sided")
cat(sprintf("二项检验p值：%.4f\n", binom_result$p.value))

# ─────────────────────────────────────────────────────────────────────
# 2. 贝叶斯分析
# ─────────────────────────────────────────────────────────────────────

cat("\n【贝叶斯分析】\n")
cat("─────────────────────────────────────────────\n")

# 计算关键统计量
prior_mode <- (a_prior - 1) / (a_prior + b_prior - 2)
prior_mean <- a_prior / (a_prior + b_prior)
prior_quantiles <- qbeta(c(0.025, 0.975), a_prior, b_prior)

posterior_mode <- (a_posterior - 1) / (a_posterior + b_posterior - 2)
posterior_mean <- a_posterior / (a_posterior + b_posterior)
posterior_quantiles <- qbeta(c(0.025, 0.975), a_posterior, b_posterior)

p_D <- beta(z + a_prior, (n - z) + b_prior) / beta(a_prior, b_prior)

cat(sprintf("\n先验分布：Beta(%.1f, %.1f)\n", a_prior, b_prior))
cat(sprintf("  Mode：%.4f  均值：%.4f\n", prior_mode, prior_mean))
cat(sprintf("  95%% CI：[%.4f, %.4f]\n", prior_quantiles[1], prior_quantiles[2]))

cat(sprintf("\n后验分布：Beta(%.1f, %.1f)\n", a_posterior, b_posterior))
cat(sprintf("  Mode：%.4f  均值：%.4f\n", posterior_mode, posterior_mean))
cat(sprintf("  95%% HDI：[%.4f, %.4f]\n", posterior_quantiles[1], posterior_quantiles[2]))
cat(sprintf("  边际似然 p(D)：%.4e\n", p_D))

cat(sprintf("\n贝叶斯推论：\n"))
cat(sprintf("  先验Mode %.4f → 后验Mode %.4f (变化 %.4f)\n", 
            prior_mode, posterior_mode, posterior_mode - prior_mode))
cat(sprintf("  不确定性下降：%.4f → %.4f\n",
            prior_quantiles[2] - prior_quantiles[1],
            posterior_quantiles[2] - posterior_quantiles[1]))

# ─────────────────────────────────────────────────────────────────────
# 3. 可视化
# ─────────────────────────────────────────────────────────────────────

cat("\n【生成图表】\n")
cat("─────────────────────────────────────────────\n")

# 生成θ序列
theta <- seq(0, 1, length.out = 1000)

# 计算密度
prior <- dbeta(theta, a_prior, b_prior)
likelihood <- theta^z * (1 - theta)^(n - z)
likelihood <- likelihood / max(likelihood)
posterior <- dbeta(theta, a_posterior, b_posterior)

df_prior <- data.frame(theta = theta, density = prior)
df_likelihood <- data.frame(theta = theta, density = likelihood)
df_posterior <- data.frame(theta = theta, density = posterior)

# 图1：先验
p1 <- ggplot(df_prior, aes(x = theta, y = density)) +
  geom_area(fill = "#5BA3D0", alpha = 0.7, color = NA) +
  geom_line(color = "#2E5090", linewidth = 0.9) +
  annotate("text", x = 0.5, y = max(prior) * 0.85,
           label = "ROPE", color = "#B2412C", fontface = "bold", size = 4.5) +
  geom_segment(aes(x = rope_lower, xend = rope_upper, y = max(prior) * 0.70, yend = max(prior) * 0.70),
               color = "#B2412C", linewidth = 2, arrow = arrow(ends = "both", length = unit(0.08, "cm"))) +
  xlim(0, 1) + ylim(0, max(prior) * 1.05) +
  labs(title = "Prior (beta)", x = "θ", y = sprintf("dbeta(θ|%.1f, %.1f)", a_prior, b_prior)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        axis.title = element_text(size = 9.5),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75))

# 图2：似然
p2 <- ggplot(df_likelihood, aes(x = theta, y = density)) +
  geom_area(fill = "#5BA3D0", alpha = 0.7, color = NA) +
  geom_line(color = "#2E5090", linewidth = 0.9) +
  annotate("text", x = 0.12, y = max(likelihood) * 0.88,
           label = sprintf("Data: z=%d,N=%d", z, n),
           color = "black", fontface = "bold", size = 3.8, hjust = 0) +
  annotate("text", x = 0.12, y = max(likelihood) * 0.73,
           label = sprintf("max at %.3f", z/n),
           color = "black", fontface = "bold", size = 3.8, hjust = 0) +
  xlim(0, 1) + ylim(0, max(likelihood) * 1.05) +
  labs(title = "Likelihood (Bernoulli)", x = "θ", y = "p(D|θ) / max") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        axis.title = element_text(size = 9.5),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75))

# 图3：后验
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
  labs(title = "Posterior (beta)", x = "θ", y = sprintf("dbeta(θ|%.1f, %.1f)", a_posterior, b_posterior)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        axis.title = element_text(size = 9.5),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75))

# 组合并显示
combined <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1, heights = c(1, 1, 1))
print(combined)

# 保存
if (save_plots) {
  ggsave("analysis_result.png", combined, width = plot_width, height = plot_height, dpi = plot_dpi)
  cat(sprintf("✓ 图表已保存：analysis_result.png\n"))
}

# ─────────────────────────────────────────────────────────────────────
# 4. 总结
# ─────────────────────────────────────────────────────────────────────

cat("\n【结论总结】\n")
cat("─────────────────────────────────────────────\n")
cat(sprintf("频率主义：Z=%.4f, p=%.4f → %s\n", 
            z_value, p_value, ifelse(p_value < 0.05, "显著", "不显著")))
cat(sprintf("贝叶斯：后验Mode=%.3f, HDI=[%.3f, %.3f] → 包含0.5,%s\n",
            posterior_mode, posterior_quantiles[1], posterior_quantiles[2],
            ifelse(posterior_quantiles[1] < 0.5 & posterior_quantiles[2] > 0.5, "不确定", "确定")))
cat("\n════════════════════════════════════════════════════════════════════\n\n")
