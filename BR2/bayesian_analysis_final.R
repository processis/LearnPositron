# 贝叶斯分析：C程序 vs Python程序性能比较
# 完整参考版本，与科学论文风格对标
# 先验：Beta(8.5, 3.5)
# 似然：Bernoulli(z=17, N=28)
# 后验：Beta(25.5, 14.5)

library(ggplot2)
library(gridExtra)

# ========== 参数设定 ==========
a_prior <- 8.5
b_prior <- 3.5
n <- 28
z <- 17

a_posterior <- a_prior + z
b_posterior <- b_prior + (n - z)

# 生成θ序列
theta <- seq(0, 1, length.out = 1000)

# ========== 1. 先验分布 ==========
prior <- dbeta(theta, a_prior, b_prior)
prior_mode <- (a_prior - 1) / (a_prior + b_prior - 2)

df_prior <- data.frame(theta = theta, density = prior)

# ROPE区间
rope_lower <- 0.45
rope_upper <- 0.55

# ========== 2. 似然函数 ==========
# 使用原始未归一化的似然（也可选择归一化）
likelihood_raw <- theta^z * (1 - theta)^(n - z)
# 这里用未归一化的版本使其更直观
likelihood <- likelihood_raw / max(likelihood_raw)
likelihood_max <- z / n

df_likelihood <- data.frame(theta = theta, density = likelihood)

# ========== 3. 后验分布 ==========
posterior <- dbeta(theta, a_posterior, b_posterior)
posterior_mode <- (a_posterior - 1) / (a_posterior + b_posterior - 2)

# 计算后验的HDI (95%)
posterior_quantiles <- qbeta(c(0.025, 0.975), a_posterior, b_posterior)
posterior_hdi_lower <- posterior_quantiles[1]
posterior_hdi_upper <- posterior_quantiles[2]

# 边际似然 p(D)
p_D <- integrate(function(x) dbeta(x, a_prior, b_prior) * dbinom(z, n, x), 0, 1)$value

df_posterior <- data.frame(theta = theta, density = posterior)

# ========== 打印摘要 ==========
cat("\n")
cat("╔════════════════════════════════════════════╗\n")
cat("║       贝叶斯分析：C vs Python 性能          ║\n")
cat("╚════════════════════════════════════════════╝\n")
cat(sprintf("先验分布：Beta(%.1f, %.1f)\n", a_prior, b_prior))
cat(sprintf("  Mode = %.3f (先验认为C程序有 %.1f%% 概率更快)\n", prior_mode, prior_mode*100))
cat("\n实验数据：\n")
cat(sprintf("  样本量 N = %d\n", n))
cat(sprintf("  C程序较快次数 z = %d\n", z))
cat(sprintf("  样本比例 = %.4f (%.2f%%)\n", likelihood_max, likelihood_max*100))
cat("\n后验分布：Beta(25.5, 14.5)\n")
cat(sprintf("  Mode = %.3f (后验认为C程序有 %.1f%% 概率更快)\n", posterior_mode, posterior_mode*100))
cat(sprintf("  95%% HDI: [%.3f, %.3f]\n", posterior_hdi_lower, posterior_hdi_upper))
cat(sprintf("  边际似然 p(D) = %.2e\n", p_D))
cat("\n");

# ========== 绘制三个图表 ==========

# 【图1】先验分布
p1 <- ggplot(df_prior, aes(x = theta, y = density)) +
  geom_area(fill = "#5BA3D0", alpha = 0.7, color = NA) +
  geom_line(color = "#2E5090", linewidth = 0.9) +
  # ROPE标注（双箭头）
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
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold", color = "#1a1a1a"),
    axis.title = element_text(size = 9.5, color = "#333333"),
    axis.text = element_text(size = 9, color = "#666666"),
    panel.border = element_rect(fill = NA, color = "#000000", linewidth = 0.75),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(8, 10, 8, 8)
  )

# 【图2】似然函数
p2 <- ggplot(df_likelihood, aes(x = theta, y = density)) +
  geom_area(fill = "#5BA3D0", alpha = 0.7, color = NA) +
  geom_line(color = "#2E5090", linewidth = 0.9) +
  # 数据标注
  annotate("text", x = 0.12, y = max(likelihood) * 0.88,
           label = sprintf("Data: z=%d,N=%d", z, n),
           color = "#000000", fontface = "bold", size = 3.8, hjust = 0) +
  annotate("text", x = 0.12, y = max(likelihood) * 0.73,
           label = sprintf("max at %.3f", likelihood_max),
           color = "#000000", fontface = "bold", size = 3.8, hjust = 0) +
  
  xlim(0, 1) + ylim(0, max(likelihood) * 1.05) +
  labs(title = "Likelihood (Bernoulli)",
       x = "θ",
       y = "p(D|θ) / max") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold", color = "#1a1a1a"),
    axis.title = element_text(size = 9.5, color = "#333333"),
    axis.text = element_text(size = 9, color = "#666666"),
    panel.border = element_rect(fill = NA, color = "#000000", linewidth = 0.75),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(8, 10, 8, 8)
  )

# 【图3】后验分布
p3 <- ggplot(df_posterior, aes(x = theta, y = density)) +
  geom_area(fill = "#5BA3D0", alpha = 0.7, color = NA) +
  geom_line(color = "#2E5090", linewidth = 0.9) +
  # HDI区间高亮
  geom_area(data = df_posterior[df_posterior$theta >= posterior_hdi_lower & df_posterior$theta <= posterior_hdi_upper, ],
            fill = "#5BA3D0", alpha = 0.95, color = NA) +
  # Mode虚线
  geom_vline(xintercept = posterior_mode, color = "#B2412C", linetype = "dashed", linewidth = 1) +
  # Mode标注
  annotate("text", x = 0.12, y = max(posterior) * 0.90,
           label = sprintf("mode=%.3f", posterior_mode),
           color = "#000000", fontface = "bold", size = 3.8, hjust = 0) +
  # p(D)标注
  annotate("text", x = 0.12, y = max(posterior) * 0.73,
           label = sprintf("p(D)=%.2e", p_D),
           color = "#000000", fontface = "bold", size = 3.8, hjust = 0) +
  # HDI线和标注
  geom_segment(aes(x = posterior_hdi_lower, xend = posterior_hdi_upper, 
                   y = max(posterior) * 0.48, yend = max(posterior) * 0.48),
               color = "#B2412C", linewidth = 2) +
  annotate("text", x = (posterior_hdi_lower + posterior_hdi_upper)/2, y = max(posterior) * 0.56,
           label = "95% HDI", color = "#B2412C", fontface = "bold", size = 3.5, hjust = 0.5) +
  # HDI端点数值
  annotate("text", x = posterior_hdi_lower - 0.01, y = max(posterior) * 0.35,
           label = sprintf("%.3f", posterior_hdi_lower),
           color = "#B2412C", fontface = "bold", size = 3.5, hjust = 1) +
  annotate("text", x = posterior_hdi_upper + 0.01, y = max(posterior) * 0.35,
           label = sprintf("%.3f", posterior_hdi_upper),
           color = "#B2412C", fontface = "bold", size = 3.5, hjust = 0) +
  
  xlim(0, 1) + ylim(0, max(posterior) * 1.05) +
  labs(title = "Posterior (beta)",
       x = "θ",
       y = sprintf("dbeta(θ|%.1f, %.1f)", a_posterior, b_posterior)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold", color = "#1a1a1a"),
    axis.title = element_text(size = 9.5, color = "#333333"),
    axis.text = element_text(size = 9, color = "#666666"),
    panel.border = element_rect(fill = NA, color = "#000000", linewidth = 0.75),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(8, 10, 8, 8)
  )

# 组合三个图表
combined_plot <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1, heights = c(1, 1, 1))

# 保存为高分辨率PNG
ggsave("bayesian_analysis_final.png", combined_plot, width = 8.5, height = 10.5, dpi = 300)

print(combined_plot)

cat("✓ 最终版本图表已保存为 bayesian_analysis_final.png\n\n")
