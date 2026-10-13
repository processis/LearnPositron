# ╔═══════════════════════════════════════════════════════════════════╗
# ║         C语言 vs Python性能比较：正确版本（参考图相符）           ║
# ║                 Beta(1,1)无信息先验版本                           ║
# ╚═══════════════════════════════════════════════════════════════════╝

library(ggplot2)
library(gridExtra)

# ========== 正确的参数设定 ==========
# 先验：Beta(1, 1) - 无信息先验（完全平坦）
a_prior <- 1
b_prior <- 1

# 实验数据
n <- 28
z <- 17

# 后验参数
a_posterior <- a_prior + z
b_posterior <- b_prior + (n - z)

# ROPE区间
rope_lower <- 0.45
rope_upper <- 0.55

# ========== 生成θ序列 ==========
theta <- seq(0, 1, length.out = 1000)

# ========== 1. 先验分布 Beta(1, 1) ==========
prior <- dbeta(theta, a_prior, b_prior)
df_prior <- data.frame(theta = theta, density = prior)

# ========== 2. 似然函数 ==========
likelihood <- theta^z * (1 - theta)^(n - z)
likelihood <- likelihood / max(likelihood)
likelihood_max <- z / n

df_likelihood <- data.frame(theta = theta, density = likelihood)

# ========== 3. 后验分布 Beta(18, 12) ==========
posterior <- dbeta(theta, a_posterior, b_posterior)
posterior_mode <- (a_posterior - 1) / (a_posterior + b_posterior - 2)

# 计算后验的HDI (95%)
posterior_quantiles <- qbeta(c(0.025, 0.975), a_posterior, b_posterior)
posterior_hdi_lower <- posterior_quantiles[1]
posterior_hdi_upper <- posterior_quantiles[2]

# 边际似然 - 用Beta函数形式（与参考图一致）
# p(D) = Beta(z+a, n-z+b) / Beta(a, b)
p_D <- beta(z + a_prior, (n - z) + b_prior) / beta(a_prior, b_prior)

df_posterior <- data.frame(theta = theta, density = posterior)

# ========== 打印统计摘要 ==========
cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║           贝叶斯分析：C vs Python 性能 (正确版本)                 ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n")
cat(sprintf("先验分布：Beta(%d, %d) - 无信息先验\n\n", a_prior, b_prior))
cat(sprintf("实验数据：N=%d, z=%d (C程序快%d/%d = %.2f%%)\n\n", n, z, z, n, (z/n)*100))
cat(sprintf("后验分布：Beta(%d, %d)\n", a_posterior, b_posterior))
cat(sprintf("  Mode：%.4f\n", posterior_mode))
cat(sprintf("  95%% HDI：[%.3f, %.3f]\n", posterior_hdi_lower, posterior_hdi_upper))
cat(sprintf("  边际似然 p(D)：%.2e\n\n", p_D))

# ========== 绘制三个图表 ==========

# 【图1】先验分布
p1 <- ggplot(df_prior, aes(x = theta, y = density)) +
  geom_area(fill = "#5BA3D0", alpha = 0.7, color = NA) +
  geom_line(color = "#2E5090", linewidth = 0.9) +
  # ROPE标注
  annotate("text", x = 0.5, y = max(prior) * 0.85,
           label = "ROPE", color = "#B2412C", fontface = "bold", size = 4.5) +
  geom_segment(aes(x = rope_lower, xend = rope_upper, y = max(prior) * 0.70, yend = max(prior) * 0.70),
               color = "#B2412C", linewidth = 2, arrow = arrow(ends = "both", length = unit(0.08, "cm"))) +
  xlim(0, 1) + ylim(0, max(prior) * 1.05) +
  labs(title = "Prior (beta)",
       x = "θ",
       y = sprintf("dbeta(θ|%d,%d)", a_prior, b_prior)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75)
  )

# 【图2】似然函数
p2 <- ggplot(df_likelihood, aes(x = theta, y = density)) +
  geom_area(fill = "#5BA3D0", alpha = 0.7, color = NA) +
  geom_line(color = "#2E5090", linewidth = 0.9) +
  annotate("text", x = 0.12, y = max(likelihood) * 0.88,
           label = sprintf("Data: z=%d,N=%d", z, n),
           color = "black", fontface = "bold", size = 4, hjust = 0) +
  annotate("text", x = 0.12, y = max(likelihood) * 0.73,
           label = sprintf("max at %.3f", likelihood_max),
           color = "black", fontface = "bold", size = 4, hjust = 0) +
  xlim(0, 1) + ylim(0, max(likelihood) * 1.05) +
  labs(title = "Likelihood (Bernoulli)",
       x = "θ",
       y = "p(D|θ) / max") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75)
  )

# 【图3】后验分布
p3 <- ggplot(df_posterior, aes(x = theta, y = density)) +
  geom_area(fill = "#5BA3D0", alpha = 0.7, color = NA) +
  geom_line(color = "#2E5090", linewidth = 0.9) +
  geom_area(data = df_posterior[df_posterior$theta >= posterior_hdi_lower & 
                                df_posterior$theta <= posterior_hdi_upper, ],
            fill = "#5BA3D0", alpha = 0.95, color = NA) +
  geom_vline(xintercept = posterior_mode, color = "#B2412C", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 0.12, y = max(posterior) * 0.90,
           label = sprintf("mode=%.3f", posterior_mode),
           color = "black", fontface = "bold", size = 4, hjust = 0) +
  annotate("text", x = 0.12, y = max(posterior) * 0.73,
           label = sprintf("p(D)=%.2e", p_D),
           color = "black", fontface = "bold", size = 4, hjust = 0) +
  geom_segment(aes(x = posterior_hdi_lower, xend = posterior_hdi_upper, 
                   y = max(posterior) * 0.48, yend = max(posterior) * 0.48),
               color = "#B2412C", linewidth = 2) +
  annotate("text", x = (posterior_hdi_lower + posterior_hdi_upper)/2, y = max(posterior) * 0.56,
           label = "95% HDI", color = "#B2412C", fontface = "bold", size = 3.5) +
  annotate("text", x = posterior_hdi_lower - 0.01, y = max(posterior) * 0.35,
           label = sprintf("%.3f", posterior_hdi_lower),
           color = "#B2412C", fontface = "bold", size = 3.5, hjust = 1) +
  annotate("text", x = posterior_hdi_upper + 0.01, y = max(posterior) * 0.35,
           label = sprintf("%.3f", posterior_hdi_upper),
           color = "#B2412C", fontface = "bold", size = 3.5, hjust = 0) +
  xlim(0, 1) + ylim(0, max(posterior) * 1.05) +
  labs(title = "Posterior (beta)",
       x = "θ",
       y = sprintf("dbeta(θ|%d,%d)", a_posterior, b_posterior)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.75)
  )

# 组合图表
combined <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1, heights = c(1, 1, 1))
print(combined)

# 保存
ggsave("bayesian_analysis_correct.png", combined, width = 8.5, height = 10.5, dpi = 300)

cat("\n✅ 正确版本图表已保存：bayesian_analysis_correct.png\n")
cat("   (现在与参考图的数值相符)\n")
