# 贝叶斯分析：C程序相对Python程序的性能优势概率
# 使用Beta-Bernoulli共轭模型
# 先验：Beta(8.5, 3.5)
# 似然：Bernoulli(z=17, N=28)
# 后验：Beta(25.5, 14.5)

library(ggplot2)
library(gridExtra)

# ========== 参数设定 ==========
a_prior <- 8.5
b_prior <- 3.5
n <- 28
z <- 17  # C程序较快的次数

# 后验参数（Beta-Bernoulli共轭）
a_posterior <- a_prior + z
b_posterior <- b_prior + (n - z)

# 生成θ序列（概率）
theta <- seq(0, 1, length.out = 1000)

# ========== 1. 先验分布 ==========
prior <- dbeta(theta, a_prior, b_prior)
prior_mode <- (a_prior - 1) / (a_prior + b_prior - 2)
prior_mean <- a_prior / (a_prior + b_prior)

# 计算先验的HDI (95% Highest Density Interval)
prior_quantiles <- qbeta(c(0.025, 0.975), a_prior, b_prior)
prior_hdi_lower <- prior_quantiles[1]
prior_hdi_upper <- prior_quantiles[2]

# ROPE (Region of Practical Equivalence)
rope_lower <- 0.45
rope_upper <- 0.55

df_prior <- data.frame(theta = theta, density = prior)

# ========== 2. 似然函数 ==========
likelihood <- theta^z * (1 - theta)^(n - z)
likelihood <- likelihood / max(likelihood)  # 归一化以便可视化
likelihood_max <- z / n  # 最大似然估计

df_likelihood <- data.frame(theta = theta, density = likelihood)

# ========== 3. 后验分布 ==========
posterior <- dbeta(theta, a_posterior, b_posterior)
posterior_mode <- (a_posterior - 1) / (a_posterior + b_posterior - 2)
posterior_mean <- a_posterior / (a_posterior + b_posterior)

# 计算后验的HDI (95%)
posterior_quantiles <- qbeta(c(0.025, 0.975), a_posterior, b_posterior)
posterior_hdi_lower <- posterior_quantiles[1]
posterior_hdi_upper <- posterior_quantiles[2]

# 计算边际似然 p(D)
p_D <- integrate(function(x) dbeta(x, a_prior, b_prior) * dbinom(z, n, x), 0, 1)$value

df_posterior <- data.frame(theta = theta, density = posterior)

# ========== 打印统计摘要 ==========
cat("\n")
cat("========== 贝叶斯分析结果 ==========\n")
cat("\n【先验分布】Beta(8.5, 3.5)\n")
cat(sprintf("  Mode: %.4f\n", prior_mode))
cat(sprintf("  Mean: %.4f\n", prior_mean))
cat(sprintf("  95%% HDI: [%.4f, %.4f]\n", prior_hdi_lower, prior_hdi_upper))

cat("\n【实验数据】\n")
cat(sprintf("  样本量 N = %d\n", n))
cat(sprintf("  C程序较快次数 z = %d\n", z))
cat(sprintf("  样本比例 = %.4f\n", likelihood_max))
cat(sprintf("  MLE: %.4f\n", likelihood_max))

cat("\n【后验分布】Beta(25.5, 14.5)\n")
cat(sprintf("  Mode: %.4f\n", posterior_mode))
cat(sprintf("  Mean: %.4f\n", posterior_mean))
cat(sprintf("  95%% HDI: [%.4f, %.4f]\n", posterior_hdi_lower, posterior_hdi_upper))
cat(sprintf("  边际似然 p(D): %.4e\n", p_D))

cat("\n【贝叶斯推断】\n")
cat(sprintf("  后验较先验下移: %.4f → %.4f\n", prior_mode, posterior_mode))
cat(sprintf("  HDI变窄（更确定）\n"))
cat(sprintf("  ROPE区间: [%.2f, %.2f]\n", rope_lower, rope_upper))

cat("\n=====================================\n\n")

# ========== 绘制三个图表 ==========

# 图1：先验分布
p1 <- ggplot(df_prior, aes(x = theta, y = density)) +
  geom_area(fill = "lightblue", alpha = 0.7) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  # HDI区间高亮
  geom_area(data = df_prior[df_prior$theta >= prior_hdi_lower & df_prior$theta <= prior_hdi_upper, ],
            fill = "dodgerblue", alpha = 0.8) +
  # Mode线
  geom_vline(xintercept = prior_mode, color = "darkblue", linetype = "solid", linewidth = 1.2) +
  # Mode标注
  annotate("text", x = 0.75, y = max(prior) * 0.95,
           label = sprintf("mode=%.2f", prior_mode), 
           color = "black", fontface = "bold", size = 4.5, hjust = 0.5) +
  # HDI线和标注
  geom_segment(aes(x = prior_hdi_lower, xend = prior_hdi_upper, 
                   y = max(prior) * 0.55, yend = max(prior) * 0.55),
               color = "darkred", linewidth = 2.5) +
  annotate("text", x = (prior_hdi_lower + prior_hdi_upper)/2, y = max(prior) * 0.65,
           label = "95% HDI", color = "darkred", fontface = "bold", size = 4) +
  annotate("text", x = prior_hdi_lower - 0.02, y = max(prior) * 0.45,
           label = sprintf("%.3f", prior_hdi_lower), color = "darkred", fontface = "bold", size = 3.5) +
  annotate("text", x = prior_hdi_upper + 0.02, y = max(prior) * 0.45,
           label = sprintf("%.3f", prior_hdi_upper), color = "darkred", fontface = "bold", size = 3.5) +
  # ROPE标注
  annotate("text", x = 0.5, y = max(prior) * 0.35,
           label = "ROPE", color = "darkred", fontface = "bold", size = 4, angle = 0) +
  geom_segment(aes(x = 0.463, xend = 0.463, y = 0, yend = max(prior) * 0.32),
               color = "darkred", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = 0.463, xend = 0.537, y = max(prior) * 0.32, yend = max(prior) * 0.32),
               color = "darkred", linewidth = 2) +
  
  xlim(0, 1) + ylim(0, max(prior) * 1.05) +
  labs(title = "Prior (beta)",
       x = "θ",
       y = sprintf("dbeta(θ|%.1f,%.1f)", a_prior, b_prior)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# 图2：似然函数
p2 <- ggplot(df_likelihood, aes(x = theta, y = density)) +
  geom_area(fill = "lightblue", alpha = 0.7) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  # MLE点
  geom_vline(xintercept = likelihood_max, color = "darkred", linetype = "solid", linewidth = 1.2) +
  annotate("text", x = likelihood_max, y = max(likelihood) * 0.75,
           label = sprintf("Data: z=%d,N=%d", z, n),
           color = "black", fontface = "bold", size = 4.5, hjust = 0.5) +
  annotate("text", x = likelihood_max, y = max(likelihood) * 0.60,
           label = sprintf("max at %.3f", likelihood_max),
           color = "black", fontface = "bold", size = 4.5, hjust = 0.5) +
  
  xlim(0, 1) + ylim(0, max(likelihood) * 1.05) +
  labs(title = "Likelihood (Bernoulli)",
       x = "θ",
       y = "p(D|θ) / max") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# 图3：后验分布
p3 <- ggplot(df_posterior, aes(x = theta, y = density)) +
  geom_area(fill = "lightcoral", alpha = 0.7) +
  geom_line(color = "darkred", linewidth = 1.1) +
  # HDI区间高亮
  geom_area(data = df_posterior[df_posterior$theta >= posterior_hdi_lower & df_posterior$theta <= posterior_hdi_upper, ],
            fill = "salmon", alpha = 0.8) +
  # Mode线
  geom_vline(xintercept = posterior_mode, color = "darkred", linetype = "dashed", linewidth = 1.2) +
  # Mode标注
  annotate("text", x = posterior_mode - 0.05, y = max(posterior) * 0.95,
           label = sprintf("mode=%.3f", posterior_mode),
           color = "black", fontface = "bold", size = 4.5, hjust = 1) +
  # HDI线和标注
  geom_segment(aes(x = posterior_hdi_lower, xend = posterior_hdi_upper,
                   y = max(posterior) * 0.55, yend = max(posterior) * 0.55),
               color = "darkred", linewidth = 2.5) +
  annotate("text", x = (posterior_hdi_lower + posterior_hdi_upper)/2, y = max(posterior) * 0.65,
           label = "95% HDI", color = "darkred", fontface = "bold", size = 4) +
  annotate("text", x = posterior_hdi_lower - 0.02, y = max(posterior) * 0.45,
           label = sprintf("%.3f", posterior_hdi_lower), color = "darkred", fontface = "bold", size = 3.5) +
  annotate("text", x = posterior_hdi_upper + 0.02, y = max(posterior) * 0.45,
           label = sprintf("%.3f", posterior_hdi_upper), color = "darkred", fontface = "bold", size = 3.5) +
  # p(D)标注
  annotate("text", x = 0.15, y = max(posterior) * 0.85,
           label = sprintf("p(D)=%.2e", p_D),
           color = "black", fontface = "bold", size = 4.5) +
  
  xlim(0, 1) + ylim(0, max(posterior) * 1.05) +
  labs(title = "Posterior (beta)",
       x = "θ",
       y = sprintf("dbeta(θ|%.1f,%.1f)", a_posterior, b_posterior)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# ========== 组合三个图表 ==========
combined_plot <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1, heights = c(1, 1, 1))

# 保存组合图
ggsave("bayesian_analysis_complete.png", combined_plot, width = 10, height = 12, dpi = 300)

print(combined_plot)

cat("✓ 完整的贝叶斯分析图表已保存为 bayesian_analysis_complete.png\n")
