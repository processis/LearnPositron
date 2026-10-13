# 贝叶斯分析：C程序 vs Python程序性能比较
# Beta-Bernoulli共轭模型
# 先验：Beta(8, 1)
# 似然：Bernoulli(z=17, N=28)
# 后验：Beta(18, 12)

library(ggplot2)
library(gridExtra)

# ========== 参数设定 ==========
# 使用Beta(8, 1)作为先验（表达较强的相信C程序更快）
a_prior <- 8
b_prior <- 1
n <- 28
z <- 17  # C程序较快的次数

# 后验参数
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
likelihood <- theta^z * (1 - theta)^(n - z)
likelihood <- likelihood / max(likelihood)
likelihood_max <- z / n

df_likelihood <- data.frame(theta = theta, density = likelihood)

# ========== 3. 后验分布 ==========
posterior <- dbeta(theta, a_posterior, b_posterior)
posterior_mode <- (a_posterior - 1) / (a_posterior + b_posterior - 2)

# 计算后验的HDI
posterior_quantiles <- qbeta(c(0.025, 0.975), a_posterior, b_posterior)
posterior_hdi_lower <- posterior_quantiles[1]
posterior_hdi_upper <- posterior_quantiles[2]

# 边际似然
p_D <- integrate(function(x) dbeta(x, a_prior, b_prior) * dbinom(z, n, x), 0, 1)$value

df_posterior <- data.frame(theta = theta, density = posterior)

# 打印统计信息
cat("\n========== 贝叶斯分析 ==========\n")
cat(sprintf("先验：Beta(%.0f, %.0f)\n", a_prior, b_prior))
cat(sprintf("似然：Bernoulli(z=%d, N=%d)\n", z, n))
cat(sprintf("后验：Beta(%.0f, %.0f)\n", a_posterior, b_posterior))
cat(sprintf("\n后验统计：\n"))
cat(sprintf("  Mode: %.3f\n", posterior_mode))
cat(sprintf("  95%% HDI: [%.3f, %.3f]\n", posterior_hdi_lower, posterior_hdi_upper))
cat(sprintf("  p(D): %.2e\n", p_D))
cat("=====================================\n\n")

# ========== 绘制图表 ==========

# 【图1】先验分布
p1 <- ggplot(df_prior, aes(x = theta, y = density)) +
  geom_area(fill = "lightblue", alpha = 0.8) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  # ROPE标注
  annotate("text", x = 0.5, y = max(prior) * 0.9,
           label = "ROPE", color = "darkred", fontface = "bold", size = 4.5) +
  geom_segment(aes(x = rope_lower, xend = rope_lower, y = 0, yend = max(prior) * 0.8),
               color = "darkred", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = rope_upper, xend = rope_upper, y = 0, yend = max(prior) * 0.8),
               color = "darkred", linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = rope_lower, xend = rope_upper, y = max(prior) * 0.75, yend = max(prior) * 0.75),
               color = "darkred", linewidth = 2.5, arrow = arrow(ends = "both", length = unit(0.1, "cm"))) +
  
  xlim(0, 1) + ylim(0, max(prior) * 1.1) +
  labs(title = "Prior (beta)",
       x = "θ",
       y = sprintf("dbeta(θ|%.0f,%.0f)", a_prior, b_prior)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.8),
    plot.margin = margin(8, 8, 8, 8)
  )

# 【图2】似然函数
p2 <- ggplot(df_likelihood, aes(x = theta, y = density)) +
  geom_area(fill = "lightblue", alpha = 0.8) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  # 最大似然标注
  annotate("text", x = 0.15, y = max(likelihood) * 0.85,
           label = sprintf("Data: z=%d,N=%d", z, n),
           color = "black", fontface = "bold", size = 4) +
  annotate("text", x = 0.15, y = max(likelihood) * 0.70,
           label = sprintf("max at %.3f", likelihood_max),
           color = "black", fontface = "bold", size = 4) +
  
  xlim(0, 1) + ylim(0, max(likelihood) * 1.1) +
  labs(title = "Likelihood (Bernoulli)",
       x = "θ",
       y = "p(D|θ)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.8),
    plot.margin = margin(8, 8, 8, 8)
  )

# 【图3】后验分布
p3 <- ggplot(df_posterior, aes(x = theta, y = density)) +
  geom_area(fill = "lightblue", alpha = 0.8) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  # 后验HDI高亮
  geom_area(data = df_posterior[df_posterior$theta >= posterior_hdi_lower & df_posterior$theta <= posterior_hdi_upper, ],
            fill = "steelblue", alpha = 0.95) +
  # Mode线
  geom_vline(xintercept = posterior_mode, color = "darkred", linetype = "dashed", linewidth = 1.2) +
  # Mode标注
  annotate("text", x = 0.15, y = max(posterior) * 0.95,
           label = sprintf("mode=%.3f", posterior_mode),
           color = "black", fontface = "bold", size = 4) +
  # p(D)标注
  annotate("text", x = 0.15, y = max(posterior) * 0.80,
           label = sprintf("p(D)=%.2e", p_D),
           color = "black", fontface = "bold", size = 4) +
  # HDI标注
  geom_segment(aes(x = posterior_hdi_lower, xend = posterior_hdi_upper, 
                   y = max(posterior) * 0.55, yend = max(posterior) * 0.55),
               color = "darkred", linewidth = 2) +
  annotate("text", x = (posterior_hdi_lower + posterior_hdi_upper)/2, y = max(posterior) * 0.62,
           label = "95% HDI", color = "darkred", fontface = "bold", size = 3.5) +
  # HDI端点标注
  annotate("text", x = posterior_hdi_lower, y = max(posterior) * 0.40,
           label = sprintf("%.3f", posterior_hdi_lower),
           color = "darkred", fontface = "bold", size = 3.5, hjust = 0.5) +
  annotate("text", x = posterior_hdi_upper, y = max(posterior) * 0.40,
           label = sprintf("%.3f", posterior_hdi_upper),
           color = "darkred", fontface = "bold", size = 3.5, hjust = 0.5) +
  
  xlim(0, 1) + ylim(0, max(posterior) * 1.1) +
  labs(title = "Posterior (beta)",
       x = "θ",
       y = sprintf("dbeta(θ|%.0f,%.0f)", a_posterior, b_posterior)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.8),
    plot.margin = margin(8, 8, 8, 8)
  )

# 组合三个图表
combined_plot <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1, heights = c(1, 1, 1))

# 保存
ggsave("bayesian_analysis_v2.png", combined_plot, width = 8, height = 10, dpi = 300)

print(combined_plot)

cat("✓ 图表已保存为 bayesian_analysis_v2.png\n")
