# ╔═══════════════════════════════════════════════════════════════════╗
# ║     Erdogmus 7次重复实验 - 贝叶斯分析与后验分布汇总                ║
# ║                                                                   ║
# ║ 对每次实验进行Beta-Bernoulli贝叶斯分析                            ║
# ║ 最后生成所有后验分布的对比图                                      ║
# ╚═══════════════════════════════════════════════════════════════════╝

library(ggplot2)
library(gridExtra)
library(dplyr)

# ========== 实验数据 ==========
experiments_data <- tibble(
  exp_name = c("实验1", "重复1", "重复2", "重复3", "重复4", "重复5", "重复6", "重复7"),
  n = c(28, 25, 12, 12, 11, 10, 10, 5),
  x = c(17, 19, 8, 9, 8, 7, 8, 4)
)

# 贝叶斯先验参数（无信息先验）
a_prior <- 1
b_prior <- 1

# ROPE区间
rope_lower <- 0.45
rope_upper <- 0.55

cat("\n╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║        7次重复实验的贝叶斯分析 - Beta(1,1)无信息先验             ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

# ========== 对每次实验进行分析 ==========
all_posteriors <- list()
all_results <- list()

for(i in 1:nrow(experiments_data)) {
  exp_name <- experiments_data$exp_name[i]
  n <- experiments_data$n[i]
  z <- experiments_data$x[i]
  
  # 后验参数
  a_post <- a_prior + z
  b_post <- b_prior + (n - z)
  
  # 后验统计量
  post_mode <- (a_post - 1) / (a_post + b_post - 2)
  post_mean <- a_post / (a_post + b_post)
  post_quantiles <- qbeta(c(0.025, 0.975), a_post, b_post)
  
  # 边际似然
  p_D <- beta(a_post, b_post) / beta(a_prior, b_prior)
  
  # 生成后验分布数据
  theta <- seq(0, 1, length.out = 1000)
  posterior <- dbeta(theta, a_post, b_post)
  df_posterior <- data.frame(
    theta = theta,
    density = posterior,
    exp_name = exp_name
  )
  
  # 保存后验分布
  all_posteriors[[i]] <- df_posterior
  
  # 保存统计结果
  all_results[[i]] <- data.frame(
    exp_name = exp_name,
    n = n,
    z = z,
    p_hat = z/n,
    a_posterior = a_post,
    b_posterior = b_post,
    mode = post_mode,
    mean = post_mean,
    hdi_lower = post_quantiles[1],
    hdi_upper = post_quantiles[2],
    p_D = p_D,
    stringsAsFactors = FALSE
  )
  
  # 打印每个实验的结果
  cat(sprintf("【%s】\n", exp_name))
  cat(sprintf("  数据：N=%d, z=%d, p̂=%.4f\n", n, z, z/n))
  cat(sprintf("  后验：Beta(%d, %d)\n", a_post, b_post))
  cat(sprintf("  Mode: %.4f, Mean: %.4f\n", post_mode, post_mean))
  cat(sprintf("  95%% HDI: [%.4f, %.4f]\n", post_quantiles[1], post_quantiles[2]))
  cat(sprintf("  p(D): %.2e\n\n", p_D))
}

# 合并所有结果
posterior_data <- do.call(rbind, all_posteriors)
results_summary <- do.call(rbind, all_results)

# ========== 结果表格 ==========
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                    贝叶斯分析结果汇总表                           ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

results_display <- results_summary %>%
  mutate(
    p_hat_pct = sprintf("%.2f%%", p_hat * 100),
    mode = round(mode, 4),
    hdi = sprintf("[%.3f, %.3f]", hdi_lower, hdi_upper),
    p_D_str = sprintf("%.2e", p_D)
  ) %>%
  select(exp_name, n, z, p_hat_pct, a_posterior, b_posterior, mode, hdi, p_D_str)

colnames(results_display) <- c("实验", "N", "z", "比例", "a", "b", "Mode", "95% HDI", "p(D)")

print(results_display)

# 保存结果
write.csv(results_summary, "bayesian_repeated_experiments_results.csv", row.names = FALSE)
cat("\n✓ 详细结果已保存：bayesian_repeated_experiments_results.csv\n\n")

# ========== 绘制后验分布汇总图 ==========
cat("生成后验分布对比图...\n\n")

# 图1：所有后验分布叠加
p_overlay <- ggplot(posterior_data, aes(x = theta, y = density, color = exp_name, fill = exp_name)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_area(alpha = 0.1) +
  xlim(0, 1) +
  labs(title = "所有实验的后验分布对比（叠加）",
       x = "θ (C程序较快的概率)",
       y = "概率密度",
       color = "实验",
       fill = "实验",
       subtitle = "先验：Beta(1,1) | 数据：来自7次重复实验") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right"
  )

print(p_overlay)
ggsave("posterior_overlay_all_experiments.png", p_overlay, width = 12, height = 6, dpi = 300)

# 图2：分面图（每个实验单独显示）
p_facet <- ggplot(posterior_data, aes(x = theta, y = density, fill = exp_name)) +
  geom_area(alpha = 0.7) +
  geom_line(color = "black", linewidth = 0.5) +
  facet_wrap(~exp_name, ncol = 4) +
  xlim(0, 1) +
  labs(title = "每次实验的后验分布（分面）",
       x = "θ",
       y = "密度",
       fill = "实验",
       subtitle = "先验：Beta(1,1)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", size = 9),
    legend.position = "bottom"
  )

print(p_facet)
ggsave("posterior_facet_all_experiments.png", p_facet, width = 14, height = 8, dpi = 300)

# 图3：后验Mode对比
p_mode <- ggplot(results_summary, aes(x = reorder(exp_name, mode), y = mode, fill = mode)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", linewidth = 1, alpha = 0.5) +
  scale_fill_gradient(low = "#FFA07A", high = "#FF4500") +
  coord_flip() +
  labs(title = "各实验的后验Mode对比",
       x = "实验",
       y = "后验Mode（C程序较快的概率）",
       fill = "Mode值") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

print(p_mode)
ggsave("posterior_mode_comparison.png", p_mode, width = 10, height = 6, dpi = 300)

# 图4：HDI区间对比
hdi_data <- results_summary %>%
  mutate(exp_name = factor(exp_name, levels = unique(exp_name)))

p_hdi <- ggplot(hdi_data, aes(x = reorder(exp_name, mode), y = mode)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = hdi_lower, ymax = hdi_upper), 
                width = 0.2, linewidth = 1, color = "steelblue") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", linewidth = 1, alpha = 0.5) +
  coord_flip() +
  labs(title = "各实验的后验Mode及95% HDI",
       x = "实验",
       y = "θ (C程序较快的概率)",
       subtitle = "点：Mode | 线：95% HDI") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

print(p_hdi)
ggsave("posterior_hdi_comparison.png", p_hdi, width = 10, height = 6, dpi = 300)

# ========== 综合对比图 ==========
# 图5：四合一综合图
combined <- gridExtra::grid.arrange(p_overlay, p_mode, p_hdi, ncol = 1, heights = c(1.2, 1, 1))
print(combined)
ggsave("bayesian_repeated_experiments_summary.png", combined, width = 12, height = 12, dpi = 300)

cat("\n✓ 已生成所有可视化图表：\n")
cat("  • posterior_overlay_all_experiments.png - 后验分布叠加\n")
cat("  • posterior_facet_all_experiments.png - 后验分布分面\n")
cat("  • posterior_mode_comparison.png - Mode对比\n")
cat("  • posterior_hdi_comparison.png - HDI对比\n")
cat("  • bayesian_repeated_experiments_summary.png - 综合图表\n")

# ========== 统计总结 ==========
cat("\n╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║                      贝叶斯推理汇总                              ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

cat("后验Mode分析：\n")
cat(sprintf("  最高：%.4f (实验: %s)\n", max(results_summary$mode), 
            results_summary$exp_name[which.max(results_summary$mode)]))
cat(sprintf("  最低：%.4f (实验: %s)\n", min(results_summary$mode), 
            results_summary$exp_name[which.min(results_summary$mode)]))
cat(sprintf("  平均：%.4f\n\n", mean(results_summary$mode)))

cat("HDI宽度分析：\n")
results_summary$hdi_width <- results_summary$hdi_upper - results_summary$hdi_lower
cat(sprintf("  最宽：%.4f (实验: %s, N=%d)\n", max(results_summary$hdi_width),
            results_summary$exp_name[which.max(results_summary$hdi_width)],
            results_summary$n[which.max(results_summary$hdi_width)]))
cat(sprintf("  最窄：%.4f (实验: %s, N=%d)\n", min(results_summary$hdi_width),
            results_summary$exp_name[which.min(results_summary$hdi_width)],
            results_summary$n[which.min(results_summary$hdi_width)]))
cat(sprintf("  平均：%.4f\n\n", mean(results_summary$hdi_width)))

cat("边际似然p(D)分析：\n")
cat(sprintf("  最大：%.2e (实验: %s)\n", max(results_summary$p_D),
            results_summary$exp_name[which.max(results_summary$p_D)]))
cat(sprintf("  最小：%.2e (实验: %s)\n", min(results_summary$p_D),
            results_summary$exp_name[which.min(results_summary$p_D)]))

cat("\n════════════════════════════════════════════════════════════════════\n\n")
