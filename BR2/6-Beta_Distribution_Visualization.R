# ╔═══════════════════════════════════════════════════════════════════╗
# ║   Beta分布可视化完整代码                                          ║
# ║   根据α和β参数展示各种分布形状：偏左、偏右、居中                  ║
# ║   基于CMU实验的8个样本数据                                        ║
# ╚═══════════════════════════════════════════════════════════════════╝

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

cat("\n╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║         Beta分布可视化分析 - 加载成功                          ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

# ============================================================
# 第一部分：准备8个实验的数据
# ============================================================

cat("【第一步】准备实验数据...\n")

raw_data <- data.frame(
  exp_name = c("实验1", "重复1", "重复2", "重复3", "重复4", "重复5", "重复6", "重复7"),
  n = c(28, 25, 12, 12, 11, 10, 10, 5),
  x = c(17, 19, 8, 9, 8, 7, 8, 4),
  stringsAsFactors = FALSE
) %>%
  mutate(
    # Prior 0.5 的后验参数
    post_05_a = 1 + x,
    post_05_b = 1 + (n - x),
    
    # Prior 0.75 的后验参数
    post_75_a = 6 + x,
    post_75_b = 2 + (n - x)
  )

cat("✓ 数据已准备\n\n")

# ============================================================
# 可视化1：8个实验的后验Beta分布对比图
# ============================================================

cat("【可视化1】绘制8个实验的后验Beta分布对比...\n\n")

# 生成Beta分布的x值范围
x_vals <- seq(0, 1, by = 0.001)

# 为每个实验生成两个先验的Beta分布
beta_data <- expand.grid(
  theta = x_vals,
  exp_name = raw_data$exp_name,
  Prior = c("Prior 0.5", "Prior 0.75")
) %>%
  left_join(
    raw_data %>% 
      select(exp_name, post_05_a, post_05_b, post_75_a, post_75_b),
    by = "exp_name"
  ) %>%
  mutate(
    density = case_when(
      Prior == "Prior 0.5" ~ dbeta(theta, post_05_a, post_05_b),
      Prior == "Prior 0.75" ~ dbeta(theta, post_75_a, post_75_b)
    ),
    alpha = case_when(
      Prior == "Prior 0.5" ~ post_05_a,
      Prior == "Prior 0.75" ~ post_75_a
    ),
    beta = case_when(
      Prior == "Prior 0.5" ~ post_05_b,
      Prior == "Prior 0.75" ~ post_75_b
    ),
    mode = (alpha - 1) / (alpha + beta - 2),
    mean = alpha / (alpha + beta)
  ) %>%
  filter(!is.na(density))

# 创建分面图
plot1 <- ggplot(beta_data, aes(x = theta, y = density, color = Prior, fill = Prior)) +
  geom_area(alpha = 0.3, position = "identity") +
  geom_line(linewidth = 1) +
  facet_wrap(~exp_name, ncol = 2, scales = "free_y") +
  scale_color_manual(
    values = c("Prior 0.5" = "#E7298A", "Prior 0.75" = "#1B9E77"),
    labels = c("Prior 0.5: Beta(1,1)", "Prior 0.75: Beta(6,2)")
  ) +
  scale_fill_manual(
    values = c("Prior 0.5" = "#E7298A", "Prior 0.75" = "#1B9E77"),
    labels = c("Prior 0.5: Beta(1,1)", "Prior 0.75: Beta(6,2)")
  ) +
  labs(
    title = "后验Beta分布对比：不同先验的影响",
    subtitle = "展示8个实验中两种先验得到的后验分布形状",
    x = "成功概率 (θ)",
    y = "概率密度",
    color = "先验分布",
    fill = "先验分布"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10)
  )

print(plot1)
cat("✓ 已显示\n\n")


# ============================================================
# 可视化2：Beta分布的多种形状
# ============================================================

cat("【可视化2】绘制Beta分布的多种形状...\n\n")

# 构造各种典型的Beta分布参数
beta_distributions <- data.frame(
  name = c(
    "均匀分布", 
    "偏左(低成功率)",
    "偏右(高成功率)",
    "居中、狭窄",
    "居中、宽阔",
    "U形(极端值)",
    "Prior 0.5\n(无信息)",
    "Prior 0.75\n(有信息)"
  ),
  alpha = c(1, 2, 8, 5, 3, 0.5, 1, 6),
  beta = c(1, 8, 2, 5, 3, 0.5, 1, 2)
)

# 生成x值和密度
x_vals <- seq(0.001, 0.999, by = 0.001)

beta_plot_data <- expand.grid(
  theta = x_vals,
  dist_id = 1:nrow(beta_distributions)
) %>%
  left_join(
    beta_distributions %>% mutate(dist_id = row_number()),
    by = "dist_id"
  ) %>%
  mutate(
    density = dbeta(theta, alpha, beta),
    mode = (alpha - 1) / (alpha + beta - 2),
    mean = alpha / (alpha + beta)
  )

# 绘制所有分布
plot2 <- ggplot(beta_plot_data, aes(x = theta, y = density, fill = name, color = name)) +
  geom_area(alpha = 0.4, position = "identity") +
  geom_line(linewidth = 0.8) +
  facet_wrap(~factor(name, levels = beta_distributions$name), 
             ncol = 3, scales = "free_y") +
  scale_color_brewer(palette = "Set2", direction = -1) +
  scale_fill_brewer(palette = "Set2", direction = -1) +
  labs(
    title = "Beta分布的多种形状：通过α和β参数控制",
    subtitle = "展示如何使用不同的参数组合来得到偏左、偏右、居中或其他特殊形状",
    x = "参数值 (θ)",
    y = "概率密度"
  ) +
  guides(fill = "none", color = "none") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    strip.text = element_text(face = "bold", size = 10, color = "white", 
                              margin = margin(5, 5, 5, 5)),
    strip.background = element_rect(fill = "gray30", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

print(plot2)
cat("✓ 已显示\n\n")


# ============================================================
# 可视化3：Beta分布参数矩阵（α和β的完整组合）
# ============================================================

cat("【可视化3】绘制Beta分布参数矩阵（6×6）...\n\n")

# 定义不同的α和β组合
alpha_vals <- c(0.5, 1, 2, 5, 10, 20)
beta_vals <- c(0.5, 1, 2, 5, 10, 20)

# 生成所有组合
x_vals <- seq(0.001, 0.999, by = 0.001)

param_grid <- expand.grid(
  theta = x_vals,
  alpha = alpha_vals,
  beta = beta_vals
) %>%
  mutate(
    density = dbeta(theta, alpha, beta),
    param_label = paste0("α=", alpha, ", β=", beta)
  )

# 创建热力图：alpha和beta的参数矩阵
plot3 <- ggplot(param_grid, aes(x = theta, y = density, fill = alpha, color = alpha)) +
  geom_area(alpha = 0.5, position = "identity") +
  geom_line(linewidth = 0.6) +
  facet_grid(beta ~ alpha, scales = "free_y", 
             labeller = labeller(
               alpha = c("0.5" = "α=0.5", "1" = "α=1", "2" = "α=2", 
                        "5" = "α=5", "10" = "α=10", "20" = "α=20"),
               beta = c("0.5" = "β=0.5", "1" = "β=1", "2" = "β=2",
                       "5" = "β=5", "10" = "β=10", "20" = "β=20")
             )) +
  scale_fill_gradient(low = "#FEE5D9", high = "#A1D99B", guide = "none") +
  scale_color_gradient(low = "#FCBBA1", high = "#238B45", guide = "none") +
  labs(
    title = "Beta分布形状矩阵：α和β参数的完整组合",
    subtitle = "纵轴为β，横轴为α。观察不同参数组合如何改变分布形状",
    x = "参数值 (θ)",
    y = "概率密度"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    strip.text.x = element_text(size = 8, face = "bold"),
    strip.text.y = element_text(size = 8, face = "bold"),
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 7)
  )

print(plot3)
cat("✓ 已显示\n\n")


# ============================================================
# 可视化4：你的数据中的参数分析
# ============================================================

cat("【可视化4】绘制参数分析图...\n\n")

# 收集你的数据中的所有参数
param_data <- data.frame(
  exp_name = rep(c("实验1", "重复1", "重复2", "重复3", "重复4", "重复5", "重复6", "重复7"), 2),
  Prior = c(rep("Prior 0.5\n无信息", 8), rep("Prior 0.75\n有信息", 8)),
  alpha = c(18, 20, 9, 10, 9, 8, 9, 5, 23, 25, 14, 15, 14, 13, 14, 10),
  beta = c(12, 7, 5, 4, 4, 4, 3, 2, 13, 8, 6, 5, 5, 5, 4, 3)
) %>%
  mutate(
    mean = alpha / (alpha + beta),
    ratio = alpha / beta,
    exp_num = rep(1:8, 2)
  )

# 图1：α和β的散点图
p1 <- ggplot(param_data, aes(x = alpha, y = beta, color = Prior, size = alpha + beta)) +
  geom_point(alpha = 0.7) +
  geom_label(aes(label = exp_name), size = 3, vjust = -1, show.legend = FALSE) +
  scale_color_manual(values = c("Prior 0.5\n无信息" = "#E7298A", "Prior 0.75\n有信息" = "#1B9E77")) +
  scale_size_continuous(range = c(3, 8), name = "α+β\n(信息强度)") +
  labs(
    title = "Alpha vs Beta 参数分布",
    subtitle = "散点大小代表信息强度（α+β），右上角=右倾，左下角=左倾",
    x = "α (成功相关)",
    y = "β (失败相关)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = "bottom"
  )

# 图2：平均值对比
p2 <- ggplot(param_data, aes(x = factor(exp_num), y = mean, fill = Prior, group = Prior)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50", alpha = 0.5) +
  scale_fill_manual(values = c("Prior 0.5\n无信息" = "#E7298A", "Prior 0.75\n有信息" = "#1B9E77")) +
  labs(
    title = "Beta分布的平均值",
    subtitle = "展示两个先验在各实验中的中心位置",
    x = "实验编号",
    y = "平均值 (α/(α+β))"
  ) +
  scale_x_discrete(labels = c("1"="实验1", "2"="重复1", "3"="重复2", "4"="重复3",
                              "5"="重复4", "6"="重复5", "7"="重复6", "8"="重复7")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 图3：信息强度
p3 <- ggplot(param_data, aes(x = factor(exp_num), y = alpha + beta, fill = Prior)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("Prior 0.5\n无信息" = "#E7298A", "Prior 0.75\n有信息" = "#1B9E77")) +
  labs(
    title = "信息强度：α+β",
    subtitle = "值越大，分布越集中（更多的先验+数据信息）",
    x = "实验编号",
    y = "α + β"
  ) +
  scale_x_discrete(labels = c("1"="实验1", "2"="重复1", "3"="重复2", "4"="重复3",
                              "5"="重复4", "6"="重复5", "7"="重复6", "8"="重复7")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 组合显示
gridExtra::grid.arrange(p1, p2, p3, ncol = 2)
cat("✓ 已显示\n\n")


# ============================================================
# 生成参考表
# ============================================================

cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║              Beta分布参数参考表                                  ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

beta_reference <- data.frame(
  分布类型 = c(
    "均匀分布",
    "偏左(低值)",
    "偏右(高值)",
    "居中尖峰",
    "居中平坦",
    "U形分布",
    "无信息先验(Prior 0.5)",
    "有信息先验(Prior 0.75)"
  ),
  α参数 = c(1, 2, 8, 5, 3, 0.5, 1, 6),
  β参数 = c(1, 8, 2, 5, 3, 0.5, 1, 2),
  平均值 = c(0.50, 0.20, 0.80, 0.50, 0.50, 0.50, 0.50, 0.75),
  众数说明 = c(
    "无众数(平坦)",
    "低值处",
    "高值处",
    "0.5处(尖锐)",
    "0.5处(平缓)",
    "两端(U形)",
    "无明确众数",
    "高值处(0.83)"
  ),
  应用场景 = c(
    "完全无知",
    "倾向低成功率",
    "倾向高成功率",
    "确信在0.5附近",
    "对0.5有弱信念",
    "相信极端值",
    "贝叶斯分析入门",
    "融入先验知识"
  )
)

print(beta_reference)

cat("\n")
cat("════════════════════════════════════════════════════════════════════\n\n")
cat("✨ Beta分布可视化分析完成！\n")
cat("📊 已生成4个主要可视化和1个参考表\n\n")

cat("【核心规律速记】\n")
cat("  • α > β  → 分布右倾（向高值倾斜）\n")
cat("  • α < β  → 分布左倾（向低值倾斜）\n")
cat("  • α = β  → 分布对称（中心在0.5）\n")
cat("  • α+β越大 → 分布越集中（信息量大）\n")
cat("  • α+β越小 → 分布越分散（信息量小）\n\n")
