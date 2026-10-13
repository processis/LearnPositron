# 儿童发育数据分析
# 数据来源: Ranalysis/data/儿童发育数据.csv

library(arrow)
library(ggplot2)

# 读入数据
df <- read.csv("Ranalysis/data/儿童发育数据.csv")

# 描述性统计
summary(df)

# Pearson 相关分析
cor(df$month, df$height, method = "pearson")

# 简单线性回归
fit <- lm(height ~ month, data = df)
summary(fit)

# 散点图及回归线
ggplot(df, aes(x = month, y = height)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    x = "月龄（月）",
    y = "身高（cm）",
    title = "儿童身高与月龄散点图及线性回归"
  ) +
  theme_minimal()

# ============================================================
# 生长曲线建模
# ============================================================

# 二次多项式模型: height = b0 + b1*month + b2*month^2
fit_quad <- lm(height ~ month + I(month^2), data = df)
summary(fit_quad)

# 幂函数模型: height = a * month^b
fit_power <- nls(height ~ a * month^b, data = df, start = list(a = 1, b = 1))
summary(fit_power)

# Michaelis-Menten 模型: height = (a * month) / (b + month)
fit_mm <- nls(height ~ SSmicmen(month, a, b), data = df)
summary(fit_mm)

# 模型比较 (AIC)
AIC(fit, fit_quad, fit_power, fit_mm)

# 伪 R-squared (非线性模型): 1 - SSE/SST
SST <- sum((df$height - mean(df$height))^2)
data.frame(
  model = c("线性回归", "二次多项式", "幂函数", "Michaelis-Menten"),
  R_squared = round(c(
    summary(fit)$r.squared,
    summary(fit_quad)$r.squared,
    1 - sum(resid(fit_power)^2) / SST,
    1 - sum(resid(fit_mm)^2) / SST
  ), 4),
  RMSE = round(c(
    sqrt(mean(resid(fit)^2)),
    sqrt(mean(resid(fit_quad)^2)),
    sqrt(mean(resid(fit_power)^2)),
    sqrt(mean(resid(fit_mm)^2))
  ), 3)
)

# 生长曲线模型比较图
month_range <- seq(min(df$month), max(df$month), length.out = 200)
pred_df <- data.frame(month = month_range)
pred_df$linear <- predict(fit, newdata = pred_df)
pred_df$quadratic <- predict(fit_quad, newdata = pred_df)
pred_df$power <- predict(fit_power, newdata = pred_df)
pred_df$micmen <- predict(fit_mm, newdata = pred_df)

pred_long <- reshape(pred_df, direction = "long",
  varying = c("linear", "quadratic", "power", "micmen"),
  v.names = "height", times = c("线性回归", "二次多项式", "幂函数", "Michaelis-Menten"),
  timevar = "model")

ggplot() +
  geom_point(data = df, aes(x = month, y = height), alpha = 0.5) +
  geom_line(data = pred_long, aes(x = month, y = height, color = model), linewidth = 0.8) +
  labs(
    x = "月龄（月）",
    y = "身高（cm）",
    color = "模型",
    title = "儿童身高生长曲线模型比较"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
