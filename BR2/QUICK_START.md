# 快速开始指南

## 5秒钟了解结果

```
实验数据：28个程序，C程序快17个（60.71%）
问题：这个优势是否显著？

结果：NO ✗
- Z检验：Z=1.134, p=0.257 > 0.05 → 不显著
- 贝叶斯：后验HDI [0.485, 0.777] 包含0.5 → 不确定
```

---

## 一键运行（推荐）

```r
# 在R中执行：
source("complete_experiment_analysis.R")
```

**生成内容**：
- 完整的统计分析输出
- 三层贝叶斯图表：`complete_experiment_analysis.png`

---

## 关键数字速记

### 频率主义（传统假设检验）
```
Z = 1.1339      → 标准化效应量
p = 0.2568      → 无法拒绝H₀
α = 0.05        → 显著性水平

结论：无统计显著差异
```

### 贝叶斯（Beta-Bernoulli共轭）
```
先验Mode：0.75   → 认为C快75%
后验Mode：0.645  → 数据调整为64.5%
后验HDI：[0.485, 0.777]  → 95%可信区间
p(D)：6.42e-02   → 边际似然
```

---

## 图表快查

| 需要 | 使用 |
|-----|------|
| 正态分布Z检验 | `experiment_normal_distribution.png` |
| Beta先验后验对比 | `beta_distribution_comparison.png` |
| **专业三层报告** | `complete_experiment_analysis.png` ⭐ |
| 科学论文风格 | `bayesian_analysis_final.png` |

---

## 三种分析方法对比

### 方法1：二项检验（最精确）
```r
source("experiment_analysis.R")
# 适合：小样本，非参数
# 结果：p = 0.3449
```

### 方法2：Z检验（最常用）
```r
source("experiment_analysis.R")  # 同上
# 适合：样本量中等
# 结果：Z = 1.1339, p = 0.2568
```

### 方法3：贝叶斯分析（最信息丰富）⭐
```r
source("complete_experiment_analysis.R")
# 适合：融合先验知识，不只要p值
# 结果：后验Mode=0.6447, HDI=[0.485, 0.777]
```

---

## 论文写法建议

### 频率主义版本
> 28个程序配对实验中，C程序快17个（60.71%）。二项检验结果不显著（p=0.257>0.05），无法拒绝零假设。C程序与Python程序的性能无统计显著差异。

### 贝叶斯版本
> 采用Beta(8.5,3.5)先验，反映对C程序速度优势的适度信念。实验数据（z=17,N=28）更新后验为Beta(25.5,14.5)，模式0.645，95% HDI为[0.485, 0.777]。虽然样本显示C程序有60.71%概率更快，但后验分布的宽度和对0.5的覆盖表明该优势在统计上不够确定。

---

## 常见问题

**Q: 为什么p=0.2568这么大？**
A: 因为Z=1.134相对较小。虽然样本中60.71%快，但28个样本的随机波动范围很大。

**Q: 贝叶斯的先验Beta(8.5,3.5)怎么选的？**
A: 反映"有把握认为C程序更快，但不是压倒性优势"的信念。可根据领域知识调整。

**Q: 如果想证明C程序更快怎么办？**
A: 需要更多样本（建议>100）或更严格的实验控制，使效应变大。

**Q: HDI和置信区间有什么区别？**
A: HDI只包含最高密度部分，更直观；CI是频率主义概念，长期覆盖率相关。

---

## 文件清单

### 脚本文件 (*.R)
- ✓ `experiment_analysis.R` - 基础频率主义
- ✓ `bayesian_analysis.R` - 贝叶斯(参考v1)
- ✓ `bayesian_analysis_v2.R` - 贝叶斯(参考v2)
- ✓ `bayesian_analysis_final.R` - 贝叶斯(最终)
- ✓ `complete_experiment_analysis.R` - **完整版** ⭐

### 图表文件 (*.png)
- ✓ `experiment_normal_distribution.png` - Z分布
- ✓ `beta_distribution_comparison.png` - Beta对比
- ✓ `complete_experiment_analysis.png` - **推荐** ⭐
- ✓ `bayesian_analysis_final.png` - 论文风格
- ✓ 其他变体版本

### 文档文件
- ✓ `README.md` - 详细文档
- ✓ `QUICK_START.md` - 本文件

---

## 下一步

1. 运行 `complete_experiment_analysis.R`
2. 查看 `complete_experiment_analysis.png`
3. 参考 `README.md` 了解详细分析
4. 根据需要选择上述脚本进行修改

---

**最后更新**：2026-10-12
