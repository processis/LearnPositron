library(tidyverse)

#儿童数据分析
#读取数据
ET=read.csv("ertfysj.csv")

#柱状图
hist(ET$month)

#散点图
plot(ET$month)
plot(ET$height)
plot(ET$month,ET$height)

# 添加回归线
abline(lm(ET$height ~ ET$month), col = "red")

#回归分析
relation <- lm(ET$height~ET$month)

print(relation)

#DPPSS数据分析
#读取数据
DPPSS=read.csv("DPPSSdataV01.csv")
DPPSS

#基础分析
summary(DPPSS)

#cor.test()检验配对样本之间的相关性。它同时返回相关系数和相关的显着性水平（p值）
#cor.test(x, y, method=c("pearson", "kendall", "spearman"))

#pearson连续

cor.test(DPPSS$Parts.., DPPSS$D.KSLOC, method=c("pearson"))


#spearman非连续
cor.test(DPPSS$CSat, DPPSS$Ship.Risk, method=c("spearman"))

td<-data.frame(DPPSS$Ship.Risk,DPPSS$CSat,DPPSS$Fielded.Volatility,DPPSS$Maint)
cor (td, method="spearman")

绘制相关性热图
library(corrplot)
tdc<-cor (td, method="spearman")
corrplot(tdc)

#柱状图

barplot(table(DPPSS$Language),main="Machine Language",names=c("C","C++","JAVA"))
barplot(table(DPPSS$DevPlatform),main="DevPlatform",names=c("Windows","Unix","Linux"))
hist(DPPSS$Maint)#必须为数值


#卡方检验
ka<-xtabs(~DPPSS$Ship.Risk+DPPSS$Experience,data=DPPSS)
chisq.test(ka)

ka1<-xtabs(~DPPSS$Defects.Inspect+DPPSS$Experience,data=DPPSS)
chisq.test(ka1)


#logistics
# 单因素Logistic
Turnover<-as.factor(DPPSS$Turnover)#使用as.factor函数将因子向量转化为字符串向量
Defects.Inspect<-as.factor(DPPSS$Defects.Inspect)
LOG1<-glm(Turnover~Defects.Inspect,family=binomial(link ='logit'))
LOG1

#方差分析
aov1 <- aov(DPPSS$D.KSLOC~DPPSS$TesterType)  
aov1

#哑变量
#回归分析
relation <- lm(DPPSS$D.KSLOC~DPPSS$TesterType)

print(relation)



relation <- lm(DPPSS$D.KSLOC~DPPSS$Test.internal+DPPSS$Test.outside+DPPSS$Test.user)

print(relation)