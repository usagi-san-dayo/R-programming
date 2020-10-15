#3群以上の母均値の差検定
#one way anova
install.packages("ggpubr")
install.packages("tidyverse")
install.packages("rstatix")

install.packages("ggplot2")
install.packages("gridExtra")

library(ggpubr)
library(rstatix)

library(ggplot2)
library(gridExtra)

#分散分析ANOVAの前提条件
#正規性の確認
qqnorm(iris$Sepal.Width ,ylab = "wid")
qqline(iris$Sepal.Width)

shapiro_test <- shapiro.test(t(iris$Sepal.Width)) 
shapiro_test

#群間の等分散性
bartlett_test <- bartlett.test(iris$Sepal.Width ~ iris$Species)
bartlett_test

#anova
anova <- anova_test(data = iris, Sepal.Length ~ Species)

#検定結果
effect <- anova$Effect
DFn <- anova$DFn
DFd <- anova$DFd
FValue <- anova$F
pValue <- anova$p

#検定結果をデータフレームに格納
one_way_ANOVA <- data.frame(effect = effect, Fvalue = FValue, pvapue = pValue)
write.csv(one_way_ANOVA, "一元分散分析.csv", row.names = F)

#事後テスト
pwc <- tukey_hsd(iris, Sepal.Width ~ Species) 

#データフレームに格納
post_hoc_test <- data.frame(levelA = pwc$group1, levelB = pwc$group2,
                           ci.low = pwc$conf.low, ci.up = pwc$conf.high, pValue = pwc$p.adj)

write.csv(post_hoc_test, "Tukey多重比較.csv", row.names = F)

#検定のプロット
boxPlot <- ggboxplot(iris, x = "Species", y = "Sepal.Width", fill = "Species") +
  stat_pvalue_manual(pwc[pwc$term == "Species", ], label = "p.adj", y.position = c(5, 6, 7)) +
  labs(subtitle = get_test_label(anova[anova$Effect == "Species", ], detailed = TRUE),
       caption = get_pwc_label(pwc[pwc$term == "Species", ]))

boxPlot
#two way anova
data <-  ToothGrowth
data$dose <- as.factor(data$dose) 

#正規性の確認
qqnorm(data$len ,ylab = "len")
qqline(data$len)

shapiro_test <- shapiro.test(t(data$len)) 

#群間の等分散性
bartlett_test_dose <- bartlett.test(data$len ~ data$dose)
bartlett_test_supp <- bartlett.test(data$len ~ data$supp)
bartlett_test_dose
bartlett_test_supp

#anova
anova <- anova_test(data = data, len ~ dose + supp)

anova_test(data = data, len ~ dose * supp) #交互作用あり

#検定結果
effect <- anova$Effect
DFn <- anova$DFn
DFd <- anova$DFd
FValue <- anova$F
pValue <- anova$p

two_way_ANOVA <- data.frame(effect = effect, Fvalue = FValue, pvapue = pValue)
rownames(two_way_ANOVA) <- rownames(anova[[1]])

write.csv(two_way_ANOVA, "2元配置分散分析.csv", row.names = T)

#多重比較
pwc <- tukey_hsd(data, len ~ supp +  dose) 

pwc #多重比較の結果

#データフレームに格納
post_hoc_test <- data.frame(factor = pwc$term, levelA = pwc$group1, levelB = pwc$group2,
                            ci.low = pwc$conf.low, ci.up = pwc$conf.high, pValue = pwc$p.adj)

#csvファイルへの出力
write.csv(post_hoc_test, "2元配置Tukey多重比較.csv", row.names = F)

#プロット
boxPlot_supp <- ggboxplot(data, x = "supp", y = "len", fill = "supp") +
    stat_pvalue_manual(pwc[pwc$term == "supp", ], label = "p.adj", y.position = 38) +
    labs(subtitle = get_test_label(anova[anova$Effect == "supp", ], detailed = TRUE),
    caption = get_pwc_label(pwc[pwc$term == "supp", ]))
  
boxPlot_dose <- ggboxplot(data, x = "dose", y = "len", fill = "dose") +
  stat_pvalue_manual(pwc[pwc$term == "dose", ], label = "p.adj", y.position = c(30, 36, 42)) +
  labs(subtitle = get_test_label(anova[anova$Effect == "dose", ], detailed = TRUE),
       caption = get_pwc_label(pwc[pwc$term == "dose", ]))

grid.arrange(boxPlot_supp, boxPlot_dose, nrow = 1)
