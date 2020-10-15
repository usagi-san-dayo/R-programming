#2標本検定
#2群の分散が等しいとき
data <- mtcars
twoSmpl_t_test <- t.test(data$mpg[data$vs == 0], data$mpg[data$vs == 1], var.equal = T)

twoSmpl_t_test <- t.test(mpg ~ vs, data = data, var.equal = T)  #formulaを用いた場合

tValue <- twoSmpl_t_test$statistic    #検定統計量t値
df <- twoSmpl_t_test$parameter        #t分布の自由度
pValue <- twoSmpl_t_test$p.value      #p値
CI <- twoSmpl_t_test$conf.int         #母平均の差の信頼区間
sampleMean <- twoSmpl_t_test$estimate #2群の標本平均
diffMu <- twoSmpl_t_test$null.value      #帰無仮説の下での母平均の差:μ1-μ2=0

#検定結果をデータフレームにまとめる
twoSmpl_testTable <- data.frame(statistics = tValue, CILower = CI[1],
                                CIUpper = CI[2], pvalue = pValue)

#2群の分散が等しくないとき
twoSmpl_t_test <- t.test(mpg ~ vs, data = data, var.equal = F)

tValue <- twoSmpl_t_test$statistic    #検定統計量t値
df <- twoSmpl_t_test$parameter        #t分布の自由度
pValue <- twoSmpl_t_test$p.value      #p値
CI <- twoSmpl_t_test$conf.int         #母平均の差の信頼区間
sampleMean <- twoSmpl_t_test$estimate #2群の標本平均
diffMu <- twoSmpl_t_test$null.value      #帰無仮説の下での母平均の差:μ1-μ2=0

twoSmpl_testTable <- rbind(twoSmpl_testTable, 
                           data.frame(statistics = tValue,
                                      CILower = CI[1], 
                                      CIUpper = CI[2],
                                      pvalue = pValue)
)

#ノンパラメトリックの場合
wilcoxon_test <- wilcox.test(mpg ~ vs, data = data)

wValue <- wilcoxon_test$statistic    #検定統計量w値
pValue <- wilcoxon_test$p.value      #p値        母平均の差の信頼区間
diff <- wilcoxon_test$null.value      #帰無仮説の下での分布の差: Δ

twoSmpl_testTable <- rbind(twoSmpl_testTable, 
                           data.frame(statistics = wValue[[1]],
                                      CILower = NA,
                                      CIUpper = NA, 
                                      pvalue = pValue)
)

rownames(twoSmpl_testTable) <- c("Student_t", "Welch_t", "Wilcoxon") #検定結果表の行名を検定名にする

#csvファイルへの出力
write.csv(twoSmpl_testTable, "母平均の差の検定.csv", row.names = T)

#片側仮説検定
t.test(mpg ~ vs, data = data, alternative = "less", var.equal = T)
t.test(mpg ~ vs, data = data, alternative = "greater", var.equal = T)

wilcox.test(mpg ~ vs, data = data, alternative = "less")
wilcox.test(mpg ~ vs, data = data, alternative = "greater")
#ggplot2を用いた2群の比較
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

data$vs <- as.factor(data$vs)
densityPlot <- ggplot(data, aes(x = mpg, y = ..density.., colour = vs, fill = vs))
densityPlot <- densityPlot + geom_histogram(position = "identity", bins = 25, alpha = 0.75)
densityPlot <- densityPlot + geom_density(stat = "density", position = "identity", alpha = 0.75)
densityPlot <- densityPlot + xlim(min(data$mpg), max(data$mpg))

densityPlot  #密度とヒストグラムの描画

data$vs <- as.factor(data$vs)
boxPlot <- ggplot(data, aes(x = mpg, y = vs, fill = vs)) 
boxPlot <- boxPlot + geom_boxplot(alpha = 0.75, colour = "gray")

boxPlot

#密度関数と箱ひげ図の描画
grid.arrange(densityPlot, boxPlot)

png("2標本検定.png", width = 600, height = 600)
grid.arrange(densityPlot, boxPlot)
dev.off()
