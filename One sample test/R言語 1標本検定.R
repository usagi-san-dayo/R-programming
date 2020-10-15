data <- mtcars 

#1標本検定
t_test <- t.test(data$mpg, mu = 23)

tValue <- t_test$statistic    #検定統計量t値
df <- t_test$parameter        #t分布の自由度
pValue <- t_test$p.value      #p値
CI <- t_test$conf.int         #母平均の信頼区間
sampleMean <- t_test$estimate #標本平均
mu0 <- t_test$null.value      #帰無仮説の下での平均値の値

#検定結果をデータフレームにまとめる
t_testTable <- data.frame(mean = sampleMean, CILower = CI[1], CIUpper = CI[2], pvalue = pValue)

write.csv(t_testTable, "1標本検定結果.csv", row.names = F)#csvファイルへの出力

#検定を可視化する
#t分布
curve(dt(x, df), -4, 4, ylab = "f(x)", xlab = "x", main = "棄却域")
#t値の描画
arrows(tValue, 0, tValue, dt(tValue, df)[[1]], 0, col = "red")
axis(side = 1, at = round(tValue, 2), col = "red" ,col.axis = "red")
#棄却域の描画
arrows(qt(0.975, df), 0, qt(0.975, df), dt(qt(0.975, df), df)[[1]],
       0, col = rgb(0, 20/255, 1))
arrows(-qt(0.975, df), 0, -qt(0.975, df), dt(qt(0.975, df), df)[[1]],
       0, col = rgb(0, 20/255, 1))

xCoordinate <- c(seq(4, qt(0.975, df), length = 100), rep(qt(0.975, df), 100),
                 rev(seq(4, qt(0.975, df), length = 100)))
yCoordinate <- c(dt(xCoordinate[1:100], df), seq(0, dt(qt(0.975, df), df)[[1]],
                                                 length =100), rep(0, 100))

polygon(xCoordinate, yCoordinate, col = rgb(0, 20/255, 1, 0.5))
polygon((-1) * xCoordinate, yCoordinate, col = rgb(0, 20/255, 1, 0.5))

legend("topleft", c("t値"), lty = 1, col = "red")

"母平均の信頼区間"
boxplot(data$mpg, horizontal = T, xlab = "mpg", main = "母平均の信頼区間")
lines(c(mu0 ,mu0), c(1.3, 0.7), col = "red", lwd = 2)
lines(c(CI[1], CI[1]), c(1.3, 0.7), col = "blue", lwd = 2)
lines(c(CI[2], CI[2]), c(1.3, 0.7), col = "blue", lwd = 2)
legend("topleft", c("母平均", "95%信頼区間"), lty = 1, col = c("red", "blue"), lwd = 2)

#片側検定
t.test(data$mpg, mu = 23, alternative = "less") #左側仮説検定
t.test(data$mpg, mu = 23, alternative = "greater") #右側仮説検定
