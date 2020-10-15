data <- iris
mean(data$Sepal.Length) #標本平均値
var(data$Sepal.Length) #不偏標本分散

#データの抽出
data[,colnames(data)[1]] #Sepal.Lengthのデータ
setdiff(colnames(data), "Species") #Species以外のデータ
data[data[,"Species"] == "setosa",]#setosa種のみのデータ

data2 <- data[,setdiff(colnames(data), "Species")]#Species以外のデータ

#データフレームに用いる関数
nrow(data)  #データフレームの行数
ncol(data)  #データフレームの列数
dim(data)   #データフレームの行、列
　
rownames(data)  #データフレームの行名
colnames(data)  #データフレームの列名

intersect(c(1,2,3,4,5), c(3,4,5))
setdiff(c(1,2,3,4,5), c(3,4,5))

#データフレームの結合
std_Sepal.Length <- (data2$Sepal.Length - mean(data2$Sepal.Length)) / sqrt(var(data2$Sepal.Length))
data2 <- cbind(data2, std_Sepal.Length)

mean_data <- round(apply(data2, 2, mean), 5) #Sepal.Length  Sepal.Width Petal.Length  Petal.Widthの平均値
var_data <- round(apply(data2, 2, var), 5) #不偏分散

data2 <- rbind(data2, mean_data, var_data) #平均、分散の行を結合する
rownames(data2) <- c(1:nrow(data), "mean","var")

data.frame(data$Sepal.Length, data$Petal.Length)  #data$Sepal.Length, data$Petal.Lengtからなるでーれフレームを作成

merge(data[1:50,],data2[46:100,])  #dataとdata2の共通部分を返す
merge(data[1:50,],data2[46:100,], all = T)  #dataとdata2を併合する

#データフレームの並び替え
sort(data2$Sepal.Length)  #昇順
rev(data2$Sepal.Length)   #降順

order(data2$Sepal.Length, decreasing = F)  #昇順
order(data2$Sepal.Length, decreasing = T)  #降順

#分割表作成
data_11 <- data[data$Sepal.Length > as.list(mean_data)$Sepal.Length & data$Species== "setosa",]
data_12 <- data[data$Sepal.Length <= as.list(mean_data)$Sepal.Length & data$Species== "setosa",]
data_21 <- data[data$Sepal.Length > as.list(mean_data)$Sepal.Length & data$Species!= "setosa",]
data_22 <- data[data$Sepal.Length <= as.list(mean_data)$Sepal.Length & data$Species!= "setosa",]

table <- data.frame(matrix(c(nrow(data_11),nrow(data_21),nrow(data_12),nrow(data_22)), nrow =2)) 

rownames(table) <- c("SpeciesがLengthである","SpeciesがLengthでない")
colnames(table) <- c("Sepal.Lengthが平均値より大きい","Sepal.Lengthが平均値以下")
table <- rbind(table, 総計=apply(table,2,sum))
table <- cbind(table, 総計=apply(table,1,sum))

#各データの平均値と不偏分散
apply(data2, 2, mean) #Sepal.Length  Sepal.Width Petal.Length  Petal.Widthの平均値
apply(data2, 2, var) #不偏分散

#データの相関係数
cor(data2$Sepal.Length, data2$Sepal.Width)
cor(data2$Sepal.Length, data2$Petal.Length)

#データの標準化
data2 <- data[,setdiff(colnames(data), "Species")]#Species以外のデータ
std_data <- scale(data2)

standarize <- function(data){#実際に作ってみる
  m <- apply(data, 2, mean)
  v <- apply(data, 2, var)
  std <- apply(data, 1, function(data){
    print(data)
    (data-m)/sqrt(v)
    }
    )
  return(t(std))
}

#データフレームをtxtファイルに保存
write.table(std_data, "標準化データ.txt", col.names = T, quote = F, append = F)  #標準化データ.txtとして保存
read.table("標準化データ.txt")                   #再読み込みできる

#データフレームをcsvファイルに保存
write.csv(std_data, "標準化データ.csv",  quote=F, row.names=F, fileEncoding="CP932")
read.csv("標準化データ.csv", fileEncoding="CP932")

#データのプロット
plot(data2$Sepal.Length, )

#Sepal.LengthとPetal.Lengthのプロット
plot(data2$Sepal.Length, xlab = "標本番号",  ylab = "length", col = "blue", main = "Lengthのプロット", ylim = c(1,8))
par(new=T)
plot(data2$Petal.Length,  xlab = "標本番号",  ylab = "length", col = "red", ylim = c(1,8))
legend("bottomright", legend = c("Sepal.Length", "Petal.Length"), pch = c(1,1), col = c("blue", "red"))

#データの相関図
plot(data2)

#箱ひげ図
boxplot(data2)

plot(data$Species, data$Sepal.Length , xlab = "", ylab = "") #plotを用いた箱ひげ図の描き方

#boxplot装飾
boxplot(data2, names=c("A", "B", "C", "D"), col=c("#993435", "#edae00", "#539952", "#000000"), main="Boxplot", xlab="Value", ylab="Entry", ylim=c(0, 10), width=c(1.1, 1.1, 1.1, 1.1), staplewex=0.8, horizontal=T, border="cyan4", notch=T, range=1)

#プロットの保存の仕方
png("プロット画像.png",width = 600 , height  = 600)
plot(data$Species, data$Sepal.Length , xlab = "", ylab = "") #plotを用いた箱ひげ図の描き方
dev.off()

#相関係数
cor(data$Sepal.Length, data$Petal.Length) #Sepal.LengthとPetal.Lengthの相関係数

#共分散行列
var(data2)

#相関行列
cov2cor(var(data2))

#相関行列_自作関数
cor_mat_fun <- function(data){ #相関行列
  mat <- matrix(nrow = ncol(data), ncol = ncol(data))
  for(i in 1:ncol(data)){
    for(j in 1:ncol(data)){
      mat[i,j] <- cor(data[,i], data[,j]) 
    }
  }
  return(mat)
}
cor_mat <- cor_mat_fun(data2)
cor_mat

colnames(cor_mat) <- colnames(data2)
cor_data.frame <- data.frame(cor_mat, row.names = colnames(data2))
write.csv(cor_data.frame, "相関行列.csv", append=F , quote=F, sep=",", row.names=T, col.names=T)

