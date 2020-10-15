#データ整形の方法
df <- read.csv("SSDSE-2020A.csv", fileEncoding = "CP932")

for(i in 1:ncol(df)){  ##データ整形の練習として複数の種類の欠損値を代入
  for (j in 1:10){
    random <- sample(3:nrow(df), 30)   #sample()ベクトルの要素をランダムに返す
    df[random[1:10], i] <- ""
    df[random[11:20], i] <- "欠損値"
    df[random[21:30], i] <- "unknown"
  }
}
  #NA挿入後のデータフレームを保存（ここでは列数が多いので30列まで扱う）
write.csv(df, paste0("SSDSE-", 2020-count ,"A(NA挿入後).csv"), fileEncoding = "CP932", row.names = F)

  #準備が整ったので、再度データを読み込む
df <- read.csv(paste0("SSDSE-", 2020-count, "A(NA挿入後).csv"), fileEncoding = "CP932")

  #データの構成の確認のために、列名を参照する
colnames(df)  #変な列名を返します

  #1,2列目を確認して、どんなデータになっているか見てみる
df[1:4,1:5]  #列名にcode、1行目にcodeに対応する年度が入っています
　　　　　　 #2行目にはcodeに対応する地域がと年齢階級
             #3行目以降はデータが格納されています

  #データの列名としては、1,2行目のみ必要そうなので、2行を組み合わせて列名とする
colnames(df) <- paste0(df[1,], "_" ,df[2,])  #念のため、区切り文字として"_"を用いる
df <- df[-c(1,2),]  　　　                   #1,2行目を消去する

  #1~10行,1~5列のデータを見てみる
df[1:10,1:5]  #列名が簡潔に整理されていることが確認できます  

  #欠損値を整理する
#for文を用いた例
for(i in 1:ncol(df)){
  df[,i] <- replace(df[,i], df[,i] == "" | df[,i] == "欠損値" | df[,i] == "unknown", NA)
} 
  #apply関数を用いた例
#df<- apply(df, 2, function(x){
#  return(replace(x, x == "" | x == "欠損値" | x == "unknown" , NA))
#  }
#)

  #数値データの整理
colnames(df)[4:ncol(df)]  #4列名以降が人口や人数を表すので、4列目以降をnumaric型にする
  apply(df[,4:ncol(df)], 2, is.numeric)
  
  #for文を用いた例
for(i in 4:ncol(df)){
  df[,i] <- as.numeric(df[,i])
}
  
#apply関数を用いた例
#df[,4:ncol(df)]<- apply(df[,4:ncol(df)], 2, function(x){
#  return(as.numeric(x))
#}
#)
  
  #因子データの整理
colnames(df)[1:3]  #2,3列目のデータは都道府県、市区町村であるため、factor型にする
  apply(df[,c(2,3)], 2, is.factor)
  
  #for文を用いた例
for(i in 1:3){
  df[,i] <- as.factor(df[,i])
}
  
  #apply関数を用いた例
#df[,1:3]<- apply(df[,1:3], 2, function(x){
#  return(as.factor(x))
#}
#)

