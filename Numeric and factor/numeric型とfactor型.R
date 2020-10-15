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
write.csv(df, paste0("SSDSE-2020A(NA挿入後).csv"), fileEncoding = "CP932", row.names = F)
#準備が整ったので、再度データを読み込む
df <- read.csv(paste0("SSDSE-2020A(NA挿入後).csv"), fileEncoding = "CP932")

#データの列名としては、1,2行目のみ必要そうなので、2行を組み合わせて列名とする
colnames(df) <- paste0(df[1,], "_" ,df[2,])  #念のため、区切り文字として"_"を用いる
df <- df[-c(1,2),]  　　　                   #1,2行目を消去する

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
#for文を用いた例
for(i in 1:3){
  df[,i] <- as.factor(df[,i])
}
  #apply関数を用いた例
#df[,1:3]<- apply(df[,1:3], 2, function(x){
#  return(as.factor(x))
#}
#)

#データ整形2
#数値データをカテゴリカルデータにする
#総人口を"0~5000", "5000~10000", "10000~15000","15000~20000","20000~"で分割する
bar <- cut(df[,"2015_総人口"], breaks = c(0, 5000, 10000, 15000, 20000,1e+10), labels = c("0~5000", "5000~10000", "10000~15000","15000~20000","20000~"), right = FALSE)
df <- cbind(df, 総人口カテゴリカル = bar)  #dfにカテゴリカルデータを追加する
#df <- transform(df, 総人口カテゴリカル = bar)  #transformを用いた例

tmp <- cut(df[,"2015_総人口"], breaks = 100*(0:10000), right = FALSE)  #1万個の区間で分割

#因子の水準を変更してみる
hokkaidou <- list(c("北海道"),region = "北海道地方")
touhoku <- list(c("青森県", "岩手県", "宮城県", "秋田県", "山形県", "福島県"), region = "東北地方")
kantou <- list(c("茨城県", "栃木県","群馬県" , "埼玉県", "千葉県", "東京都", "神奈川県"), region = "関東地方")
tyuubu <- list(c("新潟県", "富山県","石川県" ,"福井県" ,"山梨県" ,"長野県" , "岐阜県", "静岡県", "愛知県"), region = "中部地方")
kinki <- list(c("三重県", "滋賀県", "京都府", "大阪府", "兵庫県", "奈良県", "和歌山県"), region = "近畿地方")
tyuugoku <- list(c("鳥取県", "島根県", "岡山県","広島県" , "山口県"), region = "中国地方")
sikoku <- list(c("徳島県", "香川県", "愛媛県", "高知県"), region = "四国地方")
kyuusyuu <- list(c("福岡県", "佐賀県", "長崎県", "熊本県", "大分県", "宮崎県", "鹿児島県", "沖縄県"), region = "九州地方")

list_region <- list(hokkaidou, touhoku, kantou, tyuubu, kinki, tyuugoku, sikoku, kyuusyuu)

bar <- as.character(df[,"年度_都道府県"])  #character型に変更する

for(i in list_region){
  for(j in i[[1]]){
    bar <- replace(bar, bar == j, i$region)
  }
  
}

bar <- as.factor(bar)

#水準の順番を変更する
bar <- factor(bar, levels = c("北海道地方", "東北地方", "関東地方", "中部地方", "近畿地方", "中国地方", "四国地方", "九州地方"))
levels(bar)  #水準を返す
nlevels(bar) #水準の数

df <- cbind(df, 地方 = bar)  
#df <- transform(df, 地方 = bar)  #transformを用いた例


#水準をプールする
#北海道地方と東北地方をプールする
bar <- as.character(bar)
bar <- replace(bar, bar == "東北地方" | bar == "北海道地方", "東北地方.北海道地方")
bar <- as.factor(bar)
levels(bar)

df <- cbind(df, 地方.東北北海道プール = bar)
#df <- transform(df, 地方.東北北海道プール = bar)  #transformを用いた例



#factorの水準名の変更
bar <- factor(df[,"総人口カテゴリカル"], levels = levels(df[,"総人口カテゴリカル"]), labels = c("とても少ない" ,"少ない" ,"普通" ,"多い" ,"とても多い"))

#注意characterに変更後factorにすると水準が文字コード順に並ぶ
bar <- as.factor(as.character(bar)) 

#factorの水準の大小関係の変更
bar <- ordered(bar, levels = c("とても少ない" ,"少ない" ,"普通" ,"多い" ,"とても多い"))


#factor型とnumeric型の注意点　
#例として2015年総人口を10000ずつ分割
bar <- cut(df[,"2015_総人口"], breaks = 10000*(0:100), labels = 10000*c(0:99) , right = FALSE)  #1万個の区間で分割

#よくある間違い
as.numeric(bar)  #factorをnumericにすると数値が水準のindexになります

as.numeric(as.character(bar))  #数値のまま参照できる

