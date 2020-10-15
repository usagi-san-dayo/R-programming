#データセットの読み込み
df <- read.csv("SSDSE-2020B.csv", fileEncoding = "CP932")

#データセットを年度別のデータに分割する
year <- levels(as.factor(df[2:nrow(df),"Year"]))

for(i in 1:length(year)){
  tmp <- NULL
  tmp <- df[df[,"Year"] == year[i] , setdiff(colnames(df), "Year")]
  tmp <- rbind(df[1, setdiff(colnames(df), "Year")], tmp)
  write.csv(tmp, paste0("SSDSE-2020B_", year[i] ,"_.csv"), row.names = F, fileEncoding = "CP932")
}

#複数のデータセットをリストに格納する
dfList <- NULL
for(i in 1:length(year)){
  dfList[[i]] <- read.csv(paste0("SSDSE-2020B_",  year[i], "_.csv"), fileEncoding = "CP932")
}

#列名Yearを挿入
for(i in 1:length(dfList)){
  dfList[[i]] <- data.frame(Code = dfList[[i]][, 1], Year = c("年度", rep(year[i], nrow(dfList[[i]])-1)),
                                                   dfList[[i]][, 2:ncol(dfList[[i]])])
}

#データフレームの併合
#merge用いた例
tmp <- dfList[[1]][numeric(0),]
for(i in 1:length(dfList)){
  tmp <- merge(tmp, dfList[[i]][2:nrow(dfList[[i]]),], all = T)
}

df <- rbind(dfList[[1]][1,], tmp)

#rbindを用いた例
tmp <-  dfList[[1]][numeric(0),]
for(i in 1:length(dfList)){
  tmp <- rbind(tmp, dfList[[i]][2:nrow(dfList[[i]]),])
}
df <- rbind(dfList[[1]][1,], tmp)

#都道府県でソートする
bar <- df[2:nrow(df),]
tmp <- as.factor(bar[, "Prefecture"]) 
df <- rbind(df[1,], bar[order(as.numeric(tmp)),])

write.csv(df, "SSDSE2020B(併合後).csv", row.names = F, fileEncoding = "CP932")

#多元分割表
#2元分割表
colnames(df) <- df[1,]
df <- df[-1,]

for(i in 4:ncol(df)){        #数値化
  df[,i] <- as.numeric(df[,i])
}

table <- xtabs(総人口 ~ 都道府県 + 年度 ,data = df)

#3元分割表
bar <- cut (df[,"保育所等利用待機児童数"], breaks  = c(0, mean(df[,"保育所等利用待機児童数"]), 10000) ,labels = c("0~平均", "平均~"), right = F)
df <- cbind(df, 待機児童数カテゴリカル = bar)

bar <- cut (df[,"各種学校数"], breaks  = c(0, mean(df[,"各種学校数"]), 10000) ,labels = c("0~平均", "平均~"), right = F)
df <- cbind(df, 各種学校数カテゴリカル = bar)

table <- xtabs(総人口 ~ 都道府県 + 各種学校数カテゴリカル + 待機児童数カテゴリカル,　data = df)

Table <- ftable(table)
Table <- ftable(table, row.vars ="待機児童数カテゴリカル")

