df <- mtcars  #自動車のテストのデータセット

#分割表の作成(例)
#1. vsとamに関する度数
table_11 <- nrow(df[df$vs == 0 & df$am == 0, ]) #vs=0かつam=0
table_12 <- nrow(df[df$vs == 0 & df$am == 1, ]) #vs=0かつam=1
table_21 <- nrow(df[df$vs == 1 & df$am == 0, ]) #vs=1かつam=0
table_22 <- nrow(df[df$vs == 1 & df$am == 1, ]) #vs=1かつam=1

table <- data.frame(c(table_11, table_21), c(table_12, table_22))
rownames(table) <- c(0, 1)
colnames(table) <- c(0, 1) 

table  #2×2分割表

#2. vsとamに関するmpgの和
sumTable_11 <- sum(df$mpg[df$vs == 0 & df$am == 0]) #vs=0かつam=0
sumTable_12 <- sum(df$mpg[df$vs == 0 & df$am == 1]) #vs=0かつam=1
sumTable_21 <- sum(df$mpg[df$vs == 1 & df$am == 0]) #vs=1かつam=0
sumTable_22 <- sum(df$mpg[df$vs == 1 & df$am == 1]) #vs=1かつam=1

sumTable <- data.frame(c(sumTable_11, sumTable_21), c(sumTable_12, sumTable_22))
rownames(sumTable) <- c(0, 1)
colnames(sumTable) <- c(0, 1)

sumTable #和の2×2分割表

#3. vsとamに関するmpgの平均
meanTable <- sumTable / table #和の各要素を度数の各要素で割る

meanTable #平均の2×2分割表

#xtabsを用いた2×2分割表
#1. vsとamに関する度数
table <- xtabs(~ vs + am, data = df)

#2. vsとamに関するmpgの和
sumTable <- xtabs(mpg ~ vs + am, data = df)

#3. csとamに関するmpgの平均
meanTable <- sumTable / table 


#csvファイルへの出力
matrixTable <- as.matrix(table)        #扱いやすよう行列にする
matrixSumTable <- as.matrix(sumTable)
matrixMeanTable <- as.matrix(meanTable)

table <- cbind(c("", "", "vs", ""), c("", "", rownames(matrixTable)), 
               rbind(c("am", ""), colnames(matrixTable), matrixTable))
sumTable <- cbind(c("", "", "vs", ""), c("", "", rownames(matrixSumTable)), 
               rbind(c("am", ""), colnames(matrixSumTable), matrixSumTable))
meanTable <- cbind(c("", "", "vs", ""), c("", "", rownames(matrixMeanTable)), 
               rbind(c("am", ""), colnames(matrixMeanTable), matrixMeanTable))

write.table(table, "分割表（度数）.csv", quote = F, sep = ",", row.names = F, col.names = F)
write.table(sumTable, "分割表（和）.csv", quote = F, sep = ",", row.names = F, col.names = F)
write.table(meanTable, "分割表（平均）.csv", quote = F, sep = ",", row.names = F, col.names = F)

#多元分割表の作成(例)
#1. vsとamとgearに関する度数
tableList <- NULL #分割表のリストを初期化
sumTableList <- NULL
meanTableList <- NULL
for(i in 1:nlevels(as.factor(df$gear))){
  gearLevels <- levels(as.factor(df$gear))
  table_11 <- nrow(df[df$vs == 0 & df$am == 0 & df$gear == gearLevels[i], ]) #vs=0かつam=0かつgear=gearLevels[i]
  table_12 <- nrow(df[df$vs == 0 & df$am == 1 & df$gear == gearLevels[i], ]) #vs=0かつam=1かつgear=gearLevels[i]
  table_21 <- nrow(df[df$vs == 1 & df$am == 0 & df$gear == gearLevels[i], ]) #vs=1かつam=0かつgear=gearLevels[i]
  table_22 <- nrow(df[df$vs == 1 & df$am == 1 & df$gear == gearLevels[i], ]) #vs=1かつam=1かつgear=gearLevels[i]
  
  sumTable_11 <- sum(df$mpg[df$vs == 0 & df$am == 0 & df$gear == levels(df$gear)[i]]) #vs=0かつam=0かつgear=gearLevels[i]
  sumTable_12 <- sum(df$mpg[df$vs == 0 & df$am == 1 & df$gear == levels(df$gear)[i]]) #vs=0かつam=1かつgear=gearLevels[i]
  sumTable_21 <- sum(df$mpg[df$vs == 1 & df$am == 0 & df$gear == levels(df$gear)[i]]) #vs=1かつam=0かつgear=gearLevels[i]
  sumTable_22 <- sum(df$mpg[df$vs == 1 & df$am == 1 & df$gear == levels(df$gear)[i]]) #vs=1かつam=1かつgear=gearLevels[i]
  
  table <- data.frame(c(table_11, table_21), c(table_12, table_22))
  sumTable <- data.frame(c(sumTable_11, sumTable_21), c(sumTable_12, sumTable_22))
    
  rownames(table) <- c(0, 1)
  colnames(table) <- c(0, 1)
  rownames(sumTable) <- c(0, 1)
  colnames(sumTable) <- c(0, 1)
  
  tmp <- list(gear = NULL, table = NULL)
  tmp$gear <- gearLevels[i]
  tmp$table <- table
  tableList[[i]] <- tmp
  
  tmp$table <- sumTable
  sumTableList[[i]] <- tmp
  
  tmp$table <- sumTable / table
  meanTableList[[i]] <- tmp
}

tableList     #度数
sumTableList  #和
meanTableList #平均

#xtabsを用いた多元分割表
#1. 度数
table <- xtabs(~ vs + am + gear, data = df)

#2. 和　
sumTable <- xtabs(mpg ~ vs + am + gear, data = df)

#3. 平均
meanTable <- table / sumTable

#ftableを用いたピボットテーブルへの変換
table <- ftable(table)
sumTable <- ftable(sumTable)
meanTable <- ftable(meanTable)

#csvファイルへの出力
matrixTable <- as.matrix(table)        #扱いやすよう行列にする
matrixSumTable <- as.matrix(sumTable)
matrixMeanTable <- as.matrix(meanTable)

table <- cbind(c("gear", "vs_am", rownames(matrixTable)), 
      rbind(colnames(matrixTable), rep("", 3), matrixTable))
sumTable <- cbind(c("gear", "vs_am", rownames(matrixTable)), 
      rbind(colnames(matrixTable), rep("", 3), matrixTable))
meanTable <- cbind(c("gear", "vs_am", rownames(matrixTable)), 
      rbind(colnames(matrixTable), rep("", 3), matrixTable))


write.table(table, "多元分割表（度数）.csv", quote = F, sep = ",", row.names = F, col.names = F)
write.table(sumTable, "多元分割表（和）.csv", quote = F, sep = ",", row.names = F, col.names = F)
write.table(meanTable, "多元分割表（平均）.csv", quote = F, sep = ",", row.names = F, col.names = F)

