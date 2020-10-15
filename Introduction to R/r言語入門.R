#四則演算
1+2 #和
3-1 #差
2*2 #積
4/2 #商

#整数商,剰余,累乗
20%/%3 #整数商
20%%3  #剰余
2^2    #累乗

#代入
x <- 4
y <- x
z <- x*y
z
x <- 1

a <- 1+2 
b <- 2*2 
c <- b-a
c
  
#論理演算
x <- T
y <- F

x | y #論理和
x & y #論理積
!(x | y) #否定
!(x & y )

#ベクトル,行列
x <- c(1,2,3,4) 
y <- 1:4  

X <- matrix(1:4, nrow = 2, ncol = 2)
Y <- matrix(1:4, nrow = 2, ncol = 2, byrow = T) #byrow:要素を並べる順番を縦から横

#ベクトル,行列の演算
x <- 1:4
y <- c(3,6,1,2)

x + y #和
x - y #差
x * y #要素同士の積
x / y #要素同士の商 
x %/% y #要素同士の整数商
x %% y #要素同士の剰余
x %*% y #内積

X <- matrix(x, nrow = 2, ncol = 2)
Y <- matrix(y, nrow = 2, ncol = 2)

X + Y #和
X - Y #積
X * Y #要素同士の積
X / Y #要素同士の商 
X %/% Y #要素同士の整数商
X %% Y #要素同士の剰余
X %*% Y #内積

#if,for文
#####if文
x <- 0
y <- 1

if(x & y){ 
  print("hoge")
} else if(x == 0){
  print("hogehoge")
} else{
  print("bar") 
}

#####よくある間違い
if(x & y){ 
  print("hoge")
} 
else{   #####if文の}後に改行を入れるとエラーが発生します
  print("bar") 
}

####{}でくくったり関数内に定義するとエラーは出ません
{
  if(x & y){ 
  print("hoge")
  } 
  else{   #####if文の}後に改行を入れるとエラーが発生します
    print("bar") 
  }
}


######for文
for(i in 1:10){
  print("hoge")
}

for(i in c("hoge","hogehoge","bar")){
  print(i)
}

#関数
sample_mean <- function(x){ 　#ベクトルxの平均値を求める関数
  sum_x <- 0
  for(i in 1:length(x)){  #ベクトルxの要素の和
    sum_x <- sum_x + x[i]     
  }
  return(sum_x/length(x))
}

sample_mean(c(1,2,3)) #1,2,3の平均値

#ベクトルに用いる主な関数 
x <-c(1,2,3)

sum(x) #xの要素の和
mean(x) #xの要素の平均値
median(x) #xの要素の中央値
length(x) #xの要素の数


#リスト
x <- list(100,"hoge",1.0)  #異なるタイプの要素を複数格納できる
x[[1]]
x[[2]]

y <- list(年齢=100, 名前="Bill", 性別="M")  #ハッシュデータを構成できる。年齢と100が紐づけられる。
y$年齢  

#データフレーム
height <- c(170, 160, 180, 150, 100, 200)
weight <- c(60, 55, 80, 50, 20, 130)
data <- data.frame(height, weight)
data$height
data[1,2]

data$sex <- c("Male","Male","Male","Female","Female","Male")
data$BMI <- data$weight / ((data$height/100)^2)
