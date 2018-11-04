# データを読み込む
df <- read.csv("/directory/file.csv", header = T, row.names = 1, fileEncoding = "UTF-8")
nrow(df)
ncol(df)

# 列数ncolの取得
n <- ncol(df)
n
nn <- 0
# 組み合わせ数の計算 nCm
for (i in 1:(n-1)){
  nn <-nn+(n-i)
}
nn

# クラメールの連関係数用ライブラリの読み込み
library(vcd)

output <- matrix(0, nrow=nn, ncol=4)
k <- 0

# （不使用）列ラベル名を指定してクロス表作成
# df2 <- table(df$Q1, df$Q2)
# df2

# 指定したクロス表を列番号指定で出力

# crossRes <- table(df[,1], df[, 2])
# crossRes

# crossRes <- table(df [c(3,4)])
# crossRes

# クラメール連関係数の算出
# summary(assocstats((crossRes)))
# assocRes$chisq_tests
# assocRes <- assocstats(crossRes)
# assocRes$cramer

#chisq_testsオブジェクトの出力のうちピアソンのχ2値のp値（P(>X^2)のセルがp値
# 下のやつを見ればわかる（非表示）
# tmp1 <- assocRes$chisq_tests
# p_val <- assocRes$chisq_tests[2, 3]

# 連関度が高いと判断する閾値：クラメールV0.3以上
# 帰無仮説：２つのカテゴリー変数間には連関がない(独立である）
# 有意性検定のα値：5％
for(i in 1:(n-1)){
  for(j in (i+1):n){
    res <- table(df[,i], df[,j])
    resAssoc <- assocstats(res)
    p_val <- resAssoc$chisq_tests[2,3]
    cramer_v <- resAssoc$cramer
    if(cramer_v >= 0.2 && p_val <= 0.05){
       k <- k+1
       output[k, 1] <- i
       output[k, 2] <- j
       output[k, 3] <- cramer_v
       output[k, 4] <- p_val
    }
  }
}
colnames(output)<-c("Q","Q","cramerV","p-value")
options(scipen = 10)
head(output)
