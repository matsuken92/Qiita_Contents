###############################################################
# カイ二乗分布 (自由度:1)
###############################################################
# 正規分布に従う乱数を二乗してヒストグラムにする。

# パラメーター
sample_size <- 1000
trial_size <- 100000
width <- 0.3
set.seed(71)

# ベルヌーイ分布に従う乱数生成
res <- c()
for (i in sequence(trial_size)) {
  res <- rbind(res, sum(rbern(sample_size, p)))
}

res <- (res - mean(res))/sd(res)    # 標準化
data <- data.frame(x=(res**2))

# カイ二乗分布の密度関数(自由度=1)
xx <- seq(0,20,0.1)
dens <- data.frame(y=dchisq(x=xx, df=1)*trial_size*width)
length(seq(sample_size))
# グラフ描画
ggplot() +
  layer(data=data, mapping=aes(x=x), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  ) + layer(data=dens, mapping=aes(x=xx, y=y), 
            geom="line", stat="identity", position="identity", colour="red"
  )
