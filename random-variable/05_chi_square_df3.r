###############################################################
# カイ二乗分布 (自由度:3)
###############################################################
# 正規分布に従う乱数を二乗してヒストグラムにする。

set.seed(71)
# パラメーター
p <- 0.7
n <- 1000
trial_size <- 100000
width <- 0.3
df <- 3

# ベルヌーイ分布に従う乱数生成(3まわし)
gen_binom_var <- function() {
  return(sum(rbern(n, p)))
}

gen_chisq_var <- function() {
  result <- rdply(trial_size, gen_binom_var())
  return(((result$V1 - mean(result$V1))/sd(result$V1))**2)
}

# 自由度dfの分だけ生成する
result <- rlply(df, gen_chisq_var(),.progress = "text")

res <- result[[1]] + result[[2]] + result[[3]]
data <- data.frame(x=res)

# カイ二乗分布の密度関数(自由度=3)
xx <- seq(0,20,0.1)
dens <- data.frame(y=dchisq(x=xx, df=df)*trial_size*width)
# グラフ描画
ggplot() +
  layer(data=data, mapping=aes(x=x), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  ) + layer(data=dens, mapping=aes(x=xx, y=y), 
            geom="line", stat="identity", position="identity", colour="red"
  ) + ggtitle("Bernoulli to Chisquare")

