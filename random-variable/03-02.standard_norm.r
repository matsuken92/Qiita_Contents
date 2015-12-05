set.seed(73)

# パラメーター
n <- 10000
p <- 0.7
trial_size = 30000
width=0.18


# ベルヌーイ分布に従う乱数生成
gen_binom_var <- function() {
  return(sum(rbern(n, p)))
}
result <- rdply(trial_size, gen_binom_var())

m <- mean(result$V1)
sd <- sd(result$V1)

result <- (result - m)/sd

# 標準正規分布の密度関数
dens <- data.frame(y=dnorm(seq(-4,4,0.05), mean=0, 
                           sd=1)*trial_size*width)

# グラフ描画
ggplot() +
  layer(data=result, mapping=aes(x=V1), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  ) + layer(data=dens, mapping=aes(x=seq(-4,4,0.05), y=y), 
            geom="line", stat="identity", position="identity", colour="red"
  ) + ggtitle("Bernoulli to Standard Normal.") 