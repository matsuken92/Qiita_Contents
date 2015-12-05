###############################################################
# 正規分布
###############################################################
# 二項分布の正規近似 (nを大きくする)
set.seed(72)

# パラメーター
n <- 10000
p <- 0.7
trial_size = 10000
width=10


# ベルヌーイ分布に従う乱数生成
gen_binom_var <- function() {
  return(sum(rbern(n, p)))
}
result <- rdply(trial_size, gen_binom_var())

# 正規分布の密度関数
dens <- data.frame(y=dnorm(seq(6800,7200), mean=n*p, 
                           sd=sqrt(n*p*(1-p)))*trial_size*width)

# グラフ描画
ggplot() +
  layer(data=result, mapping=aes(x=V1), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  ) + layer(data=dens, mapping=aes(x=seq(6800,7200), y=y), 
            geom="line", stat="identity", position="identity", colour="red"
  ) + ggtitle("Bernoulli to Normal.") 