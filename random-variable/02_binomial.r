###############################################################
# 二項分布
###############################################################
# ベルヌーイ分布に従う変数をsample_size回行ったときの1の回数を
# trial_size分繰り返してベクトルに入れていく。

# パラメーター
p = 0.7
trial_size = 10000
sample_size = 30
set.seed(71)

# ベルヌーイ分布に従う乱数生成
gen_binom_var <- function() {
  return(sum(rbern(sample_size, p)))
}
result <- rdply(trial_size, gen_binom_var())

# 二項分布の密度関数
dens <- data.frame(y=dbinom(seq(sample_size), sample_size, 0.7))*trial_size

# グラフ描画
ggplot() +
  layer(data=result, mapping=aes(x=V1), geom="bar", stat = "bin",
        binwidth=1, fill="#6666ee", color="gray"
  ) + layer(data=dens, mapping=aes(x=seq(sample_size)+.5, y=y), 
            geom="line", stat="identity", position="identity",colour="red"
  ) + ggtitle("Bernoulli to Binomial.")