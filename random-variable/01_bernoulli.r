
###############################################################
# ベルヌーイ分布
###############################################################
# ベルヌーイ分布からのサンプリングを実行

# パラメーター
p = 0.7
trial_size = 10000
set.seed(71)

# ベルヌーイ分布に従う乱数生成
data <- rbern(trial_size, p)
# ベルヌーイ分布の確率分布
dens <- data.frame(y=c((1-p),p)*trial_size, x=c(0, 1))

# グラフ描画
ggplot() +
    layer(data=data.frame(x=data), mapping=aes(x=x), geom="bar",
          stat="bin", bandwidth=0.1
) + layer(data=dens, mapping=aes(x=x, y=y), geom="bar",
          stat="identity", width=0.05, fill="#777799", alpha=0.7)