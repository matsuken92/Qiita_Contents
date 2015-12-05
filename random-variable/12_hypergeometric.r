###############################################################
# 超幾何分布
###############################################################
# 非復元抽出
# 全部でN個から n個取り出す
# 1がM個、0がL個(N-M個)ある状況。

require(plyr)

N <- 50    # 全部でN個
M <- 12    # 1がM個
L <- N-M   # 0がL個
n <- 15     # n個取り出す

# 試行するセット回数
trial_size = 10000

# 標本空間の生成
sample_space <- append(rep(1, M), rep(0, L))

# １セット実行した結果を返す
gen_hypergeo_var <- function() {
  return(sum(sample_space[sample.int(N)[seq(n)]]))
}

result <- rdply(trial_size, gen_hypergeo_var())

x_range <- seq(0,n)
dens <- data.frame(dhyper(x_range, M, L, n)) * trial_size
names(dens) <- c("p")

#### グラフ描画
ggplot() +
  layer(data=result, mapping=aes(x=V1), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  ) + layer(data=dens, mapping=aes(x=x_range+.5, y=p), 
            geom="line", stat="identity", position="identity", colour="red"
  ) + ggtitle("Uniform to Hypergeometric")




