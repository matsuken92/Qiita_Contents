###############################################################
# ポアソン分布
###############################################################
trial_size = 5000; width <- 1;

# もともとの問題設定
p = 0.7; n = 30;
np <- p*n
cat("p=",p, ", n=", n, "np=", np)

# n→∞、p→0、np=一定 
n = 100000; p <- np/n
cat("p=",p, ", n=", n)

# ベルヌーイ分布に従う乱数生成
gen_binom_var <- function() {
  return(sum(rbern(n, p)))
}
result <- rdply(trial_size, gen_binom_var())

# ポアソン分布の密度関数
dens <- data.frame(y=dpois(seq(20), np))*trial_size

# グラフ描画
ggplot() +
  layer(data=result, mapping=aes(x=V1), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  ) + layer(data=dens, mapping=aes(x=seq(20)+.5, y=y), 
            geom="line", stat="identity", position="identity", colour="red"
  ) + ggtitle("Bernoulli to Poisson.")
