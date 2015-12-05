
###############################################################
# ベータ分布
###############################################################
# 独立に一様分布 U(0,1) に従う p+q-1 個の確率変数を大きさの順に並べ替えたとき，
# 小さい方から p 番め（大きい方からは q 番目）の確率変数 X の分布がベータ分布 B(p,q) となります。
# http://www.kwansei.ac.jp/hs/z90010/sugakuc/toukei/beta/beta.htm

install.packages("Rlab")
install.packages("ggplot2")
install.packages("actuar")
require(actuar)
require(ggplot2)
require(Rlab)

###############################################################
# 一様分布
###############################################################
width <- 0.03
p <- 0.5
digits_length <- 30
set_size      <- 3
trial_size    <- 30000

gen_unif_rand <- function() {
  # digits_length桁の2進少数をベルヌーイ分布に従う乱数から生成
  return (sum(rbern(digits_length, p) * (rep(1/2, digits_length) ** seq(digits_length))))
}

gen_rand <- function(){  
  return( rdply(set_size, gen_unif_rand())$V1 )
}
unif_dataset <- rlply(trial_size, gen_rand, .progress='text')

# 独立に一様分布 U(0,1) に従う p+q-1 個の確率変数を大きさの順に並べ替えたとき，
# 小さい方から p 番め（大きい方からは q 番目）の確率変数 X の分布がベータ分布 B(p,q) となります。
# 800個サンプルがあるので、400番目、後ろから399番目

p <- ceiling(set_size * 0.5) 
q <- set_size - p + 1
cat("p:", p, ", q:",q)

get_nth_data <- function(a){
  return(a[order(a)][p])
}

disp_data <- data.frame(lapply(unif_dataset, get_nth_data))
names(disp_data) <- seq(length(disp_data))
disp_data <- data.frame(t(disp_data))
names(disp_data) <- "V1"

x_range <- seq(0, 1, 0.001)
dens <- data.frame(y=dbeta(x_range, p, q)*trial_size*width)

ggplot() +
  layer(data=disp_data, mapping=aes(x=V1), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  ) + layer(data=dens, mapping=aes(x=x_range, y=y), 
            geom="line", stat="identity", position="identity", colour="red"
  ) + ggtitle("Bernoulli to Beta")



