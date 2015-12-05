
###############################################################
# 指数分布
###############################################################

trial_size = 7000; width <- .01;

# もともとの問題設定
p = 0.7; n = 10;
np <- p*n
cat("p=",p, ", n=", n, "np=", np)
p
# n→∞、p→0、np=一定 
n = 10000; p <- np/n
cat("p=",p, ", n=", n)

# ベルヌーイ分布に従う乱数生成
gen_exp_var <- function() {
  cnt <- 0
  while (TRUE) {
    cnt <- cnt + 1
    if (rbern(1, p)==1){
      return(cnt)  # 1が出たら何回目かを返す
    }
  }
}

data <- data.frame(x=rdply(trial_size, gen_exp_var())/n)
names(data) <- c("n", "x")

# 指数分布の密度関数
dens <- data.frame(y=dexp(seq(0, 1.5, 0.1), np)*trial_size*width)

ggplot() +
  layer(data=data, mapping=aes(x=x), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  )  + layer(data=dens, mapping=aes(x=seq(0, 1.5, 0.1), y=y), 
             geom="line", stat="identity", position="identity", colour="red"
  ) + ggtitle("Bernoulli to Exponential.")

