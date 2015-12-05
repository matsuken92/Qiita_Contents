
###############################################################
# ガンマ分布
###############################################################

trial_size = 7000; width <- .035;

# もともとの問題設定
p = 0.7; n = 10;
np <- p*n
cat("p=",p, ", n=", n, "np=", np)
# n→∞、p→0、np=一定 
n = 10000; p <- np/n
cat("p=",p, ", n=", n)
alpha <- 5

# ベルヌーイ分布に従う乱数生成
get_interval <- function(){
  cnt <- 0
  while (TRUE) {
    cnt <- cnt + 1
    if (rbern(1, p)==1){
      return(cnt)  # 1が出たら何回目かを返す
    }
  }
}

gen_exp_var <- function() {
  data <- data.frame(x=rdply(trial_size, get_interval())/n)
  names(data) <- c("n", "x")
  return(data)
}

result <- rlply(alpha, gen_exp_var())

data <- data.frame(x=result[[1]]$x + result[[2]]$x + result[[3]]$x + result[[4]]$x + result[[5]]$x)

# ガンマ分布の密度関数
dens <- data.frame(y=dgamma(seq(0, 3,.01), shape=alpha, rate=np)*trial_size*width)

ggplot() +
  layer(data=data, mapping=aes(x=x), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  )  + layer(data=dens, mapping=aes(x=seq(0,3,.01), y=y), 
             geom="line", stat="identity", position="identity", colour="red"
  ) + ggtitle("Bernoulli to Gamma")


