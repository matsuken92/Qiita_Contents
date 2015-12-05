mu <- 5
n  <- 5
sigma <- 3

trial_size <- 10000
width <- 0.2

gen_t_val <- function(){
  sample <- rnorm(n, mean=mu, sd=sigma)
  t <- (mean(sample)-mu)/(sd(sample)/sqrt(n))
}

result <- rdply(trial_size, gen_t_val())

# ベルヌーイ分布の確率分布
dens <- data.frame(y=dt(seq(-15,15, width), n))*trial_size*width

# グラフ描画
ggplot() +
  layer(data=result, mapping=aes(x=V1), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  ) + layer(data=dens, mapping=aes(x=seq(-15,15, width), y=y), 
            geom="line", stat="identity", position="identity", colour="red"
  ) + ggtitle("Normal to t.") + xlim(-8, 8)
