a <- 5; b <- 8;
width <- 0.05; p <- 0.5
sample_size <- 1000; trial_size <- 500000

gen_unif_rand <- function() {
  # sample_size桁の2進少数をベルヌーイ分布に従う乱数から生成
  return (sum(rbern(sample_size, p) * (rep(1/2, sample_size) ** seq(sample_size))))
}

gen_rand <- function(){  
  return( rdply(trial_size, gen_unif_rand()) )
}
system.time(res <- gen_rand())
res$V1 <- res$V1 * (b-a) + a

ggplot() +
  layer(data=res, mapping=aes(x=V1), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  ) + ggtitle("Bernoulli to Uniform") + xlim(4,9)

# 自己相関チェック
acf(res$V1)

small_data <- data.frame(x=res$V1[1:300])
ggplot(data=small_data) + geom_point(aes(x=seq(300), y=x))
