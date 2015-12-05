
###############################################################
# ポアソン分布
###############################################################

trial_size = 100000
width <- 1

# もともとの問題設定
p = 0.7
sample_size = 30
np <- p*sample_size
cat("p=",p, ", n=", sample_size, "np=", np)

# n→∞、p→0、np=一定 
sample_size = 10000
p <- np/sample_size
cat("p=",p, ", n=", sample_size)


# ベルヌーイ分布に従う乱数生成
gen_rand <- function(){
  res <- c()
  for (i in sequence(trial_size)) {
    res <- rbind(res, sum(rbern(sample_size, p)))
  }
  return(res)
}

system.time(res <- gen_rand())

data <- data.frame(x=res)

# 正規分布の密度関数
dens <- data.frame(y=dpois(seq(30), np))*trial_size

# グラフ描画
ggplot() +
  layer(data=data, mapping=aes(x=x), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  ) + layer(data=dens, mapping=aes(x=seq(30)+.5, y=y), 
            geom="line", stat="identity", position="identity", colour="blue"
  )

########################

trial_size = 100000
width <- 1

# もともとの問題設定
p = 0.7
sample_size = 30
np <- p*sample_size
cat("p=",p, ", n=", sample_size, "np=", np)

# n→∞、p→0、np=一定 
sample_size = 10000
p <- np/sample_size
cat("p=",p, ", n=", sample_size)


# ベルヌーイ分布に従う乱数生成
gen_rand <- function(){
  res <- c()
  for (i in sequence(trial_size)) {
    res <- rbind(res, sum(rbern(sample_size, p)))
  }
  return(res)
}

system.time(res <- gen_rand())

data <- data.frame(x=res)

# ポアソン分布の密度関数
dens <- data.frame(y=dpois(seq(20), np))*trial_size

# グラフ描画
ggplot() +
  layer(data=data, mapping=aes(x=x), geom="bar", stat = "bin",
        binwidth=width, fill="#6666ee", color="gray"
  ) + layer(data=dens, mapping=aes(x=seq(20)+.5, y=y), 
            geom="line", stat="identity", position="identity", colour="blue"
  )
