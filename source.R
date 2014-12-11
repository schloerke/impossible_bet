require("ggplot2")
source("./R/longest_chain.R")

do_all <- function(n) {
  find_longest_chains(make_random_numbers(n))
}

make_plot <- function(n = 100, r = 100000) {
  system.time({
    dist <- replicate(r, {do_all(n)})
  })
  hist(dist)
  mean(dist <= n / 2)

  p <- qplot(dist, y = ..density.., geom = "histogram", binwidth = 1) +
    geom_density(color = I("darkred"), size = I(1)) +
    geom_vline(xintercept = n / 2) +
    labs(title = paste(
        "Longest chain (n = ", n, ", r = ", r,")\nP(X <= ", n/2
        ,") = ", mean(dist <= n / 2),
        sep = ""
      ),
      x = "longest chain"
    )
  p
}


p100 <- make_plot(100, 100000)
ggsave("n=100.png", p100, width = 6, height = 4)

p1000 <- make_plot(1000, 100000)
ggsave("n=1000.png", p1000, width = 6, height = 4)



library(plyr)
dtk <- get("dist", envir = p1000$plot_env)
count(dtk)
