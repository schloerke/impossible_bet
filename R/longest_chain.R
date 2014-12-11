
make_random_numbers <- function(n = 100) {
  a <- sample.int(n)
  names(a) <- 1:n
  a
}

find_longest_chains <- function(x) {
  longestChain <- 0

  while(length(x) > 0) {
    currentChain <- c(x[1])
    chainStart <- x[1]
    curVal <- x[1]
    repeat {
      curVal <- x[as.character(curVal)]
      currentChain[length(currentChain) + 1] <- curVal

      if (curVal == chainStart) {
        break
      }
    }

    longestChain <- max(longestChain, length(currentChain) - 1)

    xCurNames <- names(x)
    x <- x[! (xCurNames %in% as.character(currentChain[-1]))]

  }
  longestChain
}









