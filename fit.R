library(ptproc)

options(digits=14)
x<-read.csv('tweets/hawkes-master/data/192000_292000_times.csv')
trade_times <- x$"X1366463464.594167"

y<-read.csv('tweets/index_now.csv')
tweet_times<-y$"X..A"

fit <- function(data) {
  # initial guess where a is alpha and C is beta
  # λ(t)=μ+∑ti<tαe−β(t−ti)
  pstart <- c(mu = 0.1, C = 0.0001, a = 1)
  
  # create a ptproc object using the conditional intensity function as defined by ptproc
  ppm <- ptproc(pts = data, cond.int = hawkes.cond.int, params = pstart)
  
  # assumption that the intensity has to be positive
  condition(ppm) <- penalty(code = NULL, condition = quote(any(params < 0)))
  
  # fit using standard optim
  f <- ptproc.fit(ppm, optim.control = list(trace = 2), alpha = 1e+5, hessian = TRUE)
  
  return (f)
}

f <- fit(tweet_times)
summary(f)

#f <- fit(trade_times[1:2204])

# evaluation of intensity over the given timeframe. Making a time frame and evaluating intensity over it
x <- seq(min(tweet_times), max(tweet_times), len=100000)
e <- evalCIF(f, xpts = x)

# this has to get integrated over 1 minute intervals to match up to empirical counts
plot(x, e, type = "l", xlab = "Times", ylab = "Conditional intensity")


# write to file for python to read
out <- data.frame(index=x, data=e)
write.csv(out, 'tweets/fitted_intensities_now.csv', row.names=FALSE)

## residuals
res <- residuals(f, type = "approx", K = 350)
write.csv(out, 'tweets/residuals_now.csv', row.names=FALSE)

