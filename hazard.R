library(survival)

time <- c(1,3,5,6, 2, 7, 9, 11)
status <- c(1, 0, 1, 1, 1, 0, 1, 1)
df <- data.frame(time, status)

df <- df[order(time), ] # order on time and events
d <- df$status                               
n <- length(d):1

H0 <- cumsum(d / n)
H0
# ------------------------

fit_cox <- coxph(Surv(time, status) ~ 1 , data=df, method = "breslow")
basehaz(fit_cox)

# -----------------------

time <- c(1, 3, 5, 6, 2, 7, 9, 11)
status <- c(1, 0, 1, 1, 1, 0, 1, 1)
sex <- c(1, 1, 1, 1, 0, 0, 0, 0)
age <- c(57, 52, 48, 42, 39, 31, 26, 22)

df <- data.frame(time, status, sex, age)

fit_cox <- coxph(Surv(time, status) ~ age + sex, data=df, method = "breslow")
summary(fit_cox)

df <- data.frame(time, status, sex, age)

fit_cox <- 
  coxph(Surv(time, status) ~ age + sex, x=TRUE, data=df, method = "breslow")

breslow_est <- function(time, status, X, B){
  data <- 
    data.frame(time, status, X)
  data <- 
    data[order(data$time), ]
  t   <- 
    unique(data$time)
  k    <- 
    length(t)
  h    <- 
    rep(0,k)
  
  for(i in 1:k) {
    lp <- (data.matrix(data[,-c(1:2)]) %*% B)[data$time>=t[i]]
    risk <- exp(lp)
    h[i] <- sum(data$status[data$time==t[i]]) / sum(risk)
  }
  
  res <- cumsum(h)
  return(res)
}
H0 <- breslow_est(time=df$time, status=df$status, X=fit_cox$x, B=fit_cox$coef)
H0
