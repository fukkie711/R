# Rによるブラック・ショールズ・モデルの計算
# calclaotion of Black-Scholes equation using R
bsmodel <- function(s, k, vol, t, r, q) {
  if (t <= 0.0) {
    data <-c(0.0, (s-k))
    return(max(data))
  } else {
    return (exp(-q*t)*s*pnorm(d1(s, k, vol, t, r, q))-exp(-r*t)*k*pnorm(d2(s, k, vol, t, r, q)))
  }
}
d1 <- function(s, k, vol, t, r, q) {
  return ((log(s/k)+(r-q+vol^2/2)*t)/vol*sqrt(t))
}
d2 <- function(s, k, vol, t, r, q) {
  return (d1(s, k, vol, t, r,q)-vol*sqrt(t))
}

