n <- 10000
set.seed(123)
w <- rbinom(n, size = 1, prob = 0.5)
m <- cumsum(w) / 1:n
e <- 0.5
plot(1:n, m, type = "l", col = "blue",
     xlab = "Anzahl der Würfe",
     ylab = "Empirischer Mittelwert",
     main = "Beispiel: Münzwurf")
abline(h = e, col = "red", lty = 2)
legend("topright", legend = c("Empirischer Mittelwert", "Erwartungswert (0.5)"),
       col = c("blue", "red"), lty = c(1,2), bty = "n")



n <- 10000
schritt <- 100
x <- runif(n)
y <- runif(n)
ist_drinnen <- x^2 + y^2 <= 1
pi <- numeric(n / schritt)
for (i in seq(schritt, n, by = schritt)) {
  pi[i / schritt] <- 4 * mean(ist_drinnen[1:i])
}
x_werte <- seq(schritt, n, by = schritt)
plot(x_werte, pi, type = "l", col = "blue", lwd = 2,
     main = "Monte Carlo-Schätzung von Pi",
     xlab = "Anzahl zufälliger Punkte",
     ylim = c(2.5, 3.7))
abline(h = pi, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Schätzung", "wahrer Wert von Pi"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)



n <- 1000
pfade <- 50
phi <- 0.9
mu <- 0
sigma <- 1
x <- matrix(0, nrow = n, ncol = pfade)
for (j in 1:pfade) {
  x[1, j] <- rnorm(1, mean = mu, sd = sigma)
  for (t in 2:n) {
    x[t, j] <- phi * x[t-1, j] + rnorm(1, mean = 0, sd = sigma)
  }
}
m <- apply(x, 2, function(x) cumsum(x) / seq_along(x))
plot(NULL, xlim = c(1, n), ylim = c(-1, 1), xlab = "Zeitpunkt n", ylab = "Stichprobenmittelwert",
     main = "Stichprobenmittelwerte eines AR(1)-Prozesses")
for (j in 1:pfade) {
  lines(1:n, m[, j], col = rgb(0, 0, 1, alpha = 0.2))
}
abline(h = mu, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Erwartungswert", "Mittelwerte"),
       col = c("red", "blue"), lty = c(2, 1), lwd = c(2, 1))



n <- 500                             
mu <- 1000                 
sigma <- 300              
x <- rnorm(n, mean = mu, sd = sigma)
m  <- cumsum(x) / seq_along(x)
plot(m, type = "l", col = "blue", lwd = 2,
     xlab = "Jahre",
     ylab = "Mittelwert",
     main = "Mittelwert von Schäden")
abline(h = mu, col = "red", lty = 2, lwd = 2)
legend("bottomright", legend = c("Mittelwert", "Erwartungswert"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

