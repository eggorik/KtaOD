.libPaths("D:/R/library")
setwd("D:/labs/LR2")
dir.create("plots", showWarnings = FALSE)

library(tseries)
library(forecast)
library(lmtest)

# ---------- 2.1. Загрузка данных ----------
data <- read.table(list.files(pattern = "\\.csv$")[1], header = TRUE, sep = ";",
                   fileEncoding = "windows-1251")
y <- data[, 16]
tsData <- ts(y, frequency = 12, start = c(2010, 1))
tsData
n <- length(y)
t <- 1:n
d1 <- data.frame(t = t, y = y)

# ---------- 2.2. График и декомпозиция ----------
png("plots/01_series.png", 1200, 500)
plot.ts(tsData, main = "Объём производства, млн руб.", xlab = "Год", ylab = "млн руб.", lwd = 2)
dev.off()

tsDataComponents <- decompose(tsData, type = "additive")
round(tsDataComponents$figure, 2)
png("plots/02_decompose.png", 1000, 900)
plot(tsDataComponents)
dev.off()

print(round(aggregate(tsData, FUN = mean), 1))
print(round(aggregate(tsData, FUN = function(v) max(v) - min(v)), 1))

stats <- function(fit, y) {
  e <- y - fit
  c(min = min(e), max = max(e), ME = mean(e), SD = sd(e), MAE = mean(abs(e)),
    MPE = mean(100 * e / y), MAPE = mean(abs(100 * e / y)),
    RMSE = sqrt(mean(e^2)), R2 = 1 - sum(e^2) / sum((y - mean(y))^2))
}

diag_plots <- function(r, file, title) {
  png(file, 1200, 900)
  par(mfrow = c(2, 2))
  plot(t, r, type = "l", main = paste(title, ": остатки"), xlab = "t", ylab = "e(t)")
  abline(h = 0, lty = 2)
  spec.pgram(r, detrend = FALSE, log = "no", fast = FALSE, pad = FALSE, taper = 0,
             main = paste(title, ": периодограмма остатков"))
  Acf(r, lag.max = 36, main = paste(title, ": ACF остатков"))
  Pacf(r, lag.max = 36, main = paste(title, ": PACF остатков"))
  dev.off()
}

# ---------- 2.3.1. Идентификация тренда (Модель 1) ----------
dy <- diff(y)
round(dy[1:60], 1)

fit_log <- nls(y ~ k / (1 + a * exp(b * t)), data = d1,
               start = list(k = 180, a = 20, b = -0.1))
fit_gom <- nls(y ~ k * a^(b^t), data = d1,
               start = list(k = 180, a = 0.1, b = 0.95))
fit_p3 <- lm(y ~ t + I(t^2) + I(t^3), data = d1)
cmp <- rbind(logistic = stats(fitted(fit_log), y),
             gompertz = stats(fitted(fit_gom), y),
             poly3    = stats(fitted(fit_p3), y))
round(cmp, 3)
AIC(fit_log, fit_gom, fit_p3)

model1 <- fit_log
summary(model1)
m1 <- fitted(model1)
r1 <- y - m1

png("plots/03_model1.png", 1200, 500)
plot(t, y, type = "l", lwd = 2, main = "Исходный ряд и Модель 1 (логистический тренд)",
     xlab = "t (месяц с янв. 2010)", ylab = "млн руб.")
lines(t, m1, col = "red", lwd = 2)
legend("bottomright", c("Исходный ряд", "Модель 1"), col = c("black", "red"), lwd = 2)
dev.off()
diag_plots(r1, "plots/04_model1_resid.png", "Модель 1")

sp1 <- spec.pgram(r1, detrend = FALSE, log = "no", fast = FALSE, pad = FALSE, taper = 0, plot = FALSE)
top1 <- order(sp1$spec, decreasing = TRUE)[1:5]
data.frame(freq = sp1$freq[top1], period = 1 / sp1$freq[top1], spec = sp1$spec[top1])

# ---------- 2.3.2. Идентификация сезонной составляющей (Модель 2) ----------
w <- 2 * pi / 12
fit_s1 <- nls(y ~ k / (1 + a * exp(b * t)) + c1 * cos(w * t) + s1 * sin(w * t),
              data = d1, start = c(coef(model1), c1 = 0, s1 = 0))
summary(fit_s1)
rs1 <- y - fitted(fit_s1)
sps <- spec.pgram(rs1, detrend = FALSE, log = "no", fast = FALSE, pad = FALSE, taper = 0, plot = FALSE)
tops <- order(sps$spec, decreasing = TRUE)[1:5]
data.frame(freq = sps$freq[tops], period = 1 / sps$freq[tops], spec = sps$spec[tops])
fit_s12_6 <- nls(y ~ k / (1 + a * exp(b * t)) + c1 * cos(w * t) + s1 * sin(w * t) +
                   c2 * cos(2 * w * t) + s2 * sin(2 * w * t),
                 data = d1, start = c(coef(fit_s1), c2 = 0, s2 = 0))
anova(fit_s1, fit_s12_6)
fit_s12_4 <- nls(y ~ k / (1 + a * exp(b * t)) + c1 * cos(w * t) + s1 * sin(w * t) +
                   c3 * cos(3 * w * t) + s3 * sin(3 * w * t),
                 data = d1, start = c(coef(fit_s1), c3 = 0, s3 = 0))
anova(fit_s1, fit_s12_4)
model2 <- fit_s12_4
summary(model2)
m2 <- fitted(model2)
r2 <- y - m2

png("plots/05_model2.png", 1200, 500)
plot(t, y, type = "l", lwd = 2, main = "Исходный ряд и Модель 2 (тренд + сезонность)",
     xlab = "t (месяц с янв. 2010)", ylab = "млн руб.")
lines(t, m2, col = "blue", lwd = 2)
legend("bottomright", c("Исходный ряд", "Модель 2"), col = c("black", "blue"), lwd = 2)
dev.off()
diag_plots(r2, "plots/06_model2_resid.png", "Модель 2")
Box.test(r2, lag = 24, type = "Ljung-Box")
shapiro.test(r2)

sp2 <- spec.pgram(r2, detrend = FALSE, log = "no", fast = FALSE, pad = FALSE, taper = 0, plot = FALSE)
top2 <- order(sp2$spec, decreasing = TRUE)[1:5]
data.frame(freq = sp2$freq[top2], period = 1 / sp2$freq[top2], spec = sp2$spec[top2])

# ---------- 2.3.3. Идентификация авторегрессионной составляющей (Модель 3) ----------
ar_fit <- Arima(r2, order = c(1, 0, 0), include.mean = FALSE)
ar_fit
coeftest(ar_fit)
ar2_fit <- Arima(r2, order = c(2, 0, 0), include.mean = FALSE)
coeftest(ar2_fit)
c(AR1 = AIC(ar_fit), AR2 = AIC(ar2_fit))
auto.arima(r2, d = 0, stationary = TRUE, seasonal = FALSE)

m3 <- m2 + fitted(ar_fit)
r3 <- y - m3

png("plots/07_model3.png", 1200, 500)
plot(t, y, type = "l", lwd = 2, main = "Исходный ряд и Модель 3 (тренд + сезонность + AR(1))",
     xlab = "t (месяц с янв. 2010)", ylab = "млн руб.")
lines(t, m3, col = "darkgreen", lwd = 2)
legend("bottomright", c("Исходный ряд", "Модель 3"), col = c("black", "darkgreen"), lwd = 2)
dev.off()
diag_plots(as.numeric(r3), "plots/08_model3_resid.png", "Модель 3")
Box.test(r3, lag = 24, type = "Ljung-Box", fitdf = 1)
shapiro.test(as.numeric(r3))

# ---------- 2.4. Сравнение моделей ----------
tab2 <- rbind("Модель 1" = stats(m1, y), "Модель 2" = stats(m2, y), "Модель 3" = stats(m3, y))
round(t(tab2), 4)
accuracy(m1, y)
accuracy(m2, y)
accuracy(as.numeric(m3), y)

# ---------- Прогноз на январь-март 2026 ----------
th <- (n + 1):(n + 3)
p2 <- predict(model2, newdata = data.frame(t = th))
ar_fc <- forecast(ar_fit, h = 3)
p3 <- p2 + as.numeric(ar_fc$mean)
se2 <- summary(model2)$sigma
lo <- p2 - qnorm(0.975) * se2
hi <- p2 + qnorm(0.975) * se2
fc <- data.frame(month = c("Январь 2026", "Февраль 2026", "Март 2026"), t = th,
                 model2 = round(p2, 2), model3 = round(p3, 2),
                 lo95 = round(lo, 2), hi95 = round(hi, 2))
fc
round(y[(n - 11):n], 2)

png("plots/09_forecast.png", 1200, 500)
idx <- (n - 47):n
plot(c(idx, th), c(y[idx], rep(NA, 3)), type = "l", lwd = 2,
     ylim = range(c(y[idx], lo, hi)), main = "Прогноз объёма производства на январь-март 2026 (Модель 2)",
     xlab = "t (месяц с янв. 2010)", ylab = "млн руб.")
lines(idx, m2[idx], col = "darkgreen", lty = 2)
polygon(c(th, rev(th)), c(lo, rev(hi)), col = adjustcolor("darkgreen", 0.2), border = NA)
lines(c(n, th), c(y[n], p2), col = "darkgreen", lwd = 2)
points(th, p2, col = "darkgreen", pch = 19)
legend("bottomleft", c("Исходный ряд", "Модель 2", "Прогноз", "95% интервал"),
       col = c("black", "darkgreen", "darkgreen", adjustcolor("darkgreen", 0.3)),
       lty = c(1, 2, 1, 1), lwd = c(2, 1, 2, 8))
dev.off()
