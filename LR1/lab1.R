.libPaths("D:/R/library")
setwd("D:/labs/LR1")
dir.create("plots", showWarnings = FALSE)

library(psych)
library(rcompanion)
library(nortest)
library(ppcor)
library(corrplot)
library(ggplot2)
library(GGally)

# ---------- 1. Загрузка данных ----------
data <- read.table("var_13.csv", header = TRUE, sep = ";",
                   fileEncoding = "UTF-8")

# ---------- 2. Просмотр, доступ к данным, подвыборки ----------
head(data)
data$Age
data$Age[3]
data[1, 5]
data[1:3, ]
data[c(1, 3, 5)][1:5, ]
subset(data, Age > 30)[1:5, ]
subset(data, Average.income > 50, select = c("Age", "Average.income"))[1:5, ]

# ---------- 3. Переименование признаков ----------
names(data) <- c("N", "Group", "Gender", "Age", "Income", "Experience",
                 "Spec", "Pages", "Score", "Level")

# ---------- 4. Структура данных, типы признаков ----------
str(data)
data$N      <- NULL
data$Group  <- as.factor(data$Group)
data$Gender <- factor(data$Gender, levels = c(1, 2))
data$Spec   <- factor(data$Spec, levels = 1:4, ordered = TRUE)
data$Level  <- factor(data$Level, levels = c("Low", "Medium", "High"), ordered = TRUE)
str(data)
summary(data)

data1 <- subset(data, Group == "1")
data2 <- subset(data, Group == "2")
quant <- c("Age", "Income", "Experience", "Pages", "Score")

# ---------- 5. Описательные статистики ----------
mode_val <- function(x) {
  t <- table(x)
  as.numeric(names(t)[which.max(t)])
}
mystats <- function(x) {
  m <- mean(x); n <- length(x); s <- sd(x)
  q <- quantile(x, c(0.25, 0.5, 0.75))
  c(n = n, min = min(x), max = max(x), mean = m, sd = s, var = var(x),
    Q1 = unname(q[1]), median = unname(q[2]), Q3 = unname(q[3]),
    mode = mode_val(x),
    skew = sum((x - m)^3 / s^3) / n,
    kurt = sum((x - m)^4 / s^4) / n - 3)
}
stats_all <- round(t(sapply(data[quant],  mystats)), 3)
stats_g1  <- round(t(sapply(data1[quant], mystats)), 3)
stats_g2  <- round(t(sapply(data2[quant], mystats)), 3)
cat("\n=== Вся выборка ===\n");  print(stats_all)
cat("\n=== Группа 1 ===\n");     print(stats_g1)
cat("\n=== Группа 2 ===\n");     print(stats_g2)
print(describe(data[quant]))

# ---------- 6. Графический анализ ----------
# 6.1 Диаграмма рассеяния
png("plots/01_scatter.png", 900, 700)
plot(data$Experience, data$Pages, pch = 19,
     col = ifelse(data$Group == "1", "red", "blue"),
     main = "Диаграмма рассеяния", sub = "Стаж использования услуги и число страниц",
     xlab = "Стаж (время использования услуги)", ylab = "Среднее число страниц в месяц")
abline(lm(Pages ~ Experience, data = data))
legend("topleft", c("Группа 1", "Группа 2"), col = c("red", "blue"), pch = 19)
dev.off()

# 6.2 Радиальная диаграмма по качественному признаку
png("plots/02_pie_spec.png", 800, 700)
x <- table(data$Spec)
pp <- round(100 * x / sum(x), 1)
pie(x, labels = paste0(pp, "%"), radius = 1, clockwise = TRUE,
    main = "Профессиональная специализация",
    col = c("red", "blue", "green", "orange"))
legend("topright", c("1 – не использую", "2 – крайне редко", "3 – ежедневно", "4 – постоянно"),
       cex = 0.8, fill = c("red", "blue", "green", "orange"))
dev.off()

# 6.3 Категориальная радиальная диаграмма (Level по полу и группе)
png("plots/03_pie_level_gender_group.png", 1000, 900)
par(mfrow = c(2, 2))
cols <- c("red", "orange", "green")
for (g in c("1", "2")) for (s in c("1", "2")) {
  x <- table(data$Level[data$Gender == s & data$Group == g])
  pie(x, labels = paste0(names(x), " (", x, ")"), col = cols, clockwise = TRUE,
      main = paste(ifelse(s == "1", "Мужчины", "Женщины"), "группы", g))
}
dev.off()

# 6.4 Категориальная столбиковая диаграмма (Income по полу и группе)
png("plots/04_bar_income_gender_group.png", 1100, 900)
par(mfrow = c(2, 2))
bcols <- c("red", "green", "blue", "orange"); i <- 1
for (s in c("1", "2")) for (g in c("1", "2")) {
  barplot(table(round(data$Income[data$Gender == s & data$Group == g])),
          col = bcols[i], xlab = "Средний доход", ylab = "Количество наблюдений",
          main = paste(ifelse(s == "1", "Мужчины", "Женщины"), "группы", g))
  i <- i + 1
}
dev.off()

# 6.5 Диаграммы размаха
png("plots/05_boxplot.png", 1100, 600)
par(mfrow = c(1, 2))
boxplot(Pages ~ Group, data = data, col = "coral",
        xlab = "Группа", ylab = "Среднее число страниц в месяц")
boxplot(Pages ~ Gender * Group, data = data, col = c("lightblue", "pink"),
        xlab = "Пол.Группа", ylab = "Среднее число страниц в месяц")
dev.off()

# 6.6 Гистограммы всех количественных признаков
png("plots/06_hist.png", 1200, 800)
par(mfrow = c(2, 3))
lab <- c(Age = "Возраст", Income = "Средний доход", Experience = "Стаж",
         Pages = "Число страниц", Score = "Активность (баллы)")
for (v in quant) {
  hist(data[[v]], freq = FALSE, breaks = 10, col = "steelblue",
       main = lab[v], xlab = lab[v], ylab = "Плотность")
  lines(density(data[[v]]), lwd = 2)
}
dev.off()

# 6.7 Матричный график
png("plots/07_pairs.png", 1000, 1000)
pairs(data[quant], main = "Матричный график",
      col = ifelse(data$Group == "1", "red", "blue"))
dev.off()

# ---------- 7. Корреляционный анализ ----------
# 7.1 Проверка нормальности (признак Score) по группам и в целом
for (nm in c("data", "data1", "data2")) {
  x <- get(nm)$Score
  cat("\n=== Score,", nm, "===\n")
  print(pearson.test(x)); print(shapiro.test(x)); print(cvm.test(x))
}
png("plots/08_normal_hist.png", 1200, 500)
par(mfrow = c(1, 3))
plotNormalHistogram(data$Score, main = "Вся выборка", xlab = "Активность (баллы)",
                    ylab = "Количество наблюдений", length = 1000)
plotNormalHistogram(data1$Score, main = "Группа 1", xlab = "Активность (баллы)",
                    ylab = "Количество наблюдений", length = 1000)
plotNormalHistogram(data2$Score, main = "Группа 2", xlab = "Активность (баллы)",
                    ylab = "Количество наблюдений", length = 1000)
dev.off()

# 7.2 Корреляции Пирсона, Спирмена, Кендалла по группам
for (nm in c("data1", "data2")) {
  M <- get(nm)[quant]
  cat("\n=== Корреляции,", nm, "===\n")
  cat("Pearson\n");  print(round(cor(M), 3))
  cat("Spearman\n"); print(round(cor(M, method = "spearman"), 3))
  cat("Kendall\n");  print(round(cor(M, method = "kendall"), 3))
}

# 7.3 Значимость коэффициента Пирсона (Experience – Pages)
print(cor.test(data1$Experience, data1$Pages))
print(cor.test(data2$Experience, data2$Pages))
print(cor.test(data1$Income, data1$Score))
print(cor.test(data2$Income, data2$Score))

# 7.4 Частные коэффициенты корреляции Пирсона
for (nm in c("data1", "data2")) {
  C <- pcor(get(nm)[quant])
  cat("\n=== Частные корреляции,", nm, "===\n")
  print(round(C$estimate, 3)); print(signif(C$p.value, 3))
}

# 7.5 Тепловая карта корреляций
png("plots/09_corrplot.png", 1200, 600)
par(mfrow = c(1, 2))
for (nm in c("data1", "data2")) {
  corrplot(cor(get(nm)[quant]), method = "color", type = "upper", order = "hclust",
           addCoef.col = "black", tl.col = "black", tl.srt = 45, diag = FALSE,
           title = paste("Пирсон,", ifelse(nm == "data1", "группа 1", "группа 2")),
           mar = c(0, 0, 2, 0))
}
dev.off()

# ---------- 8. ANOVA и Краскел-Уоллис (фактор Spec) ----------
for (v in quant) {
  cat("\n=== ", v, " ~ Spec ===\n")
  print(summary(aov(data[[v]] ~ data$Spec)))
  print(kruskal.test(data[[v]] ~ data$Spec))
}
for (v in quant) {
  cat("\n=== ", v, " ~ Group ===\n")
  print(summary(aov(data[[v]] ~ data$Group)))
  print(kruskal.test(data[[v]] ~ data$Group))
}

# ---------- 9. Таблицы сопряженности ----------
tb <- table(data$Level, data$Group)
print(addmargins(tb)); print(round(prop.table(tb), 3))
factor_names <- c("Group", "Gender", "Spec", "Level")
chi_p <- matrix(NA, 4, 4, dimnames = list(factor_names, factor_names))
fis_p <- chi_p
for (a in factor_names) for (b in factor_names) {
  t <- table(data[[a]], data[[b]])
  chi_p[a, b] <- suppressWarnings(chisq.test(t)$p.value)
  fis_p[a, b] <- fisher.test(t, simulate.p.value = TRUE, B = 1e5)$p.value
}
cat("\n=== p-value chi-square ===\n"); print(signif(chi_p, 3))
cat("\n=== p-value Fisher ===\n");     print(signif(fis_p, 3))
print(chisq.test(table(data$Level, data$Group)))
print(fisher.test(table(data$Level, data$Group)))
print(chisq.test(table(data$Spec, data$Group)))
print(fisher.test(table(data$Spec, data$Group)))

# ---------- 10. ggpairs ----------
set.seed(1)
g1 <- ggpairs(data, columns = c("Group", "Gender", "Age", "Income", "Experience"),
              aes(color = Group, alpha = 0.5))
ggsave("plots/10_ggpairs_1.png", g1, width = 12, height = 10, dpi = 100)
g2 <- ggpairs(data, columns = c("Group", "Pages", "Score", "Spec", "Level"),
              aes(color = Group, alpha = 0.5))
ggsave("plots/11_ggpairs_2.png", g2, width = 12, height = 10, dpi = 100)
