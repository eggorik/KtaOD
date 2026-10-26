.libPaths("D:/R/library")
setwd("D:/labs/LR3")
dir.create("plots", showWarnings = FALSE)

library(arules)
library(arulesViz)

# ---------- 1. Загрузка данных ----------
data <- read.delim(file = "lab3_4.csv", sep = ",", header = TRUE, row.names = 1,
                   check.names = FALSE, fileEncoding = "UTF-8")
str(data)
round(colMeans(data), 3)
colSums(data)

# ---------- 2. Подготовка транзакций ----------
dataN <- data[, -11]
isCredit <- data$Кредит
isCredit[isCredit == 1] <- "С кредитом"
isCredit[isCredit == 0] <- "Без кредита"

itemsList <- sapply(1:nrow(dataN), function(i)
  paste(c(isCredit[i], names(dataN)[dataN[i, ] == 1]),
        collapse = ",", sep = "\n"))
head(itemsList)
writeLines(itemsList, "basket.csv", useBytes = TRUE)

trans <- read.transactions("basket.csv", format = "basket", sep = ",", encoding = "UTF-8")
inspect(trans[1:5])
summary(trans)
itemFrequency(trans, type = "absolute")

png("plots/01_item_frequency.png", 1200, 700)
par(mar = c(11, 4, 3, 1))
itemFrequencyPlot(trans, cex.names = 1, col = "steelblue",
                  main = "Частотная диаграмма транзакций")
dev.off()

# ---------- 3. Поиск ассоциативных правил ----------
rules <- apriori(trans, parameter = list(support = 0.1, confidence = 0.5, minlen = 2))
summary(rules)
quality(rules)$correlation <- interestMeasure(rules, "cosine", transactions = trans)
inspect(head(sort(rules, by = "lift"), 20))

rules_df <- DATAFRAME(rules)
rules_df <- rules_df[order(-rules_df$lift), ]
write.csv2(rules_df, "rules_all.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ---------- 4. Правила со следствием «С кредитом» / «Без кредита» ----------
rulesWithCredit <- subset(rules, subset = rhs %in% "С кредитом")
summary(rulesWithCredit)
inspect(sort(rulesWithCredit, by = "confidence"))

rulesNoCredit <- subset(rules, subset = rhs %in% "Без кредита")
inspect(sort(rulesNoCredit, by = "confidence"))

credit_df <- DATAFRAME(sort(rulesWithCredit, by = "confidence"))
write.csv2(credit_df, "rules_credit.csv", row.names = FALSE, fileEncoding = "UTF-8")
nocredit_df <- DATAFRAME(sort(rulesNoCredit, by = "confidence"))
write.csv2(nocredit_df, "rules_nocredit.csv", row.names = FALSE, fileEncoding = "UTF-8")

goods <- subset(rules, subset = !(rhs %in% c("С кредитом", "Без кредита")) &
                  !(lhs %in% c("С кредитом", "Без кредита")))
goods_df <- DATAFRAME(sort(goods, by = "lift"))
write.csv2(goods_df, "rules_goods.csv", row.names = FALSE, fileEncoding = "UTF-8")
inspect(head(sort(goods, by = "lift"), 15))

# ---------- 5. Визуализация правил ----------
png("plots/02_scatter_rules.png", 1000, 700)
plot(rules, measure = c("support", "confidence"), shading = "lift", engine = "base",
     main = "Ассоциативные правила (support = 0.1, confidence = 0.5)")
dev.off()

top_credit <- head(sort(rulesWithCredit, by = "confidence"), 10)

png("plots/03_paracoord_credit.png", 1200, 700)
plot(top_credit, method = "paracoord", control = list(reorder = TRUE))
dev.off()

png("plots/04_graph_credit.png", 1200, 1000)
plot(top_credit, method = "graph", engine = "igraph",
     control = list(nodeCol = grey.colors(10), edgeCol = grey(.7), alpha = 1))
dev.off()

png("plots/05_graph_goods.png", 1200, 1000)
plot(head(sort(goods, by = "lift"), 15), method = "graph", engine = "igraph",
     control = list(nodeCol = grey.colors(10), edgeCol = grey(.7), alpha = 1))
dev.off()

png("plots/06_graph_nocredit.png", 1200, 1000)
plot(sort(rulesNoCredit, by = "confidence"), method = "graph", engine = "igraph",
     control = list(nodeCol = grey.colors(10), edgeCol = grey(.7), alpha = 1))
dev.off()
