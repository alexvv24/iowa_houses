#### Carga datos ####

library(ggplot2)
library(corrplot)
library(ggrepel)

setwd("~/OneDrive/Estudio/Kaggle/IowaHouses/data")
train <- read.csv("train.csv", stringsAsFactors = F, na.strings = "NA")
test <- read.csv("test.csv", stringsAsFactors = F, na.strings = "NA")

#### EDA ####

test.id <- test$Id
test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA
df <- rbind(train, test)

ggplot(train, aes(x = SalePrice)) + 
  geom_histogram(fill = "blue", binwidth = 1e4) + 
  scale_x_continuous(breaks = seq(0,8e5, by = 1e5), labels = comma)

numeric.vars <- names(which(sapply(train, is.numeric)))
df.numvars <- train[ ,numeric.vars]
cor.numvars <- cor(df.numvars, use = "pairwise.complete.obs")
cor.target <- as.matrix(sort(cor.numvars[ ,"SalePrice"], decreasing = T))
highcor <- names(which(apply(cor.target, 1, function(x) abs(x) > 0.5)))
cor.numvars <- cor.numvars[highcor, highcor]
corrplot.mixed(cor.numvars, tl.col = "black", tl.pos = "lt")

ggplot(train, aes(x = factor(OverallQual), y = SalePrice)) + 
  geom_boxplot(col = "blue") +
  labs(x = "Overall quality") +
  scale_y_continuous(breaks = seq(0, 8e5, by = 1e5), labels = comma)

ggplot(train, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(col = "blue") + 
  geom_smooth(method = "lm", se = T, color = "black", aes(group = 1)) +
  scale_y_continuous(breaks= seq(0, 8e5, by= 1e5), labels = comma) +
  geom_text_repel(aes(label = ifelse(train$GrLivArea[!is.na(train$SalePrice)]>4500, rownames(train), '')))
  


#### Missing values ####

na.cols <- which(colSums(is.na(df)) > 0)
sort(colSums(sapply(df[na.cols], is.na)), decreasing = T)


#### Na imputing #### 

df[is.na(df$PoolQC),]
