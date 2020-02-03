#### Carga datos ####

library(ggplot2)
library(corrplot)
library(ggrepel)

setwd("GitHub/iowa_houses/")
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

library(plyr)
library(dplyr)
df$PoolQC[is.na(df$PoolQC)] <- "None"
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
df$PoolQC <- as.integer(revalue(df$PoolQC, Qualities))
df[df$PoolArea > 0 & df$PoolQC == 0, c('PoolArea', 'PoolQC', 'OverallQual')]
df$PoolQC[2421] <- 2
df$PoolQC[2504] <- 2
df$PoolQC[2600] <- 3

df$MiscFeature[is.na(df$MiscFeature)] <- "None"
df$MiscFeature <- as.factor(df$ MiscFeature)

df$Alley[is.na(df$Alley)] <- "None"
df$Alley <- as.factor(df$Alley)

df$Fence[is.na(df$Fence)] <- "None"
df[!is.na(df$SalePrice), ] %>% 
  group_by(Fence) %>% 
  summarise(median = median(SalePrice), counts = n())
df$Fence <- as.factor(df$Fence)

df$FireplaceQu[is.na(df$FireplaceQu)] <- "None"
df$FireplaceQu <- as.integer(revalue(df$FireplaceQu, Qualities))

ggplot(df, aes(x = as.factor(Neighborhood), y = LotFrontage)) +
  geom_bar(stat = "summary", fun.y = "median", fill = "blue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
for (i in 1:nrow(df)){
  if (is.na(df$LotFrontage[i])) {
    df$LotFrontage[i] <- as.integer(median(df$LotFrontage[df$Neighborhood == df[i, "Neighborhood"]], na.rm = T))
  }
}

df$LotShape <- as.integer(revalue(df$LotShape, c("IR3" = 0, "IR2" = 1, "IR1" = 2, "Reg" = 3)))

df$LotConfig <- as.factor(df$LotConfig)
