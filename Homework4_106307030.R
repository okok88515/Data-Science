#######################
# dimension reduction #
setwd('C:\\Users\\Weber\\Desktop\\大學\\政大110下\\R\\HW4')
finan <- read.csv("financialdata.csv")
data = finan[,-1]

str(data)
data$op_profit_growth_rate <- as.numeric(gsub(",","",data$op_profit_growth_rate))
data$current_ratio <- as.numeric(gsub(",","",data$current_ratio))
data$quick_rartio <- as.numeric(gsub(",","",data$quick_rartio))
str(data)                                # Print classes of all colums
## understand ur data
M = cor(data)

## create heatmap for correlation 
library(reshape2)
melted_cormat <- melt(M)
head(melted_cormat)

library(ggplot2)

ggplot(data = melted_cormat,
       aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

heatmap(M)
library(corrplot)
corrplot(M, method="circle")

## SPCA ##-------------------------------------------------------------------
install.packages("nsprcomp")
library(nsprcomp)
spca <- nscumcomp(data, k=64, nneg=T, scale=T)
summary(spca)
screeplot(spca)
biplot(spca,scale = T)
biplot(spca,scale = T,choices=2:3)

ggplot(melt(spca$rotation), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "white", high = "steelblue") +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

ggplot(melt(spca$rotation[,1:3]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "white", high = "steelblue") +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())


scores <- as.data.frame(spca$x[,1:3])

## Compute variance explained
pve=(spca$sdev)^2 / (sum(spca$sdev^2))
pve
pve[1]
# 安裝 dplyr 套件 ---------
install.packages("dplyr")
# 安裝 dplyr 套件 ---------
library(dplyr)
str(scores)

# compute invest score by weighting PC1~PC2
scores <- scores %>% mutate(score = pve[1]*PC1 + pve[2]*PC2 + pve[3]*PC3)
invest <- scores[order(scores$score, decreasing = TRUE),]

#Order scores by the invest score
print(scores[order(scores$score, decreasing = TRUE),][4])

#plot_2 <-ggplot(scores, aes(x=PC1,y=PC2,color=score )) + geom_point(size =2) + labs(title="Plotting Customer Data against PC1 and PC2")

#plot_2
