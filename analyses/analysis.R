library(corrplot)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(ggpubr)
library(dplyr)
library(caret)
library(party)
library(sandwich)
library(randomForest)

PGATOUR_data2 %>% filter(!is.na(NUMBER_OF_WINS) & !is.na(POINTS)) %>%
    ggplot(aes(factor(NUMBER_OF_WINS), 
                             POINTS, 
                             fill = factor(NUMBER_OF_WINS))) + 
    geom_boxplot() + 
    theme_gdocs() + 
    ggtitle("Correlation between Number of Wins and points earned for a PGA tour player") +
    theme(legend.position = "none") + 
    xlab("Number of wins") + 
    ylab("Points") + 
    stat_summary(geom = 'text', label = paste(0:3, "win(s)"), fun.y = max, vjust = -1, cex = 5)

plot(ctree(NUMBER_OF_TOP_Tens ~ ROUNDS_PLAYED + AVG_Driving_DISTANCE, data = na.omit(select(PGATOUR_data2, 5:ncol(PGATOUR_data2)))))
top_ten_vector <- c(5, 60, 54, 51, 48, 45, 42, 40, 15, 8, 12, 25, 29, 35, 66, 57)
avg_score_vector <- c(63, 60, 54, 51, 48, 45, 42, 40, 15, 8, 12, 25, 29, 35, 66, 57)
cor_plotA <- cor(na.omit(PGATOUR_data2[top_ten_vector]))
corrplot(cor_plotA, method="number", addrect = 10, tl.cex = 0.8)
cor_plotB <- cor(na.omit(PGATOUR_data2[avg_score_vector]))
corrplot(cor_plotB, method="number", addrect = 10, tl.cex = 0.8)


