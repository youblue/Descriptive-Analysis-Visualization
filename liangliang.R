rm(list = ls())

#### Change this work.path !!!
work.path <- "P:/liangliang"
setwd(work.path)

data <- read.csv("zws.csv")

if (!require("ggplot2")) {
  install.packages("ggplot2")
}
library(ggplot2)

grps <- c("NORMAL", "HLHS", "D-TGA", "TOF")
#colors <- c("forestgreen", "darkred", "royalblue", "mediumorchid")
data$Group <- factor(data$Group, levels = grps)
colors <- c(rep("blue", sum(data$Group == "NORMAL")),
            rep("orange", sum(data$Group == "HLHS")),
            rep("cyan", sum(data$Group == "D-TGA")),
            rep("mediumorchid", sum(data$Group == "TOF")) )
colors <- factor(colors, levels = c("blue", "orange", "cyan", "mediumorchid") )


min.mean.sd.max <- function(x) {
  r <- c(mean(x) - 2*sd(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), mean(x) + 2*sd(x) )
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

lower <- function(x) {
  l <- mean(x) - 2*sd(x)
  l
}

upper <- function(x) {
  u <- mean(x) + 2*sd(x)
  u
}

##### 1 #####
y1 <- data[which(data$Group == "NORMAL"), "CSP.L1.ZS"]
y2 <- data[which(data$Group == "HLHS"), "CSP.L1.ZS"]
y3 <- data[which(data$Group == "D-TGA"), "CSP.L1.ZS"]
y4 <- data[which(data$Group == "TOF"), "CSP.L1.ZS"]
dfl1 <- data.frame(a = c(0.8, 1, 1.2), b = c(lower(y1), lower(y1), lower(y1) ) )
dfu1 <- data.frame(a = c(0.8, 1, 1.2), b = c(upper(y1), upper(y1), upper(y1) ) )
dfl2 <- data.frame(a = c(1.8, 2, 2.2), b = c(lower(y2), lower(y2), lower(y2) ) ) 
dfu2 <- data.frame(a = c(1.8, 2, 2.2), b = c(upper(y2), upper(y2), upper(y2) ) ) 
dfl3 <- data.frame(a = c(2.8, 3, 3.2), b = c(lower(y3), lower(y3), lower(y3) ) )
dfu3 <- data.frame(a = c(2.8, 3, 3.2), b = c(upper(y3), upper(y3), upper(y3) ) )
dfl4 <- data.frame(a = c(3.8, 4, 4.2), b = c(lower(y4), lower(y4), lower(y4) ) )
dfu4 <- data.frame(a = c(3.8, 4, 4.2), b = c(upper(y4), upper(y4), upper(y4) ) )

df2 <- data.frame(a = c(1, 1, 2, 2), b = c(3.1, 3.2, 3.2, 3.1))
df3 <- data.frame(a = c(1, 1:3,3), b = c(3.9, 4, 4, 4, 3.9))

#pdf("CSP-L.pdf", width = 7, height = 5)
png("CSP-L_zscore.png", width = 700, height = 500)
print(
  ggplot(data = data, aes(x = Group, y = CSP.L1.ZS) ) +
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", aes(fill = Group), outlier.colour = NA) + 
    scale_fill_manual(name = "Group", values = c("blue", "orange", "cyan", "mediumorchid") ) +
    theme_bw() +
    #labs(title = "CSP-L") +
    xlab("Group") +
    ylab("CSP-L Z-score") +
    
    geom_line(data = dfl1, aes(x = a, y = b)) +
    geom_line(data = dfu1, aes(x = a, y = b)) +
    geom_line(data = dfl2, aes(x = a, y = b)) +
    geom_line(data = dfu2, aes(x = a, y = b)) +
    geom_line(data = dfl3, aes(x = a, y = b)) +
    geom_line(data = dfu3, aes(x = a, y = b)) +
    geom_line(data = dfl4, aes(x = a, y = b)) +
    geom_line(data = dfu4, aes(x = a, y = b)) +
    geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 3.4, label = "p < 0.001", size = 5) +
    geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 4.2, label = "p < 0.001", size = 5)
  
)
dev.off()


##### 2 #####
y1 <- data[which(data$Group == "NORMAL"), "CSP.W1.ZS"]
y2 <- data[which(data$Group == "HLHS"), "CSP.W1.ZS"]
y3 <- data[which(data$Group == "D-TGA"), "CSP.W1.ZS"]
y4 <- data[which(data$Group == "TOF"), "CSP.W1.ZS"]
dfl1 <- data.frame(a = c(0.8, 1, 1.2), b = c(lower(y1), lower(y1), lower(y1) ) )
dfu1 <- data.frame(a = c(0.8, 1, 1.2), b = c(upper(y1), upper(y1), upper(y1) ) )
dfl2 <- data.frame(a = c(1.8, 2, 2.2), b = c(lower(y2), lower(y2), lower(y2) ) ) 
dfu2 <- data.frame(a = c(1.8, 2, 2.2), b = c(upper(y2), upper(y2), upper(y2) ) ) 
dfl3 <- data.frame(a = c(2.8, 3, 3.2), b = c(lower(y3), lower(y3), lower(y3) ) )
dfu3 <- data.frame(a = c(2.8, 3, 3.2), b = c(upper(y3), upper(y3), upper(y3) ) )
dfl4 <- data.frame(a = c(3.8, 4, 4.2), b = c(lower(y4), lower(y4), lower(y4) ) )
dfu4 <- data.frame(a = c(3.8, 4, 4.2), b = c(upper(y4), upper(y4), upper(y4) ) )

df2 <- data.frame(a = c(1, 1, 2, 2), b = c(3, 3.1, 3.1, 3))
df4 <- data.frame(a = c(1, 1:4,4), b = c(3.9, 4, 4, 4, 4, 3.9))

#pdf("CSP-W.pdf", width = 7, height = 5)
png("CSP-W_zscore.png", width = 700, height = 500)
print(
  ggplot(data = data, aes(x = Group, y = CSP.W1.ZS) ) +
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", aes(fill = Group), outlier.colour = NA) + 
    scale_fill_manual(name = "Group", values = c("blue", "orange", "cyan", "mediumorchid") ) +
    theme_bw() +
    #labs(title = "CSP-W") +
    xlab("Group") +
    ylab("CSP-W Z-score") +
    
    geom_line(data = dfl1, aes(x = a, y = b)) +
    geom_line(data = dfu1, aes(x = a, y = b)) +
    geom_line(data = dfl2, aes(x = a, y = b)) +
    geom_line(data = dfu2, aes(x = a, y = b)) +
    geom_line(data = dfl3, aes(x = a, y = b)) +
    geom_line(data = dfu3, aes(x = a, y = b)) +
    geom_line(data = dfl4, aes(x = a, y = b)) +
    geom_line(data = dfu4, aes(x = a, y = b)) +
    geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 3.3, label = "p < 0.05", size = 5) +
    geom_line(data = df4, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 4.2, label = "p < 0.001", size = 5)
  
)
dev.off()



##### 3 #####
y1 <- data[which(data$Group == "NORMAL"), "FLL1.ZS"]
y2 <- data[which(data$Group == "HLHS"), "FLL1.ZS"]
y3 <- data[which(data$Group == "D-TGA"), "FLL1.ZS"]
y4 <- data[which(data$Group == "TOF"), "FLL1.ZS"]
dfl1 <- data.frame(a = c(0.8, 1, 1.2), b = c(lower(y1), lower(y1), lower(y1) ) )
dfu1 <- data.frame(a = c(0.8, 1, 1.2), b = c(upper(y1), upper(y1), upper(y1) ) )
dfl2 <- data.frame(a = c(1.8, 2, 2.2), b = c(lower(y2), lower(y2), lower(y2) ) ) 
dfu2 <- data.frame(a = c(1.8, 2, 2.2), b = c(upper(y2), upper(y2), upper(y2) ) ) 
dfl3 <- data.frame(a = c(2.8, 3, 3.2), b = c(lower(y3), lower(y3), lower(y3) ) )
dfu3 <- data.frame(a = c(2.8, 3, 3.2), b = c(upper(y3), upper(y3), upper(y3) ) )
dfl4 <- data.frame(a = c(3.8, 4, 4.2), b = c(lower(y4), lower(y4), lower(y4) ) )
dfu4 <- data.frame(a = c(3.8, 4, 4.2), b = c(upper(y4), upper(y4), upper(y4) ) )

df4 <- data.frame(a = c(1, 1:4,4), b = c(2.4, 2.5, 2.5, 2.5, 2.5, 2.4))

#pdf("FLL.pdf", width = 7, height = 5)
png("FLL_zscore.png", width = 700, height = 500)
print(
  ggplot(data = data, aes(x = Group, y = FLL1.ZS) ) +
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", aes(fill = Group), outlier.colour = NA) + 
    scale_fill_manual(name = "Group", values = c("blue", "orange", "cyan", "mediumorchid") ) +
    theme_bw() +
    #labs(title = "FLL1") +
    xlab("Group") +
    ylab("FLL Z-score") +
    
    geom_line(data = dfl1, aes(x = a, y = b)) +
    geom_line(data = dfu1, aes(x = a, y = b)) +
    geom_line(data = dfl2, aes(x = a, y = b)) +
    geom_line(data = dfu2, aes(x = a, y = b)) +
    geom_line(data = dfl3, aes(x = a, y = b)) +
    geom_line(data = dfu3, aes(x = a, y = b)) +
    geom_line(data = dfl4, aes(x = a, y = b)) +
    geom_line(data = dfu4, aes(x = a, y = b)) +
    geom_line(data = df4, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 2.7, label = "p < 0.001", size = 5)
  
)
dev.off()



##### 4 #####
y1 <- data[which(data$Group == "NORMAL"), "FLL.BPD.ZS"]
y2 <- data[which(data$Group == "HLHS"), "FLL.BPD.ZS"]
y3 <- data[which(data$Group == "D-TGA"), "FLL.BPD.ZS"]
y4 <- data[which(data$Group == "TOF"), "FLL.BPD.ZS"]
dfl1 <- data.frame(a = c(0.8, 1, 1.2), b = c(lower(y1), lower(y1), lower(y1) ) )
dfu1 <- data.frame(a = c(0.8, 1, 1.2), b = c(upper(y1), upper(y1), upper(y1) ) )
dfl2 <- data.frame(a = c(1.8, 2, 2.2), b = c(lower(y2), lower(y2), lower(y2) ) ) 
dfu2 <- data.frame(a = c(1.8, 2, 2.2), b = c(upper(y2), upper(y2), upper(y2) ) ) 
dfl3 <- data.frame(a = c(2.8, 3, 3.2), b = c(lower(y3), lower(y3), lower(y3) ) )
dfu3 <- data.frame(a = c(2.8, 3, 3.2), b = c(upper(y3), upper(y3), upper(y3) ) )
dfl4 <- data.frame(a = c(3.8, 4, 4.2), b = c(lower(y4), lower(y4), lower(y4) ) )
dfu4 <- data.frame(a = c(3.8, 4, 4.2), b = c(upper(y4), upper(y4), upper(y4) ) )

df4 <- data.frame(a = c(1, 1:4,4), b = c(2.4, 2.5, 2.5, 2.5, 2.5, 2.4))

#pdf("FLL1_vs_BPD.pdf", width = 7, height = 5)
png("FLL1_vs_BPD_zscore.png", width = 700, height = 500)
print(
  ggplot(data = data, aes(x = Group, y = FLL.BPD.ZS) ) +
    stat_summary(fun.data = min.mean.sd.max, geom = "boxplot", aes(fill = Group), outlier.colour = NA) + 
    scale_fill_manual(name = "Group", values = c("blue", "orange", "cyan", "mediumorchid") ) +
    theme_bw() +
    #labs(title = "FLL/BPD") +
    xlab("Group") +
    ylab("FLL/BPD Z-score") +
    
    geom_line(data = dfl1, aes(x = a, y = b)) +
    geom_line(data = dfu1, aes(x = a, y = b)) +
    geom_line(data = dfl2, aes(x = a, y = b)) +
    geom_line(data = dfu2, aes(x = a, y = b)) +
    geom_line(data = dfl3, aes(x = a, y = b)) +
    geom_line(data = dfu3, aes(x = a, y = b)) +
    geom_line(data = dfl4, aes(x = a, y = b)) +
    geom_line(data = dfu4, aes(x = a, y = b)) +
    geom_line(data = df4, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 2.7, label = "p < 0.001", size = 5)
  
)
dev.off()