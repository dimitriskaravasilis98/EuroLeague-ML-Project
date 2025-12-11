library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(ggplot2)
library(dbplyr)
library(dplyr)
library(tidyr)
library(aplpack)
library(scatterplot3d)
library(gridExtra)
library(patchwork)
library(fmsb)
library(TeachingDemos)
library(png)
library(grid)

data1=read_excel("Regular_means.xlsx")
reg_means <- data.frame(data1)
attach(reg_means)
sum(is.na(reg_means))
summary(reg_means)


par(mfrow=c(4,3),cex.main = 2,cex.lab = 1,cex.axis = 1)
plot(Season, Points, type="o", lwd=3,col="black", xlab="Season", ylab="Points")
plot(Season, X2PT.., type="o", lwd=3, col="blue", xlab="Season", ylab="2PT %")
plot(Season, X3PT.., type="o", lwd=3, col="red", xlab="Season", ylab="3PT %")
plot(Season, FT.., type="o", lwd=3, col="yellow", xlab="Season", ylab="FT %")
plot(Season, OR, type="o", lwd=3, col="green", xlab="Season", ylab="Offensive Rebounds")
plot(Season, DR, type="o", lwd=3, col="sienna", xlab="Season", ylab="Defensive Rebounds")
plot(Season, TR, type="o", lwd=3, col="wheat", xlab="Season", ylab="Total Rebounds")
plot(Season, AST, type="o", lwd=3, col="cyan", xlab="Season", ylab="Assists")
plot(Season, STL, type="o", lwd=3, col="salmon1", xlab="Season", ylab="Steals")
plot(Season, TO, type="o", lwd=3, col="darkgreen", xlab="Season", ylab="Turnovers")
plot(Season, BLK, type="o", lwd=3, col="violetred1", xlab="Season", ylab="Blocks")
plot(Season, BLKA, type="o", lwd=3, col="tomato1", xlab="Season", ylab="Blocks Against")
plot(Season, FC, type="o", lwd=3, col="gray13", xlab="Season", ylab="Fouls Committed")
plot(Season, FD, type="o", lwd=3, col="darkcyan", xlab="Season", ylab="Fouls Drawn")
plot(Season, PIR, type="o", lwd=3 ,col="lightgoldenrod", xlab="Season", ylab="PIR")
plot(Season, PACE, type="o", lwd=3 ,col="chocolate", xlab="Season", ylab="Pace")
plot(Season, OR.., type="o", lwd=3 ,col="palegreen", xlab="Season", ylab="Offensive Rebounds Percentage")
plot(Season, AST.TO, type="o", lwd=3 ,col="red3", xlab="Season", ylab="Assists/Turnovers")
plot(Season, eFG.., type="o", lwd=3 ,col="yellow4", xlab="Season", ylab="Effective Field Goal")
plot(Season,TS.., type="o", lwd=3 ,col="blue4", xlab="Season", ylab="True Shooting")

data2=read_excel("Playoffs_means.xlsx")
plf_means=data.frame(data2)
attach(plf_means)
sum(is.na(plf_means))
summary(plf_means)
par(mfrow=c(4,3),cex.main = 2,cex.lab = 1,cex.axis = 1)
plot(Season, Points, type="o", lwd=3,col="black", xlab="Season", ylab="Points")
plot(Season, X2PT.., type="o", lwd=3, col="blue", xlab="Season", ylab="2PT %")
plot(Season, X3PT.., type="o", lwd=3, col="red", xlab="Season", ylab="3PT %")
plot(Season, FT.., type="o", lwd=3, col="yellow", xlab="Season", ylab="FT %")
plot(Season, OR, type="o", lwd=3, col="green", xlab="Season", ylab="Offensive Rebounds")
plot(Season, DR, type="o", lwd=3, col="sienna", xlab="Season", ylab="Defensive Rebounds")
plot(Season, TR, type="o", lwd=3, col="wheat", xlab="Season", ylab="Total Rebounds")
plot(Season, AST, type="o", lwd=3, col="cyan", xlab="Season", ylab="Assists")
plot(Season, STL, type="o", lwd=3, col="salmon1", xlab="Season", ylab="Steals")
plot(Season, TO, type="o", lwd=3, col="darkgreen", xlab="Season", ylab="Turnovers")
plot(Season, BLK, type="o", lwd=3, col="violetred1", xlab="Season", ylab="Blocks")
plot(Season, BLKA, type="o", lwd=3, col="tomato1", xlab="Season", ylab="Blocks Against")
plot(Season, FC, type="o", lwd=3, col="gray13", xlab="Season", ylab="Fouls Committed")
plot(Season, FD, type="o", lwd=3, col="darkcyan", xlab="Season", ylab="Fouls Drawn")
plot(Season, PIR, type="o", lwd=3 ,col="lightgoldenrod", xlab="Season", ylab="PIR")
plot(Season, PACE, type="o", lwd=3 ,col="chocolate", xlab="Season", ylab="Pace")
plot(Season, OR.., type="o", lwd=3 ,col="palegreen", xlab="Season", ylab="Offensive Rebounds Percentage")
plot(Season, AST.TO, type="o", lwd=3 ,col="red3", xlab="Season", ylab="Assists/Turnovers")
plot(Season, eFG.., type="o", lwd=3 ,col="yellow4", xlab="Season", ylab="Effective Field Goal")
plot(Season,TS.., type="o", lwd=3 ,col="blue4", xlab="Season", ylab="True Shooting")

png("scatter_plots.png", width = 3000, height = 2000, res = 250)
par(mfrow=c(4,5),cex.main = 2,cex.lab = 1,cex.axis = 1)
plot(Points,PIR)
abline(lm(PIR~Points),col="red",lwd = 2)
plot(X2PT..,PIR)
abline(lm(PIR~X2PT..),col="red",lwd = 2)
plot(X3PT..,PIR)
abline(lm(PIR~X3PT..),col="red",lwd = 2)
plot(FT..,PIR)
abline(lm(PIR~FT..),col="red",lwd = 2)
plot(OR,PIR)
abline(lm(PIR~OR),col="red",lwd = 2)
plot(DR,PIR)
abline(lm(PIR~DR),col="red",lwd = 2)
plot(TR,PIR)
abline(lm(PIR~TR),col="red",lwd = 2)
plot(AST,PIR)
abline(lm(PIR~AST),col="red",lwd = 2)
plot(STL,PIR)
abline(lm(PIR~STL),col="red",lwd = 2)
plot(TO,PIR)
abline(lm(PIR~TO),col="red",lwd = 2)
plot(BLK,PIR)
abline(lm(PIR~BLK),col="red",lwd = 2)
plot(BLKA,PIR)
abline(lm(PIR~BLKA),col="red",lwd = 2)
plot(FC,PIR)
abline(lm(PIR~FC),col="red",lwd = 2)
plot(FD,PIR)
abline(lm(PIR~FD),col="red",lwd = 2)
plot(PACE,PIR)
abline(lm(PIR~PACE),col="red",lwd = 2)
plot(OR..,PIR)
abline(lm(PIR~OR..),col="red",lwd = 2)
plot(AST.TO,PIR)
abline(lm(PIR~AST.TO),col="red",lwd = 2)
plot(eFG..,PIR)
abline(lm(PIR~eFG..),col="red",lwd = 2)
plot(TS..,PIR)
abline(lm(PIR~TS..),col="red",lwd = 2)
dev.off()
cor(data1)

cher_data=reg_means[,c("X3PT..","PIR","PACE","AST.TO","OR..","Points")]
cher_data_scaled=scale(cher_data)
faces(cher_data_scaled,face.type = 1,main = "Chernoff Faces for every Season")

png("scatter_plots2.png", width = 3000, height = 2000, res = 250)
par(mfrow=c(4,5),cex.main = 2,cex.lab = 1,cex.axis = 1)
plot(Points,PIR)
abline(lm(PIR~Points),col="red",lwd = 2)
plot(X2PT..,PIR)
abline(lm(PIR~X2PT..),col="red",lwd = 2)
plot(X3PT..,PIR)
abline(lm(PIR~X3PT..),col="red",lwd = 2)
plot(FT..,PIR)
abline(lm(PIR~FT..),col="red",lwd = 2)
plot(OR,PIR)
abline(lm(PIR~OR),col="red",lwd = 2)
plot(DR,PIR)
abline(lm(PIR~DR),col="red",lwd = 2)
plot(TR,PIR)
abline(lm(PIR~TR),col="red",lwd = 2)
plot(AST,PIR)
abline(lm(PIR~AST),col="red",lwd = 2)
plot(STL,PIR)
abline(lm(PIR~STL),col="red",lwd = 2)
plot(TO,PIR)
abline(lm(PIR~TO),col="red",lwd = 2)
plot(BLK,PIR)
abline(lm(PIR~BLK),col="red",lwd = 2)
plot(BLKA,PIR)
abline(lm(PIR~BLKA),col="red",lwd = 2)
plot(FC,PIR)
abline(lm(PIR~FC),col="red",lwd = 2)
plot(FD,PIR)
abline(lm(PIR~FD),col="red",lwd = 2)
plot(PACE,PIR)
abline(lm(PIR~PACE),col="red",lwd = 2)
plot(OR..,PIR)
abline(lm(PIR~OR..),col="red",lwd = 2)
plot(AST.TO,PIR)
abline(lm(PIR~AST.TO),col="red",lwd = 2)
plot(eFG..,PIR)
abline(lm(PIR~eFG..),col="red",lwd = 2)
plot(TS..,PIR)
abline(lm(PIR~TS..),col="red",lwd = 2)
dev.off()
getwd()
cor(data2)

cher_data2=plf_means[,c("X3PT..","PIR","PACE","AST.TO","OR..","Points")]
cher_data_scaled2=scale(cher_data2)
faces(cher_data_scaled2,face.type = 1,main = "Chernoff Faces for every Season")

png("face_regular.png",  width = 1600, height = 1600, res = 150)
par(mar = c(1,1,5,1))
aplpack::faces(cher_data_scaled,  face.type = 1, main = "Regular Season",cex = 1.5)
dev.off()
png("face_playoffs.png", width = 1600, height = 1600, res = 150)
par(mar = c(1,1,5,1))
aplpack::faces(cher_data_scaled2, face.type = 1, main = "Playoffs",cex = 1.5)
dev.off()
img1 <- readPNG("face_regular.png");  g1 <- rasterGrob(img1, interpolate = TRUE)
img2 <- readPNG("face_playoffs.png"); g2 <- rasterGrob(img2, interpolate = TRUE)
png("faces_side_by_side_big.png", width = 3200, height = 1600, res = 150)
grid.arrange(g1, g2, ncol=2)
dev.off()

data=read_excel("Regular Season.xlsx")
reg <- data.frame(data)
attach(reg)
reg_ssn <- subset(reg, Season != 10)
sum(is.na(reg_ssn))
summary(reg_ssn)
plf_yes=subset(reg_ssn, Playoffs...Quarter.Finals == 1)
plf_no=subset(reg_ssn, Playoffs...Quarter.Finals == 0)
summary(plf_yes)
summary(plf_no)

par(mfrow=c(1,1))
boxplot(Points ~ Playoffs...Quarter.Finals, 
        data = reg_ssn,
        col = c("red", "blue"),
        names = c("0", "1"),
        main = "Points by Playoff Qualification",
        ylab = "Points",
        xlab= "Playoffs")
boxplot(PIR ~ Playoffs...Quarter.Finals, 
        data = reg_ssn,
        col = c("red", "blue"),
        names = c("0", "1"),
        main = "PIR by Playoff Qualification",
        ylab = "PIR",
        xlab= "Playoffs")
out_points =boxplot.stats(reg_ssn$Points)$out
reg_ssn[which(reg_ssn$Points %in% boxplot.stats(reg_ssn$Points)$out), ]
par(mfrow=c(1,3))
boxplot(PACE ~ Playoffs...Quarter.Finals, 
        data = reg_ssn,
        col = c("red", "blue"),
        names = c("0", "1"),
        main = "PACE by Playoff Qualification",
        ylab = "PACE",
        xlab= "Playoffs")
boxplot(AST.TO ~ Playoffs...Quarter.Finals, 
        data = reg_ssn,
        col = c("red", "blue"),
        names = c("0", "1"),
        main = "AST/TO by Playoff Qualification",
        ylab = "AST/TO",
        xlab= "Playoffs")
boxplot(TS.. ~ Playoffs...Quarter.Finals, 
        data = reg_ssn,
        col = c("red", "blue"),
        names = c("0", "1"),
        main = "TS % by Playoff Qualification",
        ylab = "TS %",
        xlab= "Playoffs")

avg_data <- reg_ssn %>%
  group_by(Season, Playoffs...Quarter.Finals) %>%
  summarise(
    Points = mean(Points, na.rm = TRUE),
    PIR = mean(PIR, na.rm = TRUE),
    AST.TO = mean(AST.TO, na.rm = TRUE),
    TS.. = mean(TS.., na.rm = TRUE),
    PACE = mean(PACE, na.rm = TRUE)
  )

p1=ggplot(avg_data, aes(x = Season, y = Points, color = factor(Playoffs...Quarter.Finals))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("red", "blue"), labels = c("Not Qualified", "Qualified")) +
  labs(
       x = "Season",
       y = "Points",
       color = "Playoffs") +
  theme_minimal()
 
  
p2=ggplot(avg_data, aes(x = Season, y = PIR, color = factor(Playoffs...Quarter.Finals))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("red", "blue"), labels = c("Not Qualified", "Qualified")) +
  labs(
       x = "Season",
       y = "PIR",
       color = "Playoffs") +
  theme_minimal()

(p1 | p2 ) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

p3=ggplot(avg_data, aes(x = Season, y = PACE, color = factor(Playoffs...Quarter.Finals))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("red", "blue"), labels = c("Not Qualified", "Qualified")) +
  labs(
    x = "Season",
    y = "PACE",
    color = "Playoffs") +
  theme_minimal()
p4=ggplot(avg_data, aes(x = Season, y = AST.TO, color = factor(Playoffs...Quarter.Finals))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("red", "blue"), labels = c("Not Qualified", "Qualified")) +
  labs(,
       x = "Season",
       y = "AST/TO",
       color = "Playoffs") +
  theme_minimal()
p5=ggplot(avg_data, aes(x = Season, y = TS.., color = factor(Playoffs...Quarter.Finals))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("red", "blue"), labels = c("Not Qualified", "Qualified")) +
  labs(
       x = "Season",
       y = "TS%",
       color = "Playoffs") +
  theme_minimal()
(p3 | p4 | p5 ) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

colors <- c("0" = "red", "1" = "blue")
p11 <- ggplot(reg_ssn, aes(x = Points, fill = factor(Playoffs...Quarter.Finals))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  scale_fill_manual(values = colors, labels = c("Not Qualified", "Qualified")) +
  labs(title = "Histogram of Points", x = "Points", y = "Frequency", fill = "Playoffs") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
p22 <- ggplot(reg_ssn, aes(x = PIR, fill = factor(Playoffs...Quarter.Finals))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  scale_fill_manual(values = colors, labels = c("Not Qualified", "Qualified")) +
  labs(title = "Histogram of PIR", x = "PIR", y = "Frequency", fill = "Playoffs") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
(p11 | p22  ) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

p33 <- ggplot(reg_ssn, aes(x = Points, color = factor(Playoffs...Quarter.Finals), fill = factor(Playoffs...Quarter.Finals))) +
  geom_density(alpha = 0.4) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors, labels = c("Not Qualified", "Qualified")) +
  guides(color = "none") + 
  labs(title = "Density Plot of Points", x = "Points", y = "Density", fill = "Playoffs", color = "Playoffs") +
  theme_minimal(base_size = 12)
p44 <- ggplot(reg_ssn, aes(x = PIR, color = factor(Playoffs...Quarter.Finals), fill = factor(Playoffs...Quarter.Finals))) +
  geom_density(alpha = 0.4) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors, labels = c("Not Qualified", "Qualified")) +
  guides(color = "none") +
  labs(title = "Density Plot of PIR", x = "PIR", y = "Density", fill = "Playoffs", color = "Playoffs") +
  theme_minimal(base_size = 12)
(p33 | p44  ) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

means_yes <- colMeans(plf_yes[, c("Points", "PIR", "PACE", "AST.TO", "TS..", "X2PT..", "X3PT..", "FT..", 
                                  "OR", "DR", "TR", "AST", "STL", "TO", "BLK", "BLKA", "FC", "FD", "eFG..", "OR..")], na.rm = TRUE)
means_no  <- colMeans(plf_no[,  c("Points", "PIR", "PACE", "AST.TO", "TS..", "X2PT..", "X3PT..", "FT..", 
                                  "OR", "DR", "TR", "AST", "STL", "TO", "BLK", "BLKA", "FC", "FD", "eFG..", "OR..")], na.rm = TRUE)
max_vals <- apply(reg_ssn[, c("Points", "PIR", "PACE", "AST.TO", "TS..", "X2PT..", "X3PT..", "FT..", 
                              "OR", "DR", "TR", "AST", "STL", "TO", "BLK", "BLKA", "FC", "FD", "eFG..", "OR..")], 2, max, na.rm = TRUE)
min_vals <- apply(reg_ssn[, c("Points", "PIR", "PACE", "AST.TO", "TS..", "X2PT..", "X3PT..", "FT..", 
                               "OR", "DR", "TR", "AST", "STL", "TO", "BLK", "BLKA", "FC", "FD", "eFG..", "OR..")], 2, min, na.rm = TRUE)
radar_data <- rbind(max_vals,
                    min_vals,
                    means_no,
                    means_yes)

rownames(radar_data) <- c("Max", "Min", "Not Qualified", "Qualified")
radar_data <- as.data.frame(radar_data)
colors_border <- c("red", "blue")
colors_in     <- c(scales::alpha("red", 0.3), scales::alpha("blue", 0.3))
png("radar_plot.png", width = 800, height = 800)
radarchart(radar_data,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_in,
           plwd = 2,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey40",
           caxislabels = seq(0, max(max_vals), length.out = 5),
           cglwd = 0.8,
           vlcex = 0.9,
           title = "Radar Plot: Playoffs vs No Playoffs")

legend("bottomright", legend = rownames(radar_data)[3:4],
       col = colors_border, lty = 1, lwd = 2, bty = "n")
dev.off()

