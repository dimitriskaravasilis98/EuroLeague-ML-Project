library(readxl)
library(nortest)
library(corrplot)
library(ggplot2)
library(reshape2)
library(ggcorrplot)

data1=read_excel("Regular_means.xlsx")
reg_means <- data.frame(data1)
attach(reg_means)

shapiro.test(Points)
shapiro.test(X2PT..)
shapiro.test(X3PT..)
shapiro.test(FT..)
shapiro.test(OR)
shapiro.test(DR)
shapiro.test(TR)
shapiro.test(AST)
shapiro.test(STL)
shapiro.test(TO)
shapiro.test(BLK)
shapiro.test(BLKA)
shapiro.test(FC)
shapiro.test(FD)
shapiro.test(PIR)
shapiro.test(PACE)
shapiro.test(OR..)
shapiro.test(AST.TO)
shapiro.test(eFG..)
shapiro.test(TS..)

data2=read_excel("Playoffs_means.xlsx")
plf_means=data.frame(data2)
attach(plf_means)

shapiro.test(Points)
shapiro.test(X2PT..)
shapiro.test(X3PT..)
shapiro.test(FT..)
shapiro.test(OR)
shapiro.test(DR)
shapiro.test(TR)
shapiro.test(AST)
shapiro.test(STL)
shapiro.test(TO)
shapiro.test(BLK)
shapiro.test(BLKA)
shapiro.test(FC)
shapiro.test(FD)
shapiro.test(PIR)
shapiro.test(PACE)
shapiro.test(OR..)
shapiro.test(AST.TO)
shapiro.test(eFG..)
shapiro.test(TS..)

data=read_excel("Regular Season.xlsx")
reg <- data.frame(data)
attach(reg)
reg_ssn <- subset(reg, Season != 10)
plf_yes=subset(reg_ssn, Playoffs...Quarter.Finals == 1)
plf_no=subset(reg_ssn, Playoffs...Quarter.Finals == 0)
shapiro.test(plf_yes$Points)
shapiro.test(plf_yes$X2PT..)
shapiro.test(plf_yes$X3PT..)
shapiro.test(plf_yes$FT..)
shapiro.test(plf_yes$OR)
shapiro.test(plf_yes$DR)
shapiro.test(plf_yes$TR)
shapiro.test(plf_yes$AST)
shapiro.test(plf_yes$STL)
shapiro.test(plf_yes$TO)
shapiro.test(plf_yes$BLK)
shapiro.test(plf_yes$BLKA)
shapiro.test(plf_yes$FC)
shapiro.test(plf_yes$FD)
shapiro.test(plf_yes$PIR)
shapiro.test(plf_yes$PACE)
shapiro.test(plf_yes$OR..)
shapiro.test(plf_yes$AST.TO)
shapiro.test(plf_yes$eFG..)
shapiro.test(plf_yes$TS..)

shapiro.test(plf_no$Points)
shapiro.test(plf_no$X2PT..)
shapiro.test(plf_no$X3PT..)
shapiro.test(plf_no$FT..)
shapiro.test(plf_no$OR)
shapiro.test(plf_no$DR)
shapiro.test(plf_no$TR)
shapiro.test(plf_no$AST)
shapiro.test(plf_no$STL)
shapiro.test(plf_no$TO)
shapiro.test(plf_no$BLK)
shapiro.test(plf_no$BLKA)
shapiro.test(plf_no$FC)
shapiro.test(plf_no$FD)
shapiro.test(plf_no$PIR)
shapiro.test(plf_no$PACE)
shapiro.test(plf_no$OR..)
shapiro.test(plf_no$AST.TO)
shapiro.test(plf_no$eFG..)
shapiro.test(plf_no$TS..)

reg_means_noseason <- subset(reg_means, select = -Season)
M <- cor(reg_means_noseason, use = "pairwise.complete.obs")
p <- ggcorrplot(M,
                hc.order = FALSE,
                type = "lower",
                lab = TRUE,
                lab_size = 4.5,       
                digits = 2,
                colors = c("red","white","blue"),
                ggtheme = theme_minimal(base_size = 16)) 

ggsave("heatmap_corr.png", plot = p, width = 12, height = 10, dpi = 300)

plf_means_noseason <- subset(plf_means, select = -Season)
M <- cor(plf_means_noseason, use = "pairwise.complete.obs")
p <- ggcorrplot(M,
                hc.order = FALSE,
                type = "lower",
                lab = TRUE,
                lab_size = 4.5,       
                digits = 2,
                colors = c("red","white","blue"),
                ggtheme = theme_minimal(base_size = 16)) 

ggsave("heatmap_corr2.png", plot = p, width = 12, height = 10, dpi = 300)

plf_yes1 <- subset(plf_yes, select = -c(Season,Teams,Team.id,Playoffs...Quarter.Finals))
M <- cor(plf_yes1, use = "pairwise.complete.obs")
p <- ggcorrplot(M,
                hc.order = FALSE,
                type = "lower",
                lab = TRUE,
                lab_size = 4.5,       
                digits = 2,
                colors = c("red","white","blue"),
                ggtheme = theme_minimal(base_size = 16)) 

ggsave("heatmap_corr3.png", plot = p, width = 12, height = 10, dpi = 300)

plf_no1 <- subset(plf_no, select = -c(Season,Teams,Team.id,Playoffs...Quarter.Finals))
M <- cor(plf_no1, use = "pairwise.complete.obs")
p <- ggcorrplot(M,
                hc.order = FALSE,
                type = "lower",
                lab = TRUE,
                lab_size = 4.5,       
                digits = 2,
                colors = c("red","white","blue"),
                ggtheme = theme_minimal(base_size = 16)) 

ggsave("heatmap_corr4.png", plot = p, width = 12, height = 10, dpi = 300)

reg_means2 <- subset(reg_means, Season != 10)
reg_means2$Phase <- "Regular"
plf_means$Phase <- "Playoffs"
all_means <- rbind(reg_means2, plf_means)
t.test(reg_means2$Points, plf_means$Points, paired = FALSE)
t.test(reg_means2$X2PT.., plf_means$X2PT.., paired = FALSE)
t.test(reg_means2$X3PT.., plf_means$X3PT.., paired = FALSE)
t.test(reg_means2$FT.., plf_means$FT.., paired = FALSE)
t.test(reg_means2$OR, plf_means$OR, paired = FALSE)
t.test(reg_means2$DR, plf_means$DR, paired = FALSE)
t.test(reg_means2$TR, plf_means$TR, paired = FALSE)
wilcox.test(reg_means2$AST, plf_means$AST, paired = FALSE)
t.test(reg_means2$TO, plf_means$TO, paired = FALSE)
t.test(reg_means2$STL, plf_means$STL, paired = FALSE)
wilcox.test(reg_means2$BLK, plf_means$BLK, paired = FALSE)
wilcox.test(reg_means2$BLKA, plf_means$BLKA, paired = FALSE)
t.test(reg_means2$FC, plf_means$FC, paired = FALSE)
t.test(reg_means2$FD, plf_means$FD, paired = FALSE)
t.test(reg_means2$PIR, plf_means$PIR, paired = FALSE)
t.test(reg_means2$PACE, plf_means$PACE, paired = FALSE)
t.test(reg_means2$OR.., plf_means$OR.., paired = FALSE)
t.test(reg_means2$AST.TO, plf_means$AST.TO, paired = FALSE)
t.test(reg_means2$eFG.., plf_means$eFG.., paired = FALSE)
wilcox.test(reg_means2$TS.., plf_means$TS., paired = FALSE)


t.test(plf_yes$Points, plf_no$Points)
t.test(plf_yes$X2PT.., plf_no$X2PT..)
t.test(plf_yes$X3PT.., plf_no$X3PT..)
t.test(plf_yes$FT, plf_no$FT)
t.test(plf_yes$OR, plf_no$OR)
t.test(plf_yes$DR, plf_no$DR)
t.test(plf_yes$TR, plf_no$TR)
t.test(plf_yes$AST, plf_no$AST)
t.test(plf_yes$STL, plf_no$STL)
t.test(plf_yes$TO, plf_no$TO)
t.test(plf_yes$BLK, plf_no$BLK)
t.test(plf_yes$BLKA, plf_no$BLKA)
t.test(plf_yes$FC, plf_no$FC)
t.test(plf_yes$FD, plf_no$FD)
t.test(plf_yes$PIR, plf_no$PIR)
t.test(plf_yes$PACE, plf_no$PACE)
t.test(plf_yes$OR.., plf_no$OR..)
t.test(plf_yes$AST.TO, plf_no$AST.TO)
t.test(plf_yes$eFG.., plf_no$eFG..)
t.test(plf_yes$TS.., plf_no$TS..)
