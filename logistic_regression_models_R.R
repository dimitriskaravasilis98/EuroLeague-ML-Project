library(readxl)
library(car)
library(pROC)
library(cvAUC)
library(ResourceSelection)
library(DescTools)



data=read_excel("Regular Season.xlsx")
reg <- data.frame(data)
attach(reg)
reg_ssn <- subset(reg, Season != 10)
Playoffs...Quarter.Finals=as.factor(Playoffs...Quarter.Finals)

model_start1 <- glm(Playoffs...Quarter.Finals ~ 1, family=binomial(link=logit))
modelfull<-glm(Playoffs...Quarter.Finals ~
                 Points+X2PT..+X3PT..+OR+DR+TR+AST+STL+TO+BLK+BLKA+FC+PACE+OR..+AST.TO+TS..+eFG.., family=binomial(link=logit))
stepMod1 <- step(model_start1, scope = list(lower = model_start1, upper = modelfull),direction
                 = "both") 
summary(stepMod1)
anova(stepMod1)
vif(stepMod1)
y=Playoffs...Quarter.Finals
hoslem.test(stepMod1$y, fitted(stepMod1))

prob <- predict(stepMod1, type = "response")
roc_plf <- roc(reg_ssn$Playoffs...Quarter.Finals, prob)
plot(roc_plf, main = "ROC για Playoffs")
auc(roc_plf)
PseudoR2(stepMod1, which ="all")

plf_yes=subset(reg_ssn, Playoffs...Quarter.Finals == 1)
Top4=as.factor(Top4)
model_start2 <- glm(Top4 ~ 1,data = plf_yes, family=binomial(link=logit))
modelfull2<-glm(Top4 ~
                  Points+X3PT..+FT..+OR+DR+TR+STL+TO+BLK+BLKA+FC+FD+PACE+OR..+eFG..,data = plf_yes, family=binomial(link=logit))
stepMod2 <- step(model_start2, scope = list(lower = model_start2, upper = modelfull2),direction
                 = "both") 
summary(stepMod2)
anova(stepMod2)
vif(stepMod2)

y=Top4
hoslem.test(stepMod2$y, fitted(stepMod2))
PseudoR2(stepMod2, which ="all")
prob <- predict(stepMod2, type = "response")
roc_top4 <- roc(plf_yes$Top4, prob)
plot(roc_top4, main = "ROC για Top4")
auc(roc_top4)


data2=read_excel("Playoffs.xlsx")
plf <- data.frame(data2)
attach(plf)

upset_df=subset(plf, Underdog == 1)
nrow(upset_df)
Upset=as.factor(Upset)
model_start4 <- glm(Upset ~ 1,data = upset_df,
                    family = binomial(link = "logit"))
modelfull5 <- glm(
  Upset ~ Points+X2PT..+X3PT..+FT..+DR+OR+TR+AST+TO+STL+FC+FD+BLK+BLKA+OR..+PACE+eFG..+AST.TO+TS..,
  data   = upset_df,
  family = binomial(link = "logit")
)
stepMod5 <- step(model_start4,
                 scope = list(lower = model_start4, upper = modelfull5),
                 direction = "both")
summary(stepMod5)
anova(stepMod5)
vif(stepMod5)

Games=as.factor(Games)
model_games=glm(Upset~Games+X3PT..+DR,data = upset_df,family = binomial(link = "logit"))
summary(model_games)
anova(model_games)

y=upset_df$Upset
hoslem.test(stepMod5$y, fitted(stepMod5))
PseudoR2(stepMod5, which ="all")
prob <- predict(stepMod5, type = "response")
roc_upset <- roc(upset_df$Upset, prob)
plot(roc_upset, main = "ROC για upset")
auc(roc_upset)


vars <- c("Points", "X2PT..", "X3PT..", "FT..", "OR", "DR", "TR",
          "AST", "STL", "TO", "BLK", "BLKA", "FC", "FD",
          "PIR", "PACE", "OR..", "AST.TO", "TS..", "eFG..")
cor_matrix <- cor(plf[ , vars], use = "pairwise.complete.obs")
round(cor_matrix, 2)
