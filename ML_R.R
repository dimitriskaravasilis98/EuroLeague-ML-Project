library(readxl)
library(dplyr)
library(Boruta)
library(randomForest)
library(ggplot2)
library(factoextra)
library(ggrepel)
library(cluster)
library(fpc)
library(mclust)
library(clValid)
library(fossil)
library(dbscan)
library(kernlab)
library(caTools)
library(e1071)
library(caret)
library(dplyr)
library(class)
library(pROC)
library(fmsb)
library(plotly)


data=read_excel("Regular Season.xlsx")
reg <- data.frame(data)
attach(reg)
reg_ssn <- subset(reg, Season != 10)
Playoffs...Quarter.Finals=as.factor(Playoffs...Quarter.Finals)

features <- reg_ssn[, c(
  "Points","X2PT..","X3PT..","FT..","OR","DR","TR","AST","STL","TO",
  "BLK","BLKA","FC","FD","PIR","PACE","OR..","AST.TO","TS..","eFG.."
)]
labels1 <- reg_ssn$`Playoffs...Quarter.Finals`

set.seed(25)
boruta_res1 <- Boruta(
  labels1 ~ .,
  data = na.omit(data.frame(labels1, features)),
  doTrace = 2
)
selected_features1 <- getSelectedAttributes(boruta_res1, withTentative = FALSE)
features_selected1   <- features[, selected_features1]
features_normalized1 <- scale(features_selected1)
plot(boruta_res1, cex.axis = .7, las = 2, xlab = "", main = "Variable Importance (Boruta)")


fviz_nbclust(features_normalized1, kmeans, method = "wss") + geom_vline(xintercept = 2,
                                                                      linetype = 2) + labs(subtitle = "Elbow Method")
set.seed(25)
kmeans_res1 <- kmeans(features_normalized1, centers = 2)
cluster_assignments1 <- kmeans_res1$cluster
cluster_plot <- fviz_cluster(list(data = features_normalized1, cluster = cluster_assignments1),
                             geom = 'point', palette = "jco", repel = TRUE, show.clust.cent = FALSE)
cluster_means_scaled <- aggregate(
  features_normalized1,
  by = list(Cluster = cluster_assignments1),
  FUN = mean
)
cluster_means_scaled
table(cluster_assignments1)           


silhouette_avg1 <- silhouette(kmeans_res1$cluster, dist(features_normalized1))
summary(silhouette_avg1)
dunn_index1 <- dunn(dist(features_normalized1), kmeans_res1$cluster)
adj_rand_index1 <- adj.rand.index(kmeans_res1$cluster, labels1)

hc1 <- agnes(features_normalized1, method = "ward")
pltree(hc1, cex = 0.6, hang = -1, main = "Dendrogram", labels = F)
pred_labels1 <- cutree(hc1, k = 2)
cluster_plot <- fviz_cluster(list(data = features_normalized1, cluster = pred_labels1), geom =
                               'point', palette = "jco", repel = TRUE, show.clust.cent = FALSE)
silhouette_avg2 <- silhouette(pred_labels1, dist(features_normalized1))
summary(silhouette_avg2)
dunn_index2 <- dunn(dist(features_normalized1),pred_labels1)
adj_rand_index2 <- adj.rand.index(pred_labels1, labels1)



dk_data <- data.frame(features_normalized1,labels1)
data <- data.frame(features_normalized1)
labels1 = factor(labels1)
set.seed(25)
split = sample.split(dk_data$labels1, SplitRatio = 0.70)
training_set = subset(dk_data, split == TRUE)
test_set = subset(dk_data, split == FALSE)





train_X <- training_set[, -ncol(training_set)]
train_y <- training_set$labels1
test_X <- test_set[, -ncol(test_set)]
test_y <- test_set$labels1
knn_pred <- knn(train = train_X,
                test = test_X,
                cl = train_y,
                k = 5,prob = TRUE)
conf_matrix <- table(Predicted = knn_pred, Actual = test_y)
conf_matrix
conf <- confusionMatrix(conf_matrix)
cm_df <- as.data.frame(conf_matrix)
colnames(cm_df) <- c("Predicted", "Actual", "Freq")

ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()

TP <- conf_matrix[2, 2]
FP <- conf_matrix[2, 1]
TN <- conf_matrix[1, 1]
FN <- conf_matrix[1, 2]

accuracy <- (TP + TN) / sum(conf_matrix)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * precision * recall / (precision + recall)
prob_1   <- ifelse(knn_pred == "1", attr(knn_pred, "prob"), 1 - attr(knn_pred, "prob"))
prob_1 <- as.numeric(prob_1)
test_y_num <- as.numeric(as.character(test_y)) 
roc_knn <- pROC::roc(test_y_num, prob_1)
auc_knn <- pROC::auc(roc_knn)
auc_knn
pROC::plot.roc(roc_knn,
               col  = "blue",
               lwd  = 3,
               main = "ROC Curve – KNN")




training_set$labels1 <- factor(training_set$labels1)
test_set$labels1     <- factor(test_set$labels1)

set.seed(25)
classifier_RF = randomForest(formula = labels1 ~ . , data = training_set)
y_pred = predict(classifier_RF, newdata = test_set)
cm <- confusionMatrix(data=y_pred, reference=factor(test_set$labels1), dnn = c("Prediction",
                                                                               "Reference"))
cm$table
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
ggplot(plt, aes(Reference, Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    x = "Reference",
    y = "Prediction",
    title = "Confusion Matrix"
  ) +
  scale_x_discrete(labels = c("0", "1")) +
  scale_y_discrete(labels = c("1", "0")) +
  theme(plot.title = element_text(hjust = 0.5))

TP <- cm$table[2, 2]
FP <- cm$table[2, 1]
TN <- cm$table[1, 1]
FN <- cm$table[1, 2]

accuracy <- (TP + TN) / sum(cm$table)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * precision * recall / (precision + recall)
rf_prob <- predict(classifier_RF, newdata = test_set, type = "prob")[, 2]
y_true <- as.numeric(as.character(test_set$labels1))
roc_rf <- pROC::roc(response = y_true, predictor = rf_prob)
auc_rf <- auc(roc_rf)
auc_rf
plot.roc(
  roc_rf,
  col = "blue",
  lwd = 3,
  main = "ROC Curve – Random Forest"
)



set.seed(25)

xgb_fit <- train(labels1 ~ ., data = training_set, method = "xgbTree")
y_pred <- predict(xgb_fit, newdata = test_set)
cm <- confusionMatrix(data = y_pred,
                      reference = factor(test_set$labels1),
                      dnn = c("Prediction", "Reference"))
cm
plt <- as.data.frame(cm$table)
ggplot(plt, aes(Reference, Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    x = "Reference",
    y = "Prediction",
    title = "Confusion Matrix"
  ) +
  scale_x_discrete(labels = c("0", "1")) +
  scale_y_discrete(labels = c("0", "1")) +
  theme(plot.title = element_text(hjust = 0.5))

TP <- cm$table[2, 2]
FP <- cm$table[2, 1]
TN <- cm$table[1, 1]
FN <- cm$table[1, 2]

accuracy <- (TP + TN) / sum(cm$table)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * precision * recall / (precision + recall)
prob <- predict(xgb_fit, newdata = test_set, type = "prob")[, 2]
y_true <- as.numeric(as.character(test_set$labels1))
roc_xgb <- pROC::roc(y_true, prob, quiet = TRUE)
auc_xgb <- pROC::auc(roc_xgb)
auc_xgb
pROC::plot.roc(
  roc_xgb,
  col  = "blue",
  lwd  = 3,
  main = "ROC Curve – XGBoost"
)

set.seed(25)

classifier_svm = svm(formula = labels1 ~ ., data = training_set, type = 'C-classification', kernel =
                   'radial',prob=TRUE)
y_pred = predict(classifier_svm, newdata = test_set,prob=TRUE)

cm <- confusionMatrix(data=y_pred, reference=factor(test_set$labels1), dnn = c("Prediction",
                                                                               "Reference"))
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
ggplot(plt, aes(Reference, Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    x = "Reference",
    y = "Prediction",
    title = "Confusion Matrix"
  ) +
  scale_x_discrete(labels = c("0", "1")) +
  scale_y_discrete(labels = c("1", "0")) +
  theme(plot.title = element_text(hjust = 0.5))
cm$table
TP <- cm$table[2, 2]
FP <- cm$table[2, 1]
TN <- cm$table[1, 1]
FN <- cm$table[1, 2]
accuracy <- (TP + TN) / sum(cm$table)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * precision * recall / (precision + recall)
prob_svm <- attr(y_pred, "probabilities")[, "1"]
y_true <- as.numeric(as.character(test_set$labels1))
roc_svm <- pROC::roc(y_true, prob_svm, quiet = TRUE)
auc_svm <- pROC::auc(roc_svm)
auc_svm
pROC::plot.roc(
  roc_svm,
  col = "blue",
  lwd = 3,
  main = "ROC Curve – SVM"
)

metrics <- data.frame(
  row.names = c("Max", "Min", "KNN", "RandomForest", "XGBoost", "SVM", "ANN"),
  
  Accuracy  = c(1, 0, 0.753, 0.824, 0.824, 0.847, 0.859),
  Precision = c(1, 0, 0.710, 0.788, 0.806, 0.818, 0.806),
  Recall    = c(1, 0, 0.647, 0.765, 0.735, 0.794, 0.853),
  F1        = c(1, 0, 0.677, 0.776, 0.769, 0.806, 0.829),
  AUC       = c(1, 0, 0.873, 0.92, 0.92, 0.903, 0.927)
)
# Χρώματα για κάθε μοντέλο
# Χρώματα για τις γραμμές μόνο
colors <- c(
  rgb(0.1, 0.3, 0.9, 0.9),  # KNN
  rgb(0.9, 0.2, 0.2, 0.9),  # RF
  rgb(0.2, 0.7, 0.2, 0.9),  # XGB
  rgb(0.7, 0.2, 0.7, 0.9),  # SVM
  rgb(0.9, 0.6, 0.1, 0.9)   # ANN
)

# Καθαρό άσπρο φόντο
par(bg = "white")
par(mar = c(2, 2, 4, 2))

radarchart(
  metrics,
  axistype = 1,
  
  # ΜΟΝΟ γραμμές
  pcol = colors,
  pfcol = rep(NA, 5),    # <--- ΑΦΑΙΡΕI ΤΟ ΓΕΜΙΣΜΑ
  plwd = 3,
  plty = 1,
  
  # Grid styling
  cglcol = "grey70",
  cglty = 1,
  cglwd = 0.8,
  
  # Axis labels
  axislabcol = "grey30",
  caxislabels = seq(0.60, 0.90, 0.05),
  vlcex = 1.0,
  
  title = "Σύγκριση μοντέλων ως προς τις μετρικές"
)

legend(
   x = 0.5, y = 1.2,
  legend = c("KNN", "Random Forest", "XGBoost", "SVM", "ANN"),
  col = colors,
  lty = 1,
  lwd = 3,
  bty = "n",
  cex = 1.0
)

