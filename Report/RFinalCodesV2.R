# Arash Sadeghi Bablan


library(readxl)

#Chapter 1: Data Cleaning with R
RawDataPath <- "C:\\Programming\\DataMiningProject\\Sadeghi_data\\Sadeghi_dataV1.xlsx"
RawData <- read_excel(RawDataPath)
colnames(RawData)

colnames(RawData)[colnames(RawData) == "DARAMAD.M.KH.5"] <- "DARAMAD.M.KH.4"
RawData[is.na(RawData)] <- 0
RawData$"H.KHORAKI.NOOSHIDANI" <- ifelse(RawData$"H.KHORAKI.NOOSHIDANI" == 0, NA, RawData$"H.KHORAKI.NOOSHIDANI")

RawData$M.INCOME <- rowSums(RawData[,c("M.DARAMAD.NAKH", "HOOGHOOGH.MOSTAMAR", "GH.MOSTAMAR", "M.DARAMAD.KH", "DARYAFTI.NAKH.F", "DARAMAD.M.KH.1", "DARAMAD.M.KH.2", "DARAMAD.M.KH.3", "DARAMAD.M.KH.4")])
RawData$DAHAK <- as.integer(cut(RawData$M.INCOME, quantile(RawData$M.INCOME, probs = seq(0, 1, 0.1)), labels = 1:10, include.lowest = TRUE))
RawData$DAHAK10 <- ifelse(RawData$DAHAK == 10, 1, 0)
write.csv(RawData, file = file.path(dirname(RawDataPath), "Sadeghi_data_CleanV1.csv"), row.names = FALSE)

prop.missing <- sum(is.na(RawData$"H.KHORAKI.NOOSHIDANI"))/nrow(RawData)
percent.missing <- prop.missing * 100
percent.missing

median.value <- median(RawData$"H.KHORAKI.NOOSHIDANI", na.rm = TRUE)
RawData$"H.KHORAKI.NOOSHIDANI" <- ifelse(is.na(RawData$"H.KHORAKI.NOOSHIDANI"), median.value, RawData$"H.KHORAKI.NOOSHIDANI")

write.csv(RawData, file = file.path(dirname(RawDataPath), "Sadeghi_data_CleanV2.csv"), row.names = FALSE)

#Load libraries
library(ggplot2)
library(hexbin)
library(vcd)
library(ggridges)
library(ggradar)

# Load the cleaned data

CleanData <- read.csv("C:\\Programming\\DataMiningProject\\Sadeghi_data\\Sadeghi_data_CleanV2.csv")

summary(CleanData)

hist(CleanData$sen)
barplot(table(CleanData$jensiat))
plot(CleanData$sen, CleanData$DAHAK10)

cor(CleanData)


# Plot the distribution of education level (savad)
ggplot(CleanData, aes(x=savad)) + 
  geom_bar(fill='blue') +
  xlab("Education Level (1: Educated, 0: Cannot Read)") + 
  ylab("Frequency") +
  ggtitle("Distribution of Education Level")

# Plot the proportion of males and females
ggplot(CleanData, aes(x=1, fill=factor(jensiat))) + 
  geom_bar(width=1) + 
  scale_fill_manual(values=c("#0072B2", "#D55E00"), 
                    labels=c("Male", "Female")) +
  coord_polar(theta='y') + 
  ggtitle("Proportion of Males and Females")

# Plot the distribution of household income
ggplot(CleanData, aes(x=factor(DAHAK10), y=M.INCOME)) + 
  geom_boxplot(fill='blue') +
  xlab("Income (0: Not Rich, 1: Rich)") + 
  ylab("Household Income") +
  ggtitle("Distribution of Household Income")

# Plot the relationship between age and education level
ggplot(CleanData, aes(x=sen, y=savad)) + 
  geom_point(color='blue') +
  xlab("Age") + 
  ylab("Education Level (1: Educated, 0: Cannot Read)") +
  ggtitle("Relationship between Age and Education Level")

#Plot the relationship between age and education level, coloring the points based on gender:
ggplot(CleanData, aes(x = sen, y = savad, color = factor(jensiat))) +
  geom_point() +
  scale_color_discrete(name = "Gender", labels = c("Male", "Female")) +
  xlab("Age") +
  ylab("Education level (1=educated, 0=cannot read)") +
  ggtitle("Relationship between Age and Education level")

#Plot the distribution of household size using a histogram:
ggplot(CleanData, aes(x = T.O)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  xlab("Number of rooms in the house") +
  ylab("Frequency") +
  ggtitle("Distribution of Household Size")

#Plot the relationship between job state and age using a boxplot:
ggplot(CleanData, aes(x = factor(v.f.s), y = sen)) +
  geom_boxplot() +
  xlab("Job state (1=works, 2=doesn't work, 3=income without working, 4=student, 5=housewife, 6=other)") +
  ylab("Age") +
  ggtitle("Relationship between Job State and Age")

#Plot the distribution of household income using a density plot:
ggplot(CleanData, aes(x = M.INCOME)) +
  geom_density(fill = "blue", color = "black") +
  xlab("Household Income") +
  ylab("Density") +
  ggtitle("Distribution of Household Income")

#NOT GOOD
#Plot the relationship between the type of house and household size using a scatterplot:
ggplot(CleanData, aes(x = T.O, y = factor(M.O.B))) +
  geom_point(shape = 1) +
  xlab("Number of rooms in the house") +
  ylab("Type of house") +
  ggtitle("Relationship between Type of House and Household Size")

#Hexbin Plot: To visualize the distribution of two continuous variables and show
#the density of points in a two-dimensional space, you can use a hexbin plot.
hexbinplot(CleanData$sen ~ CleanData$M.INCOME, xlab = "Age", ylab = "Total Income", 
           main = "Hexbin Plot of Age vs Total Income",
           gridsize = 20, colramp = colorRampPalette(c("blue", "yellow", "red")))

#Mosaic Plot: To show the relationship between two categorical variables, you can use a mosaic plot.

mosaicplot(table(CleanData$jensiat, CleanData$DAHAK10), main = "Mosaic Plot of Gender and Rich/Not Rich", 
           color = TRUE, cex.axis = 0.6)


#Box-and-Whisker Plot with Jitter: To visualize the distribution of multiple continuous variables for
#multiple categories, you can use a box-and-whisker plot with jitter.
ggplot(RawData, aes(x = as.factor(DAHAK10), y = M.INCOME, color = as.factor(jensiat))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(x = "Rich/Not Rich", y = "Total Income", color = "Gender") +
  ggtitle("Box-and-Whisker Plot with Jitter of Total Income by Rich/Not Rich and Gender")


library(rpart)
library(lattice)
library(caret)
library(ROCR)
library(dplyr)
library(ggplot2)
# Load the randomForest package
library(randomForest)

#Decision Tree
# Load the cleaned data
clean_data <- read.csv("C:\\Programming\\DataMiningProject\\Sadeghi_data\\Sadeghi_data_CleanV3.csv")

# Split the data into training and test sets
set.seed(123) # set seed for reproducibility

training_idx <- sample(1:nrow(clean_data), 0.7 * nrow(clean_data))
training_data <- clean_data[training_idx, ]
test_data <- clean_data[-training_idx, ]

# Build the decision tree model
model <- rpart(DAHAK10 ~ ., data = training_data, method = "class")

# Make predictions on the test data
predictions <- predict(model, test_data, type = "class")
summary(predictions)
# Evaluate the model's accuracy
accuracy <- mean(predictions == test_data$DAHAK10)
print(paste("Accuracy:", accuracy))

# Plot the tree
plot(model)
text(model)


cm_tree <- confusionMatrix(as.factor(predictions), as.factor(test_data$DAHAK10))
cm_tree
# Plot the ROC curve with ggplot2
ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ggtitle("ROC Curve") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_classic()

############ bad model
# Build the random forest model
model <- randomForest(DAHAK10 ~ ., data = training_data, method = "class")

# Make predictions on the test data
predictions <- predict(model, test_data, type = "class")

# Evaluate the model's accuracy
accuracy <- mean(predictions == test_data$DAHAK10)
print(paste("Accuracy:", accuracy))

# Plot the feature importance
plot(model$importance, main = "Feature Importance")

confusion_matrix <- confusionMatrix(test_data$DAHAK10, predictions)

test_data$DAHAK10 <- as.factor(test_data$DAHAK10)
predictions <- as.factor(predictions)
levels(test_data$DAHAK10) <- levels(predictions)
confusion_matrix <- confusionMatrix(test_data$DAHAK10, predictions)
confusion_matrix

# Load the data
data(DAHAK10)

# Split the data into training and test data sets
splitIndex <- createDataPartition(DAHAK10$DAHAK10, p = 0.8, list = FALSE, times = 1)
training_data <- DAHAK10[ splitIndex,]
test_data  <- DAHAK10[-splitIndex,]

# Train the random forest model with cross-validation
model_control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
model <- train(DAHAK10 ~ ., data = training_data, method = "rf", trControl = model_control)

# Make predictions on the test data
predictions <- predict(model, test_data)

# Evaluate the model's accuracy
accuracy <- mean(predictions == test_data$DAHAK10)
print(paste("Accuracy:", accuracy))

# Plot the feature importance
plot(varImp(model))



# Load the cleaned data
clean_data <- read.csv("C:\\Programming\\DataMiningProject\\Sadeghi_data\\Sadeghi_data_CleanV3.csv")

# Split the data into training and test sets
set.seed(123) # set seed for reproducibility
training_idx <- sample(1:nrow(clean_data), 0.7 * nrow(clean_data))
training_data <- clean_data[training_idx, ]
test_data <- clean_data[-training_idx, ]

# Build the logistic regression model
model <- glm(DAHAK10 ~ ., data = training_data, family = binomial(link = "logit"))

# Make predictions on the test data
predictions <- predict(model, test_data, type = "response")
predictions_class <- ifelse(predictions > 0.5, "1", "0")

# Evaluate the model's accuracy
accuracy <- mean(predictions_class == test_data$DAHAK10)
print(paste("Accuracy:", accuracy))

# Load the necessary libraries
library(caret)

# Fit the logistic regression model
model <- train(DAHAK10 ~ ., data = training_data, method = "glm", family = binomial("logit"))

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Convert predictions to a binary format
predictions_bin <- ifelse(predictions > 0.5, 1, 0)
# Calculate the confusion matrix
cm <- confusionMatrix(as.factor(predictions_bin), as.factor(test_data$DAHAK10))
  # Extract precision, recall, and F1-score
precision <- cm$byClass["Pos Pred Value"]
recall <- cm$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)
precision
recall
f1_score

# Print the results
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1_score, "\n")

# Plot the confusion matrix
library(ggplot2)
library(plotly)
plot_confusion_matrix <- function(cm) {
  cm_df <- data.frame(
    Reference = c(rep("Positive", cm[1, 1]), rep("Negative", cm[2, 2])),
    Prediction = c(rep("Positive", cm[1, 1]), rep("Negative", cm[2, 2]))
  )
  ggplot(cm_df, aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = ..count..), color = "white") +
    scale_fill_gradient(low = "red", high = "green", limits = c(0, max(cm))) +
    geom_text(aes(label = ..count..), color = "white") +
    labs(x = "Reference", y = "Prediction")
}
cm_plot <- plot_confusion_matrix(cm$table)
# Plot the ROC curve
library(pROC)
library(drc)
roc_obj <- roc(test_data$DAHAK10, as.numeric(predictions_bin))
roc_plot <- plot(roc_obj, print.auc=TRUE, col="#00AFBB", print.thres=FALSE)
# Plot the Precision-Recall curve
pr_obj <- pr(test_data$DAHAK10, as.numeric(predictions_bin))
pr_plot <- plot(pr_obj, main="Precision-Recall Curve", col="#00AFBB")

# neural networks
# partition the data
set.seed(2)
# nueralnet hiden =3
training=sample(nrow(clean_data), nrow(clean_data)*0.7)
validation=setdiff(nrow(clean_data), training)

training_idx <- sample(rwown(clean_data), 0.7 * nrow(clean_data))
training_data <- clean_data[training_idx, ]
test_data <- clean_data[-training_idx, ]
attach(clean_data)
class_0 = rep(0,length(clean_data$DAHAK10))
class_1 = rep(0,length(clean_data$DAHAK10))
class_0[which(clean_data$DAHAK10 == 0)] = 1
class_1[which(clean_data$DAHAK10 == 1)] = 1
class = data.frame(class_0, class_1)
train_data = data.frame(clean_data[training,], class[training,1:2])
valid_data = data.frame(clean_data[validation,], class[validation,1:2])


library(neuralnet)
data.df=as.data.frame(clean_data)
library(lattice)
library(neuralnet)
library(ggplot2)
library(caret)
training=sample(row.names(data.df), dim(data.df) [1]*0.6)
validation=setdiff(row.names(data.df), training)
attach(data.df)
class_0 = rep(0, length(DAHAK10))
class_1 = rep(0, length(DAHAK10))
class_0[which(DAHAK10 == 0)] = 1
class_1[which(DAHAK10 == 1)] = 1
class = data.frame (class_0, class_1)
train_data = data.frame (data.df [training,], class [training, 1:2])
valid_data = data.frame (data.df [validation,], class[validation, 1:2])
View(train_data)
as.factor (N.T.M)
as.factor(v.f.s)
as.factor(savad)
as.factor(OTO)
M.LEBAS
nn <- neuralnet (class_0 + class_1 ~ N.T.M + v.f.s + H.KHORAKI.NOOSHIDANI+ H.HAMLONAGHL +
                   H.KALA.MOT +H.ERTEBATAT+  H.BEHDASHT +H.MOBLEMAN + H.MASKAN +
                 + H.POOSHAK +S.Z+ T.O, data = train_data, linear.output = F, hidden = 3)
nn$weights
plot (nn, rep="best")
training.prediction=compute(nn, train_data[,-c(33,34,35)])
training.class=apply(training.prediction$net.result,1,which.max)-1
confusionMatrix(as.factor(training.class), as.factor(data.df[training,]$DAHAK10))
training.prediction=compute(nn, valid_data[,-c(33,34,35)])
training.class=apply(training.prediction$net.result,1,which.max)-1
confusionMatrix(as.factor(training.class),as.factor(data.df[validation,]$DAHAK10))



nn_4hidden <- neuralnet (class_0 + class_1 ~ N.T.M + v.f.s + H.KHORAKI.NOOSHIDANI+ H.HAMLONAGHL +
                   H.KALA.MOT +H.ERTEBATAT+  H.BEHDASHT +H.MOBLEMAN + H.MASKAN +
                   + H.POOSHAK +S.Z+ T.O, data = train_data, linear.output = F, hidden = 4)

nn_4hidden$weights
plot (nn_4hidden, rep="best")
training.prediction=compute(nn_4hidden, train_data[,-c(33,34,35)])
training.class=apply(training.prediction$net.result,1,which.max)-1
confusionMatrix(as.factor(training.class), as.factor(data.df[training,]$DAHAK10))
training.prediction=compute(nn_4hidden, valid_data[,-c(33,34,35)])
training.class=apply(training.prediction$net.result,1,which.max)-1
confusionMatrix(as.factor(training.class),as.factor(data.df[validation,]$DAHAK10))