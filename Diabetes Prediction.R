# Install required Packages
install.packages("caret")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("GGally")
install.packages("mlbench")
install.packages("e1071")
install.packages("kernlab")
install.packages("DataExplorer")
install.packages("plotly")
install.packages("VIM")
install.packages("pROC")


# Load the installed libraries
library(caret)
library(tidyverse)
library(corrplot)
library(gridExtra)
library(GGally)
library(mlbench)
library(e1071)
library(kernlab)
library(DataExplorer)
library(plotly)
library(VIM)
library(class)
library(knitr)
library(pROC)

# Import dataset, convert column with strings to Factor and view
diabetes <- read.csv("diabetes-dataset.csv", stringsAsFactors = TRUE, header = TRUE)

# Understanding Dataset
str(diabetes)
summary(diabetes)
view(diabetes)
head(diabetes)
tail(diabetes)
unique(diabetes)
glimpse(diabetes)
names(diabetes)

# Diabetes Outcome summary for diabetic or non-diabetic patients
table(diabetes$Outcome) %>% 
  view() 

# Convert Outcome column to a factor
diabetes$Outcome <- as.factor(diabetes$Outcome)

# Plot pie chart to visualise frequency of Diabetes outcome
diabetes %>%
  count(Outcome) %>%  # Count the frequency of each outcome
  ggplot(aes(x = "", y = n, fill = Outcome)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of diabetes Outcome Category") +
  scale_fill_manual(values = c("forestgreen", "firebrick")) +
  theme_void() +
  geom_text(aes(label = paste0(round((n/sum(n)) * 100), "%")), 
            position = position_stack(vjust = 0.5))

# Reshape data format from wide to long for graph plotting
diab_long <-diabetes %>% gather(attributes, value, 1:8)


# Visualize all columns with Line plot
diab_long %>% 
  ggplot(aes(value))+
  geom_line(stat = "density", aes(color=attributes), linewidth = 1)+
  facet_wrap(~attributes, scales = "free_x")+
  labs(x="Values", y="Frequency", title="Density plot showing distribution of Diabetes dataset variables") +
  theme_bw()

# Visualize all columns with histogram
diab_long %>% 
  ggplot(aes(value))+
  geom_histogram(fill = "#FF007F", colour="black", show.legend = FALSE)+
  facet_wrap(~attributes, scales = "free_x")+
  labs(x="Values", y="Frequency", title="Histograms showing frequency of Diabetes dataset variables") +
  theme_bw()

# Plot Age spread and frequency of patients
diabetes %>%
  ggplot(aes(x = Age, fill = "Age")) +
  geom_histogram(color = "black", bins = 30) +  # Adjust bins as needed
  labs(title = "Histogram showing patients Age frequency", x = "Age", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = "lightseagreen")

# Visualize Outcomes by number of pregnancies 
diabetes %>% 
  ggplot(aes(x = Pregnancies, fill = Outcome)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Bar Plot of Pregnancies by Outcome", x = "Pregnancies", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "forestgreen", "1" = "firebrick"))

# Visualize Outcomes by number of Ages
diabetes %>%
  ggplot(aes(x = Age, fill = Outcome)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Bar Plot of Ages by Outcome", x = "Age", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "forestgreen", "1" = "firebrick"))

# DATA PREPROCESSING
# Data cleaning using feature selection
# Check for NaNs
anyNA(diabetes)
missing_data <- colSums(is.na(diabetes)/nrow(diabetes))
round(missing_data, 2)

# Identify all rows with zeros and assign to zeros
zeros <- diabetes %>% 
  filter(Glucose %in% c(0) | BloodPressure %in% c(0) | SkinThickness %in% c(0) | Insulin %in% c(0) | BMI %in% c(0))

# Check the total number of rows that have zeros
zeros %>% 
  summarise(count=n())

# Create a simple bar plot to visualize the total rows with zeros
zeros %>%
  summarise(total_rows_with_zeros = sum(count=n())) %>%
  ggplot(aes(x = "", y = total_rows_with_zeros, fill = "Total Rows with Zeros")) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "", y = "Total Rows with Zeros", title = "Total Rows with Zeros in Dataframe") +
  theme_minimal()

# Check each column for the total number of rows with zeros 
zeros_count <- diabetes %>%
  summarise(
    Glucose_zeros = sum(Glucose == 0),
    BloodPressure_zeros = sum(BloodPressure == 0),
    SkinThickness_zeros = sum(SkinThickness == 0),
    Insulin_zeros = sum(Insulin == 0),
    BMI_zeros = sum(BMI == 0)
  )

# Create a simple bar plot to visualize the total rows with zeros for each column
zeros_count %>% 
  pivot_longer(everything(), names_to = "Variable", values_to = "Count") %>%
  ggplot(aes(x = Variable, y = Count, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Variables", y = "Count of Zeros", title = "Count of Zeros in Each Variable")+
  theme_minimal()

# Convert zeros to NA
diabetes <- mutate(diabetes,
                   Glucose = ifelse(Glucose == 0, NA, Glucose),
                   BloodPressure = ifelse(BloodPressure == 0, NA, BloodPressure),
                   BMI = ifelse(BMI == 0, NA, BMI),
                   SkinThickness = ifelse(SkinThickness == 0, NA, SkinThickness),
                   Insulin = ifelse(Insulin == 0, NA, Insulin),
)

# Using feature selection view proportion of null values per column
missing_data <- colSums(is.na(diabetes)/nrow(diabetes))
round(missing_data, 2) %>% 
  view()

# Replace all NAs by mean
diabetes <- mutate(diabetes,
                   Glucose = ifelse(is.na(Glucose), mean(Glucose, na.rm = TRUE), Glucose),
                   BloodPressure = ifelse(is.na(BloodPressure), mean(BloodPressure, na.rm = TRUE), BloodPressure),
                   BMI = ifelse(is.na(BMI), mean(BMI, na.rm = TRUE), BMI),
                   SkinThickness = ifelse(is.na(SkinThickness), mean(SkinThickness, na.rm = TRUE), SkinThickness),
                   Insulin = ifelse(is.na(Insulin), mean(Insulin, na.rm = TRUE), Insulin),
)

# Confirm if there are any NA
anyNA(diabetes)

# Transformation: Reshape data format from wide to long for visualization
diab_long <-diabetes %>% gather(attributes, value, 1:8)

# Explorative Data Analysis (EDA)
# Visualize all columns with boxplot and density plot
# Boxplot of pregnancy by outcome
ggplot(diabetes, aes(x=Pregnancies, y=Outcome))+
  geom_boxplot(fill = "skyblue")+
  labs(title = "Summary of Pregnancies for both outcomes")
# Desity plot for pregnancy by outcome
ggplot(diabetes, aes(Pregnancies, fill = Outcome))+
  geom_density(alpha = 0.5)+
  labs(title = "Density of Pregnancies for both outcomes")+
  scale_fill_manual(values = c("0" = "forestgreen", "1" = "firebrick"))

# Boxplot of Glucose by outcome
ggplot(diabetes, aes(Glucose, Outcome))+
  geom_boxplot(fill = "skyblue")+
  labs(title = "Summary of Glucose for both outcomes")
# Desity plot for Glucose by outcome
ggplot(diabetes, aes(Glucose, fill = Outcome))+
  geom_density(alpha = 0.5)+
  labs(title = "Density of Glucose for both outcomes")+
  scale_fill_manual(values = c("0" = "forestgreen", "1" = "firebrick"))

# Boxplot of Blood Pressure by outcome
ggplot(diabetes, aes(BloodPressure, Outcome))+
  geom_boxplot(fill = "skyblue")+
  labs(title = "Summary of Blood Pressure for both outcomes")
# Desity plot for Blood Pressure by outcome
ggplot(diabetes, aes(BloodPressure, fill = Outcome))+
  geom_density(alpha = 0.5)+
  labs(title = "Density of Blood Pressure for both outcomes")+
  scale_fill_manual(values = c("0" = "forestgreen", "1" = "firebrick"))

# Boxplot of Skin Thickness by outcome
ggplot(diabetes, aes(SkinThickness, Outcome))+
  geom_boxplot(fill = "skyblue")+
  labs(title = "Summary of Skin Thickness for both outcomes")

# Boxplot of Insulin by outcome
ggplot(diabetes, aes(Insulin, Outcome))+
  geom_boxplot(fill = "skyblue")+
  labs(title = "Summary of Insulin for both outcomes")

# Boxplot of BMI by outcome
ggplot(diabetes, aes(BMI, Outcome))+
  geom_boxplot(fill = "skyblue")+
  labs(title = "Summary of BMI for both outcomes")

# Boxplot of Diabetes Pedigree Function by outcome
ggplot(diabetes, aes(DiabetesPedigreeFunction, Outcome))+
  geom_boxplot(fill = "skyblue")+
  labs(title = "Summary of Diabetes Pedigree Function for both outcomes")

# Boxplot of Age by outcome
ggplot(diabetes, aes(Age, Outcome))+
  geom_boxplot(fill = "skyblue")+
  labs(title = "Summary of Age for both outcomes")

# Statistics of the Glucose
cat("The mean Glucose is", mean(diabetes$Glucose), "\n")
cat("The Glucose variance is", var(diabetes$Glucose), "\n")
cat("The range of Glucose is", range(diabetes$Glucose), "\n")
cat("The standard deviation for Glucose taken is", sd(diabetes$Glucose), "\n")


# Data  Preparation
# Convert all data to numeric
diab_num <- diabetes %>% 
  mutate(
    Pregnancies = as.numeric(Pregnancies),
    Glucose = as.numeric(Glucose),
    BloodPressure = as.numeric(BloodPressure),
    SkinThickness= as.numeric(SkinThickness),
    Insulin= as.numeric(Insulin),
    Age = as.numeric(Age),
    Outcome = as.numeric(Outcome)
  )

str(diab_num)

# Data Correlation using Pearson Correlation Matrix
corr_mat <- cor(diab_num)

# Print Correlation matrix
print(corr_mat)

# Plot correlation matrix
corrplot(corr_mat,
         method = "circle",
         type = "lower",outline = T,
         addgrid.col = "darkgray",order="hclust",
         mar = c(0,0,0,4),addrect = 4,
         rect.col = "black", rect.lwd = 5,
         cl.pos = "b", tl.col = "red",
         tl.cex =0.5, cl.cex = 0.5)

# Find attributes that are most correlated
highlyCorrelated <- findCorrelation(corr_mat,
                                    cutoff = .65,
                                    verbose = TRUE,
                                    names = TRUE)
# Create a dataframe
highlyCorrelated <- data.frame(highlyCorrelated)
# View highly correlated columns
highlyCorrelated

# Convert Outcome from numeric to factor to prepare for scaling
# Diabetes = 2, No diabetes = 1
diab_num$Outcome <- as.factor(as.character(diab_num$Outcome))

# Show dataset summary
summary(diab_num)
summary(diabetes)
str(diabetes)

# Feature scaling (Normalization)
# Center and scale model creation
model <-preProcess(diab_num[,c(1:9)],
                method=c("center","scale"))
# Use model to generate predicted values for the dataset
diab_num <- predict(model, diab_num[,c(1:9)])

# View dataset
head(diab_num)

# Original data
diab_num %>% 
  ggplot(diab_num, mapping = aes(x = Outcome, fill = Outcome)) +
  geom_bar(colour="blue",) +
  labs(title="Diabetes Outcome proportion") +
  theme_bw() +
  scale_fill_manual(values = c("1" = "forestgreen", "2" = "firebrick"))

# Reshape data format from wide to long for graph plotting
diab_long <-diab_num %>% gather(attributes, value, 1:8)

# Visualize all columns of preprocessed data with Line plot
diab_long %>% 
  ggplot(aes(value))+
  geom_line(stat = "density", aes(color=attributes), linewidth = 1)+
  facet_wrap(~attributes, scales = "free_x")+
  labs(x="Values", y="Frequency", title="Density plot showing distribution of Preprocessed dataset") +
  theme_bw()

# Visualize all columns of preprocessed data with histogram
diab_long %>% 
  ggplot(aes(value))+
  geom_histogram(fill = "#FF007F", colour="black", show.legend = FALSE)+
  facet_wrap(~attributes, scales = "free_x")+
  labs(x="Values", y="Frequency", title="Histograms showing frequency of Preprocessed Dataset") +
  theme_bw()

# Data Splicing for training and testing
# Set seed to ensure reproducibility
set.seed(123)
# Create 70 percent partitioning
intrain<- createDataPartition(y = diab_num$Outcome, p = 0.7, list = FALSE)
train.diab <- diab_num[intrain,]
test.diab <- diab_num[-intrain,]
# Check dimensions of training and testing data frame
dim(train.diab)
dim(test.diab)

# Calculate frequencies of categories in training and test data
train_freq <- table(train.diab$Outcome)
test_freq <- table(test.diab$Outcome)
# Combine frequencies from both datasets
combined_freq <- cbind(train_freq, test_freq)

# Create a bar plot for both training and test data frequencies
barplot(combined_freq, beside = TRUE,
        main = "Frequency of Partitioned Training and Test Data",
        xlab = "Category",
        ylab = "Frequency",
        col = c("forestgreen", "firebrick"),
        legend.text = c("Non-diabetic", "Diabetic"),
        args.legend = list(x = "topright"))


# MACHINE LEARNING
# Create train control
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Train dataframe using traincontrol
svm_Linear <- train(Outcome ~., data = train.diab, method = "svmLinear",trControl=trctrl, tuneLength = 10)
svm_Linear

# Predict outcome classes of test dataset
test_pred <- predict(svm_Linear, newdata = test.diab)
test_pred

# Check accuracy of model using confusion matrix
confusionMatrix(table(test_pred, test.diab$Outcome))

# Build svmLinear classifier
set.seed(123)
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(Outcome ~., data = train.diab, method = "svmLinear",trControl=trctrl,tuneGrid = grid,tuneLength = 10)
svm_Linear_Grid

# Plot accuracy based on classifiers
plot(svm_Linear_Grid)

# Compute ROC curve
roc_svm <- roc(test.diab$Outcome, as.numeric(test_pred))
# Show ROC AUC
roc_svm

# Plot ROC curve
plot(roc_svm, col = "firebrick", main = "SVM ROC Curve",
     xlab = "False Positive Rate (Specificity)",
     ylab = "True Positive Rate (Sensitivity)")

# KNN Model training
#train the KNN model
set.seed(123)
ml.knn <- train(Outcome~., data=train.diab,
                 method="knn", preProcess = c("center", "scale"),
                 trControl=trctrl)
ml.knn

knn_predict <- predict(ml.knn, newdata = test.diab)

# Check accuracy of model using confusion matrix
confusionMatrix(table(knn_predict, test.diab$Outcome))

# Compute ROC curve
roc_knn <- roc(test.diab$Outcome, as.numeric(knn_predict))
# Show ROC AUC
roc_knn

# Plot ROC curve
plot(roc_knn, col = "firebrick", main = "KNN ROC Curve",
     xlab = "False Positive Rate (Specificity)",
     ylab = "True Positive Rate (Sensitivity)")


# Compare Algorithm
diab_ML_comparison <- resamples(list(SVM=svm_Linear, KNN=ml.knn))
summary(diab_ML_comparison)

# Plot comparing
dotplot(diab_ML_comparison)
