#Import Data
dataset = read.csv('ap.csv',header = TRUE)

#view(dataset)
df = dataset                           # Original Copy

#Exploring the Dataset
str(df)

#Dimension of the data frame
cat("Dimension of the data frame:")
dim(df)

# Time to convert these variables to FACTOR type for easy manipulation
df$University.Rating = as.factor(df$University.Rating)
df$SOP = as.factor(df$SOP)
df$LOR = as.factor(df$LOR)
df$Research = as.factor(df$Research)


#Data Split for Training and Test set

require(caTools)
set.seed(2019)                                    #to fix random value
split = sample.split(df$Chance.of.Admit, SplitRatio = 0.8)
df_train = subset(df, split ==TRUE)
df_test = subset(df, split ==FALSE)

cat(paste("No of rows in training set:", nrow(df_train)),"\n")
cat(paste("No of rows in testing set:", nrow(df_test)))


#Training of Model
library(rpart)
model <- rpart(Chance.of.Admit ~ ., data = df_train)


# Predicting the result having with test data using model MLR1
test_result = predict(model, newdata = df_test)

#Compare the actual test result with predicted test result side by side
result = data.frame(df_test$Chance.of.Admit, test_result)
head(result)


#Plot decision tree
library(rpart.plot)
prp(model,box.palette = "YlGnBl", tweak = 1.2)

