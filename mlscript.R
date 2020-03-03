#i#nstall.packages("caret", dependencies=c("Depends", "Suggests"))
library(caret)

#150 observations of iris flowers
#5th col is the species of the flower observed

#https://en.wikipedia.org/wiki/Iris_flower_data_set

##1.load the iris data
#attach the iris dataset to the environment
data(iris)

#renaming the dataset
dataset <- iris

#creating a validation dataset
#by splitting using a 80/20 split

#creating a list of 80% of the rows in the original dataset we can use for train

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,] #sets the 80% as dataset

#2. Summarising Dataset
dim(dataset) #120 instances and 5 attributes

#types of attributes
sapply(dataset, class)

head(dataset)

levels(dataset$Species)

#class distribution
percentage <- prop.table(table(dataset$Species))*100
cbind(freq=table(dataset$Species),percentage=percentage)

#statistical summary
summary(dataset)

#Visualizations
#split input and output
x <- dataset[,1:4]
y <- dataset[,5]

#boxplot for each attribute
par(mfrow=c(1,4))
  for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
  }

#barplot for class breakdown
plot(y)

#multivariate plots
#scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")

#box and whisker plts for each attribute
featurePlot(x=x, y=y, plot="box"
            
            # density plots for each attribute by class value
            scales <- list(x=list(relation="free"), y=list(relation="free"))
            featurePlot(x=x, y=y, plot="density", scales=scales)
            
            #evaluate some algorithms
            #Set-up the test harness to use 10-fold cross validation.
            #Build 5 different models to predict species from flower measurements
            #Select the best model.
            
            #Test Harness, using 10-fold cross validation
            control <- trainControl(method = "cv", number=10)
            metric <- "Accuracy" #using this to evaluate models
            
            #Build Models
            #5 diff ones: LDA, CART, KNN, SVM, RF
            #mixture of simple linear(LDA), noninear (CART, kNN) and complex nonlinear methods (SVM, RF)
            
            
            # a) linear algorithms
            set.seed(7)
            fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
            # b) nonlinear algorithms
            # CART
            set.seed(7)
            fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
            # kNN
            set.seed(7)
            fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
            # c) advanced algorithms
            # SVM
            set.seed(7)
            fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
            # Random Forest
            set.seed(7)
            fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)
            
            #selecting best model
            #summrize accuracy of models
            results <- resamples(list(lda=fit.lda, cartfit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
            summary(results)
            
            #ploting the model evaluation results, to compare spread and mean accuracy of each model
            dotplot(results)
            #LDA seems the best
            
            #summarize best model. gives the mean and SD accuracy achieved, specifically 97.5% accuracy +/ 4%
            
            #making predictions
            #estimate skill of LDA on the validation dataset
            predictions <- predict(fit.lda, validation)
            confusionMatrix(predictions, validation$Species)
            #within our expected margin of 97% +/-4%, suggesting am accurate and a reliably accurate model
print(fit.lda)
            
            
            
