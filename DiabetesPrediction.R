DATASET_FILENAME  <- "diabetic_data.csv"  # Name of input dataset file
OUTPUT_FIELD      <- "readmitted"             # Field name of the output class to predict
SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage
OUTLIER_CONF      <- 0.9                  # Confidence p-value for outlier detection
TYPE_DISCRETE     <- "DISCRETE"           # field is discrete (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded
DISCRETE_BINS     <- 6                    # Number of empty bins to determine discrete
MAX_LITERALS      <- 55                   # Maximum number of hot encoding new fields
HOLDOUT           <- 70                   # % split to create TRAIN dataset

################################
## Setting up Hyperparameters ##
################################

#HYPERPARAMETERS RANDOM FOREST
Number_of_Trees_Forest = 500
Number_of_Features_Each_Split = 2
KFOLD_RANDOMFOREST = 5

#HYPERPARAMETERS MULTINOMIAL LOGISTIC REGRESSION
KFOLD_MULTINOMIALLOGISTIC = 5

#HYPERPARAMETERS NEURAL NETWORK
KFOLD_NEURALNETWORK = 5
NEURAL_BATCHSIZE = 128 
NEURAL_VALIDATIONSPLIT = 0.2
EPOCHS = 170

#########################
## Installing Packages ##
#########################

# install.packages(c("outliers",
#                    "ggplot2",
#                    "ggpubr",
#                    "corrplot",
#                    "MASS",
#                    "formattable",
#                    "stats",
#                    "caret",
#                    "PerformanceAnalytics",
#                    "outliers",
#                    "corrplot",
#                    "formattable",
#                    "stats",
#                    "caret",
#                    "PerformanceAnalytics",
#                    "stringr",
#                    "partykit",
#                    "C50",
#                    "randomForest",
#                    "keras",
#                    "RColorBrewer",
#                    "pROC"))

#########################
## Importing Libraries ##
#########################
MYLIBRARIES<-c("outliers",
               "ggplot2",
               "ggpubr",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "caret",
               "PerformanceAnalytics",
               "outliers",
               "corrplot",
               "formattable",
               "stats",
               "caret",
               "PerformanceAnalytics",
               "stringr",
               "partykit",
               "C50",
               "randomForest",
               "keras",
               "RColorBrewer",
               "pROC")

###########################################
## User Defined Pre-processing functions ##
###########################################

PreProcess_All_Diagnosis<-function(diabetic_data) {
  for(j in 1:3)
  {
    if(j==1)
    {
      diag = diabetic_data$diag1
    }
    else if(j==2)
    {
      diag = diabetic_data$diag2
    }
    else if(j==3)
    {
      diag = diabetic_data$diag3
    }
    
    for(i in 1:nrow(diabetic_data)) {
      if (diag[i] == "?") {
      }
      else if ((diag[i] >= 390 && diag[i] <= 459) || diag[i] == 785) {
        diag[i] = "Circulatory"
      }
      else if ((diag[i] >= 460 && diag[i] <= 519) || diag[i] == 786) {
        diag[i] = "Respiratory"
      }
      else if ((diag[i] >= 520 && diag[i] <= 579) || diag[i] == 787) {
        diag[i] = "Digestive"
      }
      else if ((diag[i] >= 800 && diag[i] <= 999)) {
        diag[i] = "Injury"
      }
      else if ((diag[i] >= 710 && diag[i] <= 739)) {
        diag[i] = "Musculoskeletal"
      }
      else if ((diag[i] >= 580 && diag[i] <= 629) || diag[i] == 788) {
        diag[i] = "Genitourinary"
      }
      else if ((diag[i] >= 140 && diag[i] <= 239) || diag[i] == 785) {
        diag[i] = "Neoplasms"
      }
      else if (substring(diag[i], 1, 1) == "V" || substring(diag[i], 1, 1) == "E") {
        diag[i] = "Other"
      }
      else if (as.integer(diag[i]) == 250) {
        diag[i] = "Diabetes"
      }
      else {
        diag[i] = "Other"
      }
    }
    if(j==1)
    {
      diabetic_data$diag1 = diag
    }
    else if(j==2)
    {
      diabetic_data$diag2 = diag
    }
    else if(j==3)
    {
      diabetic_data$diag3 = diag
    }
  }
  return (diabetic_data)
}

PreProcess_Age<-function(diabetic_data) {
  
  for(i in 1:nrow(diabetic_data)) {
    if (diabetic_data$age[i] == "[0-10)" || diabetic_data$age[i] == "[10-20)" || diabetic_data$age[i] == "[20-30)") {
      diabetic_data$age[i] = "30 years old or younger"
    }
    else if (diabetic_data$age[i] == "[30-40)" || diabetic_data$age[i] == "[40-50)" || diabetic_data$age[i] == "[50-60)") {
      diabetic_data$age[i] = "30 to 60 years old"
    }
    else if (diabetic_data$age[i] == "?") {
      # Keep it as it is. (Do Nothing)
    }
    else {
      diabetic_data$age[i] = "Older than 60"
    }
  }
  return (diabetic_data)
}

PreProcess_ReAdmitted<-function(diabetic_data) {
  
  for(i in 1:nrow(diabetic_data)) {
    if (diabetic_data$readmitted[i] == "[0-10)" || diabetic_data$age[i] == "[10-20)" || diabetic_data$age[i] == "[20-30)") {
      diabetic_data$age[i] = "30 years old or younger"
    }
    else if (diabetic_data$age[i] == "[30-40)" || diabetic_data$age[i] == "[40-50)" || diabetic_data$age[i] == "[50-60)") {
      diabetic_data$age[i] = "30 to 60 years old"
    }
    else if (diabetic_data$age[i] == "?") {
      # Keep it as it is. (Do Nothing)
    }
    else {
      diabetic_data$age[i] = "Older than 60"
    }
  }
  return (diabetic_data)
}
#**********************************************
####Data Preprocessing plots####
PreprocessPlots <-function(diabetic_data){
  coul <- brewer.pal(5, "Set2")
  plot1<-barplot(table(diabetic_data$readmitted),col=coul,xlab = "Readmitted Status",ylab = "Count",main = "Distribution of readmitted")
  plot2<-ggplot(diabetic_data,aes(x=gender,fill=factor(readmitted)))+geom_bar()+xlab("gender")+ylab("Count")+labs(fill="readmitted")
  plot3<-ggplot(diabetic_data,aes(x=age,fill=factor(readmitted)))+geom_bar()+xlab("age")+ylab("Count")+labs(fill="readmitted")
  plot4<-ggplot(diabetic_data,aes(x=admission_type_id,fill=factor(readmitted)))+geom_bar()+xlab("admission_type_id")+ylab("Count")+labs(fill="readmitted")
  plot5<-ggplot(diabetic_data,aes(x=time_in_hospital,fill=factor(readmitted)))+geom_bar()+xlab("time_in_hospital")+ylab("Count")+labs(fill="readmitted")
  plot6<-ggplot(diabetic_data,aes(x=num_procedures,fill=factor(readmitted)))+geom_bar()+xlab("number of procedures")+ylab("Count")+labs(fill="readmitted")
  plot7<-ggplot(diabetic_data,aes(x=number_diagnoses,fill=factor(readmitted)))+geom_bar()+xlab("number_diagnoses")+ylab("Count")+labs(fill="readmitted")
  
  return(list(plot1,plot2,plot3,plot4,plot5,plot6,plot7))
}

displayFeatureInfo<-function(diabetic_data) {
  for (i in 1: ncol(diabetic_data)) {
    print(names(diabetic_data[i]))
    print(table(diabetic_data[i]))
  }
}

StandardizeANDNormalize<-function(ordinals) {
  #Standardization only for linear, logistic regression as it assumes Gaussian distribution and 
  #use it to reduce multicollinearity.
  zscaled<-as.data.frame(scale(ordinals,center=TRUE, scale=TRUE))
  #print(zscaled)
  #In the chosen classifier, the input values need to be scaled to [0.0,1.0]
  #This is a frame of just the numeric fields, nice and ready for the ML
  ordinalReadyforML<-Nrescaleentireframe(zscaled)
  return (ordinalReadyforML)
}

####################################################
## User Defined Modeling and Evaluation functions ##
####################################################

TrainAndEvaluate_KFold_RandomForest <- function(combinedML, KFOLDS)
{
  #CREATING FOLDS
  folds <- createFolds(y = combinedML[,ncol(combinedML)], k = KFOLDS, list = F)
  combinedML$folds <- folds
  
  accuracies <- c(0)
  precisions1 <- c(0)
  precisions2 <- c(0)
  precisions3 <- c(0)
  recalls1 <- c(0)
  recalls2 <- c(0)
  recalls3 <- c(0)
  fscores1 <- c(0)
  fscores2 <- c(0)
  fscores3 <- c(0)
  aucscores <- c(0)
  
  #Perform KFOLDS fold cross validation
  for(i in 1:KFOLDS) {
    print(paste("Fold: ", i))
    
    #Segment your data by fold using the which() function and use the test and train data 
    #partitions however you desire...
    testIndexes <- which(combinedML$folds==i, arr.ind=TRUE)
    testingData <- combinedML[testIndexes, ]
    trainingData <- combinedML[-testIndexes, ]
    testingInput = subset(testingData, select = -c(readmitted))
    
    # Create a Random Forest model with default parameters
    model1 <- randomForest(as.factor(readmitted) ~ ., data = trainingData, ntree = Number_of_Trees_Forest, mtry = Number_of_Features_Each_Split, importance = TRUE)
    
    # Predicting on test set
    predValid <- predict(model1, testingInput, type = "class")
    
    metrics_val <- CalculateEvaluationMetrics(predValid, testingData$readmitted)
    
    #Combining all accuracies in to one list in order to take mean in the end
    accuracies <- c(accuracies, unlist(metrics_val["accuracy"]))
    precisions1 <- c(precisions1, unlist(metrics_val["precision"])[1])
    precisions2 <- c(precisions2, unlist(metrics_val["precision"])[2])
    precisions3 <- c(precisions3, unlist(metrics_val["precision"])[3])
    recalls1 <- c(recalls1, unlist(metrics_val["recall"])[1])
    recalls2 <- c(recalls2, unlist(metrics_val["recall"])[2])
    recalls3 <- c(recalls3, unlist(metrics_val["recall"])[3])
    fscores1 <- c(fscores1, unlist(metrics_val["f1Score"])[1])
    fscores2 <- c(fscores2, unlist(metrics_val["f1Score"])[2])
    fscores3 <- c(fscores3, unlist(metrics_val["f1Score"])[3])
    aucscores <- c(aucscores, unlist(metrics_val["aucscore"]))
  }
  print(paste("Averaged Accuracy: ", sum(accuracies)/KFOLDS))
  print(paste("Averaged Precision for No Readmission: ", sum(precisions3)/KFOLDS))
  print(paste("Averaged Precision for Readmission before 30 Days: ", sum(precisions1)/KFOLDS))
  print(paste("Averaged Precision for Readmission after 30 Days: ", sum(precisions2)/KFOLDS))
  print(paste("Averaged Recall for No Readmission: ", sum(recalls3)/KFOLDS))
  print(paste("Averaged Recall for Readmission before 30 Days: ", sum(recalls1)/KFOLDS))
  print(paste("Averaged Recall for Readmission after 30 Days: ", sum(recalls2)/KFOLDS))
  print(paste("Averaged F1-Score for No Readmission: ", sum(fscores3)/KFOLDS))
  print(paste("Averaged F1-Score for Readmission before 30 Days: ", sum(fscores1)/KFOLDS))
  print(paste("Averaged F1-Score for Readmission after 30 days: ", sum(fscores2)/KFOLDS))
  print(paste("Averaged AUC Score across all Folds: ", sum(aucscores)/KFOLDS))
}

TrainAndEvaluate_KFold_MultinomialLogisticRegression<- function(combinedML, KFOLDS) {
  #CREATING FOLDS
  folds <- createFolds(y = combinedML[,ncol(combinedML)], k = KFOLDS, list = F)
  
  combinedML$folds <- folds

  accuracies <- c(0)
  precisions1 <- c(0)
  precisions2 <- c(0)
  precisions3 <- c(0)
  recalls1 <- c(0)
  recalls2 <- c(0)
  recalls3 <- c(0)
  fscores1 <- c(0)
  fscores2 <- c(0)
  fscores3 <- c(0)
  aucscores <- c(0)
  
  #Perform KFOLDS fold cross validation
  for(i in 1:KFOLDS) {
    print(paste("Fold: ", i))
    testIndexes <- which(combinedML$folds==i, arr.ind=TRUE)
    testingData <- combinedML[testIndexes, ]
    trainingData <- combinedML[-testIndexes, ]
    testingInput = subset(testingData, select = -c(readmitted))
    
    # Fit the model
    model <- nnet::multinom(readmitted ~ ., data = trainingData, method="logistic", maxit = 1e3)
    
    # Make predictions
    predicted <- model %>% predict(testingInput)
    
    metrics_val <- CalculateEvaluationMetrics(predicted, testingData$readmitted)
    
    #Combining all accuracies in to one list in order to take mean in the end
    accuracies <- c(accuracies, unlist(metrics_val["accuracy"]))
    precisions1 <- c(precisions1, unlist(metrics_val["precision"])[1])
    precisions2 <- c(precisions2, unlist(metrics_val["precision"])[2])
    precisions3 <- c(precisions3, unlist(metrics_val["precision"])[3])
    recalls1 <- c(recalls1, unlist(metrics_val["recall"])[1])
    recalls2 <- c(recalls2, unlist(metrics_val["recall"])[2])
    recalls3 <- c(recalls3, unlist(metrics_val["recall"])[3])
    fscores1 <- c(fscores1, unlist(metrics_val["f1Score"])[1])
    fscores2 <- c(fscores2, unlist(metrics_val["f1Score"])[2])
    fscores3 <- c(fscores3, unlist(metrics_val["f1Score"])[3])
    aucscores <- c(aucscores, unlist(metrics_val["aucscore"]))
  }
  
  print(paste("Averaged Accuracy: ", sum(accuracies)/KFOLDS))
  print(paste("Averaged Precision for No Readmission: ", sum(precisions1)/KFOLDS))
  print(paste("Averaged Precision for Readmission before 30 Days: ", sum(precisions2)/KFOLDS))
  print(paste("Averaged Precision for Readmission after 30 Days: ", sum(precisions3)/KFOLDS))
  print(paste("Averaged Recall for No Readmission: ", sum(recalls1)/KFOLDS))
  print(paste("Averaged Recall for Readmission before 30 Days: ", sum(recalls2)/KFOLDS))
  print(paste("Averaged Recall for Readmission after 30 Days: ", sum(recalls3)/KFOLDS))
  print(paste("Averaged F1-Score for No Readmission: ", sum(fscores1)/KFOLDS))
  print(paste("Averaged F1-Score for Readmission before 30 Days: ", sum(fscores2)/KFOLDS))
  print(paste("Averaged F1-Score for Readmission after 30 days: ", sum(fscores3)/KFOLDS))
  print(paste("Averaged AUC Score across all Folds: ", sum(aucscores)/KFOLDS))
}


TrainAndEvaluate_KFold_NeuralNetwork <- function(combinedML, KFOLDS, BATCH_SIZE, VALIDATION_SPLIT) {
  #CREATING FOLDS
  folds <- createFolds(y = combinedML[,ncol(combinedML)], k = KFOLDS, list = F)
  combinedML$folds <- folds
  
  accuracies <- c(0)
  precisions1 <- c(0)
  precisions2 <- c(0)
  precisions3 <- c(0)
  recalls1 <- c(0)
  recalls2 <- c(0)
  recalls3 <- c(0)
  fscores1 <- c(0)
  fscores2 <- c(0)
  fscores3 <- c(0)
  aucscores <- c(0)
  
  #Perform KFOLDS fold cross validation
  for(i in 1:KFOLDS) {
    print(paste("Fold: ", i))
    
    #Segment your data by fold using the which() function and use the test and train data 
    #partitions however you desire...
    testIndexes <- which(combinedML$folds==i, arr.ind=TRUE)
    testingData <- combinedML[testIndexes, ]
    trainingData <- combinedML[-testIndexes, ]
    
    #Change to matrix
    trainingData <- as.matrix(trainingData)
    testingData <- as.matrix(testingData)
    dimnames(trainingData) <- NULL
    dimnames(testingData) <- NULL
    
    #Separating independent and dependent variables
    trainingInput <- trainingData[, 1:(ncol(combinedML)-2)]
    training_output <- trainingData[, (ncol(combinedML)-1)]
    testInput <- testingData[, 1:(ncol(combinedML)-2)]
    test_output <- testingData[, (ncol(combinedML)-1)]
    
    #print(names(combinedML))
    #print(table(training_output))
    
    #One hot encode the dependent variable
    training_output = matrix(as.numeric(training_output))
    training_output = training_output - 1
    traininglabels <- to_categorical(training_output, 3)
    test_output = matrix(as.numeric(test_output))
    test_output = test_output - 1
    testlabels <- to_categorical(test_output, 3)
    
    num_features = ncol(trainingInput)
    
    # Create sequential model
    model <- keras_model_sequential()
    model %>%
      layer_dense(units = 8, activation = 'relu', input_shape = c(num_features)) %>%
      layer_dense(units = 6, activation = 'relu') %>%
      layer_dense(units = 4, activation = 'relu') %>%
      layer_dense(units = 3, activation = 'sigmoid')
    summary(model)
    
    # Compile
    model %>%
      compile(loss = 'categorical_crossentropy',
              optimizer = 'rmsprop',
              metrics = 'accuracy')
    
    history <- model %>%
      fit(trainingInput,
          traininglabels,
          epoch = EPOCHS,
          batch_size = BATCH_SIZE,
          validation_data = list(testInput, testlabels))
    plot(history)
    
    #Yielding predictions
    pred <- model %>%
      predict_classes(testInput)
    
    #Combining all accuracies into one list in order to take mean in the end
    metrics_val <- CalculateEvaluationMetrics(pred, as.factor(test_output))
    
    #Combining all accuracies in to one list in order to take mean in the end
    accuracies <- c(accuracies, unlist(metrics_val["accuracy"]))
    precisions1 <- c(precisions1, unlist(metrics_val["precision"])[1])
    precisions2 <- c(precisions2, unlist(metrics_val["precision"])[2])
    precisions3 <- c(precisions3, unlist(metrics_val["precision"])[3])
    recalls1 <- c(recalls1, unlist(metrics_val["recall"])[1])
    recalls2 <- c(recalls2, unlist(metrics_val["recall"])[2])
    recalls3 <- c(recalls3, unlist(metrics_val["recall"])[3])
    fscores1 <- c(fscores1, unlist(metrics_val["f1Score"])[1])
    fscores2 <- c(fscores2, unlist(metrics_val["f1Score"])[2])
    fscores3 <- c(fscores3, unlist(metrics_val["f1Score"])[3])
    aucscores <- c(aucscores, unlist(metrics_val["aucscore"]))
  }
  
  print(paste("Averaged Accuracy: ", sum(accuracies)/KFOLDS))
  print(paste("Averaged Precision for No Readmission: ", sum(precisions1)/KFOLDS))
  print(paste("Averaged Precision for Readmission before 30 Days: ", sum(precisions2)/KFOLDS))
  print(paste("Averaged Precision for Readmission after 30 Days: ", sum(precisions3)/KFOLDS))
  print(paste("Averaged Recall for No Readmission: ", sum(recalls1)/KFOLDS))
  print(paste("Averaged Recall for Readmission before 30 Days: ", sum(recalls2)/KFOLDS))
  print(paste("Averaged Recall for Readmission after 30 Days: ", sum(recalls3)/KFOLDS))
  print(paste("Averaged F1-Score for No Readmission: ", sum(fscores1)/KFOLDS))
  print(paste("Averaged F1-Score for Readmission before 30 Days: ", sum(fscores2)/KFOLDS))
  print(paste("Averaged F1-Score for Readmission after 30 days: ", sum(fscores3)/KFOLDS))
  print(paste("Averaged AUC Score across all Folds: ", sum(aucscores)/KFOLDS))
}

encode_ordinal <- function(x, order) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  return (x)
}

CalculateEvaluationMetrics<-function(predicted, actual_output) {
  #To grab precision for a particular class (precision.class1 <- precision["class1"]). To grab recall for a particular class (recall.virginica <- recall["virginica"]) print(dim(table1))
  table1 <- table(Predicted = predicted, Actual = actual_output)
  print(table1)
  
  accuracy <- sum(diag(table1)) / sum(table1)
  precision <- diag(table1) / rowSums(table1)
  recall <- (diag(table1) / colSums(table1))
  f1Score <- (2*((precision*recall)/(precision+recall)))
  roc.multi <- multiclass.roc(actual_output, as.numeric(predicted))
  
  print(paste("Accuracy: ", accuracy))
  print("Precision: ")
  print(precision)
  print("Recall: ")
  print(recall)
  print("F1Score: ")
  print(f1Score)
  print("AUC Score: ")
  print(roc.multi$auc)
  
  #Plotting ROC curves
  rs <- roc.multi[['rocs']]
  plot.roc(rs[[1]])
  sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
  
  metricslist <- list("accuracy" = accuracy, "precision" = precision, "recall" = recall, "f1Score" = f1Score, "aucscore" = roc.multi$auc)
  return (metricslist)
}

#################MAIN FUNCTION STARTS HERE##############

main<-function() {
  print("Inside main function")

  #install_keras()
  setwd("E:/UK DRIVE/Courses/Practical Business Analytics/Project/PBAProject")
  allResults<-NULL 
  
  diabetic_data<-NreadDataset(DATASET_FILENAME)

  # Extract records for the AfricanAmerican race 
  positions<-which(diabetic_data$race == 'AfricanAmerican')
  diabetic_data<-diabetic_data[positions,]
  
  #Removing punctuation from column names
  names(diabetic_data) <- gsub("[_]", "", names(diabetic_data)) 
  
  # Dropping features that are unimportant or have too many missing values
  diabetic_data = subset(diabetic_data, select = c(age, dischargedispositionid, diag1, diag2, diag3, numberoutpatient, numberinpatient, timeinhospital, maxgluserum, numlabprocedures, A1Cresult, readmitted))
  
  #Preprocessing some features
  diabetic_data <- PreProcess_All_Diagnosis(diabetic_data)
  diabetic_data <- PreProcess_Age(diabetic_data)
  
  #Visualising features 
  PreprocessPlots(diabetic_data)

  # Checking number of features remaining after filtering out 
  print("Number of records: ")
  print(ncol(diabetic_data))
  
  # FIRST STEP IS TO GET AN OVERVIEW OF THE DATASET
  NPREPROCESSING_prettyDataset(diabetic_data)
  
  # SECOND STEP IS TO KNOW FIELD TYPES
  field_types1<-NPREPROCESSING_initialFieldType(diabetic_data)
  numeric_fields<-names(diabetic_data)[field_types1=="NUMERIC"]
  print(paste("NUMERIC FIELDS=",length(numeric_fields)))
  print(numeric_fields)
  symbolic_fields<-names(diabetic_data)[field_types1=="SYMBOLIC"]
  print(paste("SYMBOLIC FIELDS=",length(symbolic_fields)))
  print(symbolic_fields)
  
  # Determine the discrete numeric fields from the numeric fields. 
  # Adding on other field types namely discrete numeric fields and gathering all to field_types2
  field_types2<-NPREPROCESSING_discreteNumeric(dataset=diabetic_data,
                                               field_types=field_types1,
                                               cutoff=DISCRETE_BINS)
  print(field_types2)
  
  results<-data.frame(field=names(diabetic_data),initial=field_types1,types2=field_types2)
  print(formattable::formattable(results))
  
  # SEPERATING DISCRETE AND ORDINAL VARIABLES
  # This is a sub-set frame of just the discrete fields
  discrete<-diabetic_data[,which(field_types2==TYPE_DISCRETE)]
  #print("Below are the discrete values")
  #print(discrete)
  
  # This is a sub-set frame of just the ordinal fields
  ordinals<-diabetic_data[,which(field_types2==TYPE_ORDINAL)]
  #print("Below are the ordinals")
  #print(ordinals)
  
  #Third step is to remove outliers from ordinals(continuous variables) and replace them with means
  #Discrete ordinal, continuous ordinal(No significant distancing b/w values)
  ordinals<-NPREPROCESSING_outlier(ordinals=ordinals,confidence=OUTLIER_CONF)

  #Standardize and Normalize Ordinal Variables 
  #(Standardizing reduces multicollinearity)(Reducing scale speeds up convergence)(Eliminate feature bias)
  ordinalReadyforML<-StandardizeANDNormalize(ordinals)
  #print(ordinalReadyforML)
  #print(names(diabetic_data[,!names(diabetic_data) %in% c("readmitted")]))
  
  #Process the categorical (symbolic/discrete) fields using 1-hot-encoding excluding the readmitted output variable
  categoricalReadyforML<-NPREPROCESSING_categorical(dataset=diabetic_data[,!names(diabetic_data) %in% c("readmitted")],field_types=field_types2)
  #print(formattable::formattable(data.frame(fields=names(categoricalReadyforML))))

  #Integer encoding the output variable
  categoricalReadyforML$readmitted = encode_ordinal(diabetic_data$readmitted, order = c("NO", "<30", ">30"))

  # How many fields were generated through the 1-hot-encoding process
  nonNumericbefore<-length(which(field_types2!=TYPE_ORDINAL))
  print(nonNumericbefore)
  nonNumerictranformed<-ncol(categoricalReadyforML)
  print(paste("Symbolic fields. Before encoding=",nonNumericbefore,"After",nonNumerictranformed))
  
  # Output the names of the encoded fields (literals)
  print(formattable::formattable(data.frame(field=1:nonNumerictranformed,encoded=names(categoricalReadyforML))))
  
  # Combine the two sets of data that are ready for ML
  combinedML<-cbind(ordinalReadyforML,categoricalReadyforML)
  
  # Are any of the fields redundant?  
  # This will also be checking for multicollinearity using co-relation matrix and remove redundant ones (high correlation between variables)
  combinedML<-NPREPROCESSING_redundantFields(dataset=combinedML,cutoff=OUTLIER_CONF)
  
  # Randomize the entire data set - sample() is an easy way to do this!
  combinedML<-combinedML[sample(nrow(combinedML)),]
  
  # Getting an overview of the changes
  NPREPROCESSING_prettyDataset(combinedML)

  #TRAINING THE MODELS
  print("KFoldStratified_MultinomialLogisticRegression: ")
  TrainAndEvaluate_KFold_MultinomialLogisticRegression(combinedML, KFOLD_MULTINOMIALLOGISTIC)
  
  #print("KFoldStratified_NeuralNetwork Training: ")
  #TrainAndEvaluate_KFold_NeuralNetwork(combinedML, KFOLD_NEURALNETWORK, NEURAL_BATCHSIZE, NEURAL_VALIDATIONSPLIT)
  
  #print("KFoldStratified_RandomForest Training: ")
  #TrainAndEvaluate_KFold_RandomForest(diabetic_data, KFOLD_RANDOMFOREST)
}



# Program Starts Here
# Automatically release memory
gc()

# Tries to clear plots and other graphics in RStudio output
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the RStudio console area
cat("\014")

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

debugSource("lab3DataPrep.R")
debugSource("4labFunctions.R")

set.seed(123)

main()

print("end")