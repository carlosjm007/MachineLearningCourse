library(caret);

####################################################
# In this part, we are going to load the data from "http://groupware.les.inf.puc-rio.br/har"
# they offered this information about personal activity taken from his devices; in this case,
# the data comes from people who made barbell lifts correctly and incorrectly in 5 different ways.
#
# Our target is to predict the manner in which they did the exercise.

training <- read.csv("pml-training.csv");
testing <- read.csv("pml-testing.csv");


###################################################
# We will clean data from the variables where exist more than 50% na records and we will delete
# columns where we'll cannot find variability through the rows

i_nzv <- nearZeroVar(training, saveMetrics=TRUE);
training <- training[, !i_nzv$nzv];
i_nrows <- nrow(training);
i_names_columns <- c();
i_dataRmNA <- c();
for(i in names(training)){
	if (i != "classe"){
		dataRmNA <- sum(is.na(training[,c(i)]));
		if (!(dataRmNA > (i_nrows*0.5))){
			i_names_columns <- c(i_names_columns, i);
		}
	}
};

training <- training[, c(i_names_columns,"classe")];
testing <- testing[, i_names_columns];
###################################################
# To do tests, We are going to split the data
inTrain <- createDataPartition(y=training$classe, p=0.75, list=FALSE);

i_training <- training[inTrain,];
i_testing <- training[-inTrain,];

#################################################
#
modelFit <- train(classe ~., data=i_training, method = "svm");

rfPredictionsTesting <- predict(modelFit, newdata = testing, class = "class")