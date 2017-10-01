library(caret);
library(rpart);
library(rpart.plot);
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
		if (i != "X"){
			dataRmNA <- sum(is.na(training[,c(i)]));
			if (!(dataRmNA > (i_nrows*0.5))){
				i_names_columns <- c(i_names_columns, i);
			}
		}
	}
};


training <- training[, c(i_names_columns,"classe")];
testing <- testing[, i_names_columns];
###################################################
# To do tests, We are going to split the data
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE);

i_training <- training[inTrain,];
i_testing <- training[-inTrain,];





#################################################
#
set.seed(2233)

modelFit <- rpart(classe ~ ., data=i_training, method="class");

####
# As you can see, I have made a terrible mistake, the decision tree only uses the X column because is an enumeration of the rows, we have to delete that column.

i_training$X = NULL;

##
#	So we are going to train again and prove the model
modelFit <- train(classe ~., data=i_training, method = "svmLinear");

i_testing$classtesting <- predict(modelFit, newdata = i_testing)

confusionMatrix(i_testing$classtesting, i_testing$classe)

# With the confusionMatrix we can see this is a good model
# Now we'll use the test data to predict the manner in which they did the exercise
#i_testing$result <- ifelse(i_testing$classtesting == i_testing$classe,1,0)

testing$testpre <- predict(modelFit, newdata = testing)
n = length(testing$testpre)
for(i in 1:n){
	filename = paste0("case_",i,".txt")
	write.table(testing$testpre[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}