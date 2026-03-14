## This function is to derive the posterior distributions of the genotype coefficients.

PosteriorDistribution<-function(WD, OD, ParameterNames, ParameterProperty, CropName, RoundOfGLUE)
{
glueWarningLogFile <- file.path(OD, "GlueWarning.txt")

#Step 1. Read the saved parameter sets and their corresponding probabilities.
eval(parse(text = paste('RandomAndProbability<-read.table("',OD,
'/RandomParameterSetsAndProbability_',RoundOfGLUE,'.txt",header=TRUE,comment.char="")',sep="")));
#print(RandomAndProbability);

# Check if "Probability" column exists
if (!("Probability" %in% colnames(RandomAndProbability))) {
  errorMsg <- "PosteriorDistribution: 'Probability' column not found in RandomParameterSetsAndProbability file."
  write(errorMsg, file = glueWarningLogFile, append = T)
  print(errorMsg)
  stop(errorMsg)
}

# Check if probability has valid values (no NA)
if (any(is.na(RandomAndProbability[,"Probability"]))) {
  errorMsg <- paste("PosteriorDistribution: NA values found in Probability column.",
   "The likelihood calculation failed due to numerical issues or no valid model runs.")
  write(errorMsg, file = glueWarningLogFile, append = T)
  print(errorMsg)
  stop(errorMsg)
}

#Step 2. Derive the mean and variance values of the posterior distribution.
ParameterNumber<-length(ParameterNames);

# Check if the "ParameterNames" match the columns in the file (excluding "Probability")
dataCols <- ncol(RandomAndProbability) - 1  # last column is Probability
if (ParameterNumber != dataCols) {
  errorMsg <- paste0("PosteriorDistribution: Number of parameter names (", ParameterNumber,
    ") does not match the number of parameter columns in the data file (", dataCols,
    "). Check that the cultivar/ecotype file columns align with the genetic coefficients.")
  write(errorMsg, file = glueWarningLogFile, append = T)
  print(errorMsg)
  stop(errorMsg)
}

Mean<-c();
STDEV<-c();
MaxProbability<-c();

for (i in 1:ParameterNumber)
{
ParameterMean<-sum(RandomAndProbability[,i]*RandomAndProbability[,"Probability"]);
#ParameterMean<-sum(RandomAndProbability[,ParameterNames[i]]*RandomAndProbability[,"Probability"]);
#Calculate the posterior mean calues for each of the parameters.
Mean<-cbind(Mean, ParameterMean);

ParameterSTDEV<-sqrt(sum((RandomAndProbability[,i]-ParameterMean)^2*RandomAndProbability[,"Probability"]));
#ParameterSTDEV<-sqrt(sum((RandomAndProbability[,ParameterNames[i]]-ParameterMean)^2*RandomAndProbability[,"Probability"]));
#Calculate the posterior standard deviation for each of the parameters.

STDEV<-cbind(STDEV, ParameterSTDEV);

MaximumProbabilityIndex<-which.max(RandomAndProbability[,"Probability"]);
#ParameterMaxProbability<-as.numeric(RandomAndProbability[MaximumProbabilityIndex,ParameterNames[i]]);
ParameterMaxProbability<-as.numeric(RandomAndProbability[MaximumProbabilityIndex, i]);
#Get the parameter set that has the largest probability value.
MaxProbability<-cbind(MaxProbability, ParameterMaxProbability);
}

Mean<-as.numeric(sprintf('%4.3f', Mean));
STDEV<-as.numeric(sprintf('%4.3f', STDEV));
MaxProbability<-as.numeric(sprintf('%4.3f', MaxProbability));

PosteriorDistribution<-rbind(Mean, STDEV, MaxProbability);

colnames(PosteriorDistribution)<-ParameterNames;
rownames(PosteriorDistribution)<-c("Mean","STDEV","MaxProbability");

eval(parse(text = paste('write.table(PosteriorDistribution,file ="',OD,
'/PosteriorDistribution_', RoundOfGLUE,'.txt")',sep="")));
#Save the information about posterior distribution in a file.

} 





