## This function is to derive the posterior distributions of the genotype coefficients.

PosteriorDistribution<-function(WD, OD, ParameterNames, ParameterProperty, CropName, RoundOfGLUE)
{
#Step 1. Read the saved parameter sets and their corresponding probabilities.
eval(parse(text = paste('RandomAndProbability<-read.table("',OD,
'/RandomParameterSetsAndProbability_',RoundOfGLUE,'.txt",header=TRUE,comment.char="")',sep="")));
#print(RandomAndProbability);

#Step 2. Derive the mean and variance values of the posterior distribution.
ParameterNumber<-length(ParameterNames);

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





