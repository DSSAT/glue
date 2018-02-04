##The function to calculate the likelihood values for each of the integrated measurements.

IntegratedLikelihoodCalculation1<-function (WD, OD, CropName)
{

##Step 1. Get the information of integrated measurements from the processed evaluation file.
eval(parse(text=paste('Evaluation<-read.table("',OD,'/EvaluateFrame_1.txt", header=TRUE,comment.char="")',sep = '')));
#print(Evaluation);
Dimension<-dim(Evaluation);

Treatments<-Evaluation[,"RUN"]
TreatmentNumberIndex<-which.max(Evaluation[,"RUN"]);
TreatmentNumber<-Treatments[TreatmentNumberIndex]; ##Get the number of treatments in the experiment.
RunNumber<-(Dimension[1]/TreatmentNumber); ##Get the number of model runs.

## Step 2. Get the information about measurement variance from a given Excel file.
#library(xlsReadWrite);

if (CropName=="PT" || CropName=="SC" || CropName=="CS" || CropName=="TN" || CropName=="TR" || CropName=="WH" || CropName=="PI")
{
#  eval(parse(text=paste('VAR<-read.xls("',WD,
#  '/MeasurementVariance.xls", sheet = "',CropName,'", rowNames = T, colNames=T)',sep = '')));
  
  eval(parse(text=paste('VAR<-read.csv("',WD,
  '/MeasurementVariance_', CropName, '.csv", header = T)',sep = '')));
} else
{
#  eval(parse(text=paste('VAR<-read.xls("',WD,
#  '/MeasurementVariance.xls", sheet = "Sheet 1", rowNames = T, colNames=T)',sep = '')));

  eval(parse(text=paste('VAR<-read.csv("',WD, '/MeasurementVariance_All.csv", header = T)',sep = '')));
}

newRowNames <- VAR[ , 1];
VAR <- VAR[ , -1];
rownames(VAR) <- newRowNames;
##Read the variance information including standard devaiton, Variance, and CV from a given Excel file.

##Step 3. Calclate the likelihood value for the integrated measurements, such as
 #anthesis date (ADAP), maturity date (MDAP), first pod date (PD1TS), 
 #Pod/Ear/Panicle weight at maturity (kg [dm]/ha) (PWAM),
 #Yield at harvest maturity (kg [dm]/ha) (HWAM), Tops weight at maturity (kg [dm]/ha) (CWAM),
 #Leaf area index, maximum (LAIX), Leaf number per stem at maturity (L#SM). 

MeasurementNames<-c();
RowNames<-rownames(VAR);

NumberOfMeasurement<-dim(VAR)[1];

if (CropName=="BA" || CropName=="RI" || CropName=="WH")
{
  VAR["PD1T","Flag"]<-0;
  VAR["PWAM","Flag"]<-0;
#Evaluate file of barley doesn't have this two outputs.
} 

for (i in 1:NumberOfMeasurement)
{
if (VAR[i,"Flag"]==1)
{
MeasurementNames<-c(MeasurementNames,RowNames[i]);
}
}

eval(parse(text=paste('EvaluateFile<-readLines("',OD,'/Evaluate_output.txt",n=-1)',sep = '')));
IsPD1P<-grep("PD1P",EvaluateFile);
#Read the Evaluate file to see if it contains the output called "PD1P".

if(length(IsPD1P)!=0)
{  
Address<-which(MeasurementNames == "PD1T");
MeasurementNames[Address]<-"PD1P";
##Integrated measurement names used in first round of GLUE. 
}
##Integrated measurement names used in first round of GLUE for the legume crops, since the legume crop has
##an output as "PD1P" not as "PD1T". Must be careful when modified the measurement variance file. Make sure
##the output name is correct.

MeasurementNumber<-length(MeasurementNames); ##Number of integrated measurements.

IntegratedLikelihoodMatrix<-matrix(Evaluation[,1],nrow=length(Evaluation[,1]),ncol=1,byrow=T);
#The first column of Evaluation is the number of runs in each model run with an individual random parameter sets.
#Since more than one experiment could be run simultaneously, the total treatment number should be the sum of all
#treatments in all experiments. Thus, the value of "Run" is used as number of "Total Treatment".

IntegratedLikelihoodMatrix<-rbind("Treatment",IntegratedLikelihoodMatrix);
#Set the title.

##Calculate the likelihood values for each of the measurements.
for (i in 1:MeasurementNumber)
{

Simulation<-Evaluation[,paste(MeasurementNames[i],"S", sep="")]; ##Read the simulated values.
Measurement<-Evaluation[,paste(MeasurementNames[i],"M", sep="")]; ##Read the measured values.
Simulation<-ifelse(Simulation==-99, NA, Simulation);
Measurement<-ifelse(Measurement==-99, NA, Measurement);##Change the unknown measurements "-99" to NA.

if (is.na(VAR[MeasurementNames[i],"CV"]))
{
Variance<-VAR[MeasurementNames[i],"Variance"];
} else
{
CV<-VAR[MeasurementNames[i],"CV"];
Variance<-(Measurement*CV)^2;
}

eval(parse(text=paste('source("',WD,'/Calculation.r")',sep = '')));
Likelihood<-Calculation(Simulation, Measurement, Variance); 
##Call the function "Calculation" to calculate the likelihood values.

Likelihood<-c(MeasurementNames[i],Likelihood); ##Add a tile for the likelihood values.

eval(parse(text = paste('Likelihood',MeasurementNames[i],'<-matrix(Likelihood,nrow=length(Likelihood),ncol=1,byrow=T)', 
sep = ''))); ##Save the likehood values for each of the measurements as a single column matrix.

eval(parse(text = paste('IntegratedLikelihoodMatrix<-cbind(IntegratedLikelihoodMatrix,Likelihood',
MeasurementNames[i],')',sep="")));

#print(LikelihoodHDAP);
##Save the likehood values for all of the measurements as a matrix, with the treatment number as the first column.

}

IntegratedLikelihoodMatrix<-ifelse(is.na(IntegratedLikelihoodMatrix)==TRUE, 1, IntegratedLikelihoodMatrix);
#Change the NA values to 1 before calculating the combined likelihood value.
                     
#library ('MASS'); 
#eval(parse(text=paste('write.matrix(IntegratedLikelihoodMatrix,file ="',OD,
#'/IntegratedLikelihoodMatrix_Frame_1.txt")',sep = '')));

eval(parse(text=paste('write.table(as.matrix(IntegratedLikelihoodMatrix), file ="',OD,
'/IntegratedLikelihoodMatrix_Frame_1.txt", row.names = F, col.names = F, append = F)',sep = '')));

eval(parse(text=paste('IntegratedLikelihoodMatrixTable<-read.table("',OD,
'/IntegratedLikelihoodMatrix_Frame_1.txt", header=TRUE,comment.char="")',sep = '')));

##Step 4. Likelihood combination
eval(parse(text=paste('source ("',WD,'/Combination.r")',sep = '')));
IntegratedCombinedLikelihood<-Combination(IntegratedLikelihoodMatrixTable);
#Calculate the combined likelihood values for the integrated measurements.

names(IntegratedCombinedLikelihood)<-"IntegratedCombinedLikelihood" #Set the name for the combined likelihood values.

IntegratedLikelihoodMatrixTable<-cbind(IntegratedLikelihoodMatrixTable,IntegratedCombinedLikelihood)#Add the combined likelihood 1 to the likelihood matrix.

##Step 5. Distribute the icombined likelihood values of the integrated measurements to each treatment.
for (i in 1:TreatmentNumber)
{

Selection<-i;                               
RowIndex <- which(Selection==IntegratedLikelihoodMatrixTable$Treatment);
# Get the address of the rows that contain information of treatment i.
ColumnIndex<-c("Treatment",MeasurementNames,"IntegratedCombinedLikelihood");

eval(parse(text = paste('IntegratedCombinedLikelihoodTreatment_1_',i,
'<-IntegratedLikelihoodMatrixTable[RowIndex,ColumnIndex]',sep="")));

#library ('MASS'); 
#eval(parse(text = paste('write.matrix(IntegratedCombinedLikelihoodTreatment_1_',i,
#',file ="',OD,'/IntegratedLikelihoodTreatment_1_',i,'.txt")',sep="")));

eval(parse(text = paste('write.table(as.matrix(IntegratedCombinedLikelihoodTreatment_1_',i,
'), file ="',OD,'/IntegratedLikelihoodTreatment_1_',i,'.txt", row.names = F, col.names = T, append = F)',sep="")));

}

return(TreatmentNumber);
rm(list = ls());

}
