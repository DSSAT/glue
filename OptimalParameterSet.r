##This is the function to get the optimal parameter set for future research.

OptimalParameterSet<-function (GLUEFlag, OD, DSSATD, CropName, CultivarID, CultivarName, GenotypeFileName, TotalParameterNumber, EcotypeID, EcotypeParameters)
{
##1.Select the optimal parameter set.
if(GLUEFlag==1 | GLUEFlag==3)
{
eval(parse(text=paste('Optimal<-read.table("',OD,
'/PosteriorDistribution_2.txt", header=TRUE,comment.char="")',sep = '')));
OptimalParameterSet<-Optimal["MaxProbability",];
} else
{
eval(parse(text=paste('Optimal<-read.table("',OD,
'/PosteriorDistribution_1.txt", header=TRUE,comment.char="")',sep = '')));
OptimalParameterSet<-Optimal["MaxProbability",];
}

##2.Change partial cultivar file above with the values of optimal parameter set.
## This cultivar then can be chosen by model users to do future simulations. It requires model
## users to copy this cultivar to replace the corresponding cultivar in the genotype file under "DSSAT048/Genotype".

eval(parse(text=paste('GenotypeFilePath="',OD,'/',GenotypeFileName,'.CUL"',sep = '')));
GenotypeFile<-readLines(GenotypeFilePath, n=-1);
# Read the genotype file in the working directory.

CultivarID<-paste("^",CultivarID, sep='');
CultivarAddress<-grep(pattern=CultivarID, GenotypeFile);
# Read the line that has the information of the cultivar that is under estimation.

OldLine<-GenotypeFile[CultivarAddress];
#Get the line according to the line number.

if(EcotypeParameters > 0){
  eval(parse(text=paste('EcotypeFilePath="',GD,'/',GenotypeFileName,'.ECO"',sep = '')));
  EcotypeFile<-readLines(EcotypeFilePath, n=-1)
  EcotypeID<-paste("^",EcotypeID, sep='');
  EcotypeAddress<-grep(pattern=EcotypeID, EcotypeFile);
  EcoOldLine<-EcotypeFile[EcotypeAddress];
}

if(CropName != "SC")
{
  Step<-6;
  ValuePosition1<-(38-Step);
  ValuePosition2<-(42-Step);
  #Set the starting and end points of parameter locations.

  EcoValuePosition1 <- (26-Step);
  EcoValuePosition2 <- (30-Step);

  for (i in 1:TotalParameterNumber)
  {
  ValuePosition1<-ValuePosition1+Step;
  ValuePosition2<-ValuePosition2+Step;

  eval(parse(text = paste("Parameter<-OptimalParameterSet[1,",i,"]",sep = '')));
  #To solve the format problem for parameters with negative values. Modified by He, 2015-6-18.
  if(Parameter < 0 & Parameter > -1.0)                                                          #
  {                                                                                             #
  ParameterFormat<-sprintf('%1.3f', Parameter);                                                 #
  ParameterFormat<-paste(substring(ParameterFormat,1,1), substring(ParameterFormat,3), sep=''); #
  } else if (Parameter <= -1.0 & Parameter > -10.0)                                             # 
  {                                                                                             #
  ParameterFormat<-sprintf('%2.2f', Parameter);                                                 #
  } else if (Parameter <= -10.0 & Parameter > -100.0)                                           #
  {                                                                                             #
  ParameterFormat<-sprintf('%3.1f', Parameter);                                                 #
  }                                                                                             #
  
  if(Parameter>=0 & Parameter<10)
  {
  ParameterFormat<-sprintf('%1.3f', Parameter);
  } else if (Parameter>=10 & Parameter<100)
  {
  ParameterFormat<-sprintf('%2.2f', Parameter);
  } else if (Parameter >= 100)
  {
  ParameterFormat<-sprintf('%3.1f', Parameter);
  }
  if(EcotypeParameters > 0 & i > (TotalParameterNumber - EcotypeParameters)){ #Write on .ECO file
    EcoValuePosition1 <- EcoValuePosition1+Step;
    EcoValuePosition2 <- EcoValuePosition2+Step;
    
    substr(EcoOldLine, EcoValuePosition1, EcoValuePosition2)<-ParameterFormat;
  }else{ #Write on .CUL file
    substr(OldLine, ValuePosition1, ValuePosition2)<-ParameterFormat;
  }  
  }
} else
{
  Step<-15;
  
  #chp modified
  ValuePosition1<-(47-Step);
  ValuePosition2<-(61-Step);
  #Set the starting and end points of parameter locations.

  for (i in 1:TotalParameterNumber)
  {
  ValuePosition1<-ValuePosition1+Step;
  ValuePosition2<-ValuePosition2+Step;

  eval(parse(text = paste("Parameter<-OptimalParameterSet[1,",i,"]",sep = '')));

  if(Parameter>=0 & Parameter<10)
  {
  ParameterFormat<-sprintf('%1.3f', Parameter);
  
  } else if (Parameter>=10 & Parameter<100)
  {
  ParameterFormat<-sprintf('%2.2f', Parameter);
  
  # chp added extra format statement for values between 100 and 1000
  } else if (Parameter>=100 & Parameter<1000)
  {
  ParameterFormat<-sprintf('%3.1f', Parameter);
  
  } else
  {
  ParameterFormat<-sprintf('%4.0f', Parameter);
  }
  
  #print(ParameterFormat);
  
  substr(OldLine, ValuePosition1, ValuePosition2)<-'      ';# Delete initial values.
  substr(OldLine, ValuePosition1, ValuePosition2)<-ParameterFormat;
}

}

NumberOfIllegalCharacters<-grep(pattern="/", CultivarName);
if(length(NumberOfIllegalCharacters)!=0) CultivarName<-gsub(pattern="/", "_", CultivarName);
##For some cultivars, they have illegal characters such as /, *, # in theri name. This should be changed.
                                                 
eval(parse(text=paste("NewGenotypeFilePath='",OD,"/",CropName,"",CultivarName,".CUL'",sep = '')));
write(OldLine, file=NewGenotypeFilePath);

#updates the .CUL file
GenotypeFile[CultivarAddress] <- OldLine
write(GenotypeFile,paste0(OD,'/',GenotypeFileName,".CUL"))

#Save the new genotype file as "cul" file in the DSSAT directory.


if(EcotypeParameters > 0){
  eval(parse(text=paste("NewGenotypeFilePath='",OD,"/",CropName,"",CultivarName,".ECO'",sep = '')));
  write(EcoOldLine, file=NewGenotypeFilePath);
  
  EcotypeFile[EcotypeAddress] <- EcoOldLine;
  write(EcotypeFile,paste0(OD,'/',GenotypeFileName,".ECO"));
}
#Save the new ecotype file as "eco" file in the DSSAT directory.

}

