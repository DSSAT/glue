#Change the genotype file of crops in DSSAT.

GenotypeChange<-function(GD, DSSATD, OD, CropName, GenotypeFileName, CultivarID, TotalParameterNumber, RunNumber, RandomMatrix)
{
eval(parse(text=paste('GenotypeFilePath="',GD,'/',GenotypeFileName,'.CUL"',sep = '')));

ReadLine<-readLines(GenotypeFilePath, n=-1)
GenotypeFile<-as.character(ReadLine); #Get the genotype file saved as a template.

LineNumber<-grep(pattern=CultivarID, GenotypeFile); #Get the number of the line where the cultivar "GLUECUL" is located.
OldLine<-GenotypeFile[LineNumber];#Get the line according to the line number.

R<-RunNumber;#Get what parameter set will be used to change the genotype file.

# if (CropName != "SC")
# {
  ParameterStep<-6;
  ValuePosition1<-(38-ParameterStep);
  ValuePosition2<-(42-ParameterStep);

  for (i in 1:TotalParameterNumber)
  {
  ValuePosition1<-ValuePosition1+ParameterStep;
  ValuePosition2<-ValuePosition2+ParameterStep;

  eval(parse(text = paste("Parameter<-RandomMatrix[R,",i,"]",sep = '')));
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
  
  if(Parameter >= 0 & Parameter < 10)
  {
  ParameterFormat<-sprintf('%1.3f', Parameter);
  } else if (Parameter >= 10 & Parameter < 100)
  {
  ParameterFormat<-sprintf('%2.2f', Parameter);
  } else if (Parameter >= 100)
  {
  ParameterFormat<-sprintf('%3.1f', Parameter);
  }

  substr(OldLine, ValuePosition1, ValuePosition2)<-ParameterFormat;
  }

  GenotypeFile[LineNumber]<-OldLine;#Replace the old line with new generated line in the Genotype file.
# } else
# {
#   ParameterStep<-15;
# 
#   #chp modified
#   ValuePosition1<-(47-ParameterStep); #The initial starting point was 42, but it was changed to 46 since "EXPNO" was added by Cheryl recently.
#   ValuePosition2<-(61-ParameterStep); #The initial ending point was 47, but it is 51 now.
# 
#   for (i in 1:TotalParameterNumber)
#   {
#   ValuePosition1<-ValuePosition1+ParameterStep;
#   ValuePosition2<-ValuePosition2+ParameterStep;
# 
#   eval(parse(text = paste("Parameter<-RandomMatrix[R,",i,"]",sep = '')));
# 
#   if(Parameter>=0 & Parameter<10)
#   {
#   ParameterFormat<-sprintf('%1.3f', Parameter);
#   } else if (Parameter>=10 & Parameter<100)
#   {
#   ParameterFormat<-sprintf('%2.2f', Parameter);
#   
#   # chp added extra format statement for values between 100 and 1000
#   } else if (Parameter>=100 & Parameter<1000)
#   {
#   ParameterFormat<-sprintf('%3.1f', Parameter);
#   } else
#   
#   # chp - how to get format xxxx. (with nothing past the ".") ?
#   {
#   ParameterFormat<-sprintf('%4.0f', Parameter);
#   }
# 
#   ##chp
#   #print(ParameterFormat);
#   #print (" ");
#   
#   substr(OldLine, ValuePosition1, ValuePosition2)<-'      ';# Delete initial values.
#   substr(OldLine, ValuePosition1, ValuePosition2)<-ParameterFormat;
#   
#  }

  GenotypeFile[LineNumber]<-OldLine;#Replace the old line with new generated line in the Genotype file.
  
  eval(parse(text=paste('ECOFilePath="',GD,'/SCCAN048.ECO"',sep = '')));
  ReadLine<-readLines(ECOFilePath, n=-1)
  ECOFile<-as.character(ReadLine);
  #Get the ECO file from the Genotype directory.
  
  eval(parse(text=paste("NewECOFilePath='",OD,"/SCCAN048.ECO'",sep = '')));
  write(ECOFile, file=NewECOFilePath);
  #Save the ECO file in the GLWork directory. 
}
                                                   
eval(parse(text=paste("NewGenotypeFilePath='",OD,"/",GenotypeFileName,".CUL'",sep = '')));
write(GenotypeFile, file=NewGenotypeFilePath);
#Save the new genotype file as "cul" file in the GLWork directory.

}




 
