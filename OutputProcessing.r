OutputProcessing<-function(WD, OD, CropName, RoundOfGLUE, ModelRunNumber)
{
 eval(parse(text=paste("source('",WD,"/EvaluateProcessing.r')",sep = '')));
 TreatmentNumber<-EvaluateProcessing(WD, OD, CropName, RoundOfGLUE, ModelRunNumber);
 #Process the evaluate data and return the number of treatment.

 # eval(parse(text=paste('write("",file = "',OD,'/Evaluate_output.txt")',sep = '')));
 # #Empty the output files after model running. 
}
