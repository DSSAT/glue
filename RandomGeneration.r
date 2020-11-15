##Main function of generating random parameter values

RandomGeneration<-function(WD,CulData, TotalParameterNumber, ncol.predefined,NumberOfModelRun, RoundOfGLUE, GLUEFlag)
{

Option="U";
#Set uniform distribution as the default distribution to generate the random parameter sets.

if(Option=="U")
{
eval(parse(text = paste("source('",WD,"/Uniform.r')",sep = '')));
RandomParameter<-(Uniform(CulData, TotalParameterNumber, ncol.predefined,NumberOfModelRun, RoundOfGLUE, GLUEFlag));
#Get rid of the negative random values through ABS();

}

return(RandomParameter);##Return the generated random matrix.
}

