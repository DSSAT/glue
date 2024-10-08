##Generate random numbers that follows uniform distribution

Uniform<-function(CulData, TotalParameterNumber, ncol.predefined,NumberOfModelRun, RoundOfGLUE, GLUEFlag)
{
#(1)Read default values.
col.parastart = ncol.predefined + 1  
DefaultValue = CulData[3,col.parastart:ncol(CulData)]

#(2) Read parameter set that have the maximum likelihood values when first round of GLUE is finished.
if ((GLUEFlag==1)&(RoundOfGLUE==2))
{
eval(parse(text = paste('PosteriorDistribution1<-read.table("',OD,
'/PosteriorDistribution_1.txt",header=TRUE,comment.char="")',sep="")));

MaximumProbability<-as.numeric(PosteriorDistribution1["MaxProbability", ]);
#print(MaximumProbability);
}

#(3) Generate random numbers
ParameterMatrix<-c();

for (i in 1:TotalParameterNumber)
{
#Generate uniform random values for P[i], i is the total number of parameters involved.
colNo = i + ncol.predefined
Flag = CulData[4,colNo]

if (GLUEFlag==1)
{
  if (RoundOfGLUE==1)
  {
    if (Flag=="P")
    {
     Minimum=as.numeric(as.character(CulData[1,colNo]))
     Maximum=as.numeric(as.character(CulData[2,colNo]))
    } else if (Flag=="N")
    {
     Minimum=as.numeric(DefaultValue[i])
     Maximum=as.numeric(DefaultValue[i])
    } else if (Flag=="G")
    {
     Minimum=as.numeric(DefaultValue[i])
     Maximum=as.numeric(DefaultValue[i])
    }
  } else if (RoundOfGLUE==2)
  {
    if(Flag=="G")
    {
     Minimum=as.numeric(as.character(CulData[1,colNo]))
     Maximum=as.numeric(as.character(CulData[2,colNo]))
    } else if (Flag=="N")
    {
     Minimum=as.numeric(DefaultValue[i])
     Maximum=as.numeric(DefaultValue[i])
    } else if (Flag=="P")
    {
     Minimum=MaximumProbability[i];
     Maximum=MaximumProbability[i];
    }
  }
} else if (GLUEFlag==2)
{
    if (Flag=="P")
    {
    Minimum=as.numeric(as.character(CulData[1,colNo]))
    Maximum=as.numeric(as.character(CulData[2,colNo]))
    } else if (Flag=="N")
    {
    Minimum=as.numeric(DefaultValue[i])
    Maximum=as.numeric(DefaultValue[i])
    } else if (Flag=="G")
    {
    Minimum=as.numeric(DefaultValue[i])
    Maximum=as.numeric(DefaultValue[i])
    }
} else if (GLUEFlag==3)
{
     if (Flag=="N")
    {
     Minimum=as.numeric(DefaultValue[i])
     Maximum=as.numeric(DefaultValue[i])
    } else if (Flag=="P")
    {
     Minimum=as.numeric(DefaultValue[i])
     Maximum=as.numeric(DefaultValue[i])
    } else if (Flag=="G")
    {
    Minimum=as.numeric(as.character(CulData[1,colNo]))
    Maximum=as.numeric(as.character(CulData[2,colNo]))
    }
}
 
GenerateParameter<-runif(NumberOfModelRun,min=Minimum,max=Maximum);
MatrixGeneratedParameter<-matrix(GenerateParameter, nrow=NumberOfModelRun, ncol=1, byrow=T);
ParameterMatrix<-cbind(ParameterMatrix, MatrixGeneratedParameter);

}

#print(ParameterMatrix);

#ParameterMatrix<-ifelse(ParameterMatrix>=999,999,ParameterMatrix);
#Set the values of some parameters such as G2 that are great than 1000 to 999, otherwise, the model will stop running.

ParameterMatrix<-ifelse(is.na(ParameterMatrix), 0, ParameterMatrix);
#If the values of some parameters are NAs which were generated by missed parameter values in the genotype file,
#they were set as zeros.

#print(ParameterMatrix);

return(ParameterMatrix);                   
}

