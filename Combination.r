## This is the function to combine the likelihood values from different measurements.

Combination<-function(LikelihoodMatrix)
{

#print(LikelihoodMatrix);
Dimension<-dim(LikelihoodMatrix);
#print(Dimension);

if (Dimension[2]==2)
{
  CombinedLikelihood<-LikelihoodMatrix[2];
} else
{
  CombinedLikelihood<-LikelihoodMatrix[2];

  for (i in 3:Dimension[2])
  {
   CombinedLikelihood<-CombinedLikelihood*LikelihoodMatrix[i];
  }

}

return(CombinedLikelihood);

}
