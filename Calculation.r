## The function to calculate the likelihood values.

Calculation<-function(Simulation,Measurement,Variance)
{
Constant=10000;
#A constant to amplify the likelihood values in case of beyond computer precision becasue the vasues may be very small.

Likelihood<-Constant*(1/sqrt(2*pi*Variance))*exp(-(Measurement-Simulation)^2/(2*Variance));
# The likelihood function derived from the normal distribution.

return(Likelihood);

}