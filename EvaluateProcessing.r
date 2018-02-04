##The program to process the data in Evaluate.Out of DSSAT output files.
EvaluateProcessing<-function(WD, OD, CropName, RoundOfGLUE, ModelRunNumber)
{

eval(parse(text=paste("File<-readLines('",OD,"/Evaluate_output.txt',n=-1)",sep = '')));
FileLength<-length(File);
#print(FileLength);

if ((CropName != "BA") & (CropName != "WH") & (CropName != "CS"))   
{
TreatmentNumber<-(FileLength-3);##Get the number of treatments.
#print(TreatmentNumber);

FrameTitle<-substr(File[3],2,500);
FrameData<-c();
##Read the titles of the predictions, which is located in the 3rd line of the evaluate output.
#FrameData<-c(); ##Creat an empty frame to store the selected data.

SummaryFrame<-data.frame()

for (j in 1:TreatmentNumber)
{
n=3+j;##Set the starting line for data reading in an individual model run, staring from the 4th line.
FrameData=rbind(FrameData,File[n]);##Combine the results in one matrix.
}

if(ModelRunNumber!=1)
{
EvaluateFrame<-FrameData;
eval(parse(text = paste('write(EvaluateFrame, "',OD,'/EvaluateFrame_',
RoundOfGLUE,'.txt", append = T)',sep="")));
#Write the data as a frame file, where repair the treatment names as an entity (" ").
} else
{
EvaluateFrame<-rbind(FrameTitle,FrameData);
eval(parse(text = paste('write(EvaluateFrame, "',OD,'/EvaluateFrame_',
RoundOfGLUE,'.txt", append = F)',sep="")));
}

} else if (CropName == "BA" || CropName == "WH" || CropName == "CS") ##The eveluate.out file of BA (barley), WH (wheat) is different from the others, such as Maize.
{
TreatmentNumber<-(FileLength-5);

FrameTitle<-substr(File[5],2,500);
FrameData<-c();

SummaryFrame<-data.frame()

for (j in 1:TreatmentNumber)
{
n=5+j;##Set the starting line for data reading in an individual model run, staring from the 6th line for Barley.
FrameData=rbind(FrameData,File[n]);
}

if(ModelRunNumber!=1)
{
EvaluateFrame<-FrameData;
eval(parse(text = paste('write(EvaluateFrame, "',OD,'/EvaluateFrame_',
RoundOfGLUE,'.txt", append = T)',sep="")));
} else
{
EvaluateFrame<-rbind(FrameTitle,FrameData);
eval(parse(text = paste('write(EvaluateFrame, "',OD,'/EvaluateFrame_',
RoundOfGLUE,'.txt", append = F)',sep="")));
}
}

rm(File);
}



