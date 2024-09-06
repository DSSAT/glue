##The program to process the data in Evaluate.Out of DSSAT output files.
EvaluateProcessing<-function(WD, OD, CropName, RoundOfGLUE, ModelRunNumber)
{

eval(parse(text=paste("File<-readLines('",OD,"/Evaluate_output.txt',n=-1)",sep = '')));
FileLength<-length(File);
#print(FileLength);

TitleLine = grep("@RUN",File)

FrameTitle<-File[TitleLine]
FrameData<-c();
##Read the titles of the predictions, which is located in the 3rd line of the evaluate output.
#FrameData<-c(); ##Creat an empty frame to store the selected data.

TreatmentNumber<-(FileLength-TitleLine);##Get the number of treatments.
#print(TreatmentNumber);

#SummaryFrame<-data.frame()

for (j in 1:TreatmentNumber)
{
  n=TitleLine+j;
  FrameData=rbind(FrameData,File[n]);##Combine the results in one matrix.
}

#The if statement had to change because it was only writing the evaluate header 
# during the first model run -- this does not work in this parallelized version
#if(ModelRunNumber!=1)
if(file.exists(paste0(OD,'/EvaluateFrame_',RoundOfGLUE,'.txt')))
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

rm(File);
}



