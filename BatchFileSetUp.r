#Change the genotype file of maize in DSSAT.

BatchFileSetUp<-function(WD, OD, CultivarBatchFile)
{
eval(parse(text=paste('BatchFilePath="',WD,'/DSSBatch.template"',sep = '')));
BatchFileTamplate<-readLines(BatchFilePath, n=-1);
#Get the template of the batch file.

eval(parse(text=paste('BatchFilePath="',OD,'/',CultivarBatchFile,'"', sep = '')));
CultivarBatchFile<-readLines(BatchFilePath, n=-1);
TotalLineNumber<-length(CultivarBatchFile);
#Get the batch file generated with GenSelect.

LineAddress<-grep(pattern="@FILEX", CultivarBatchFile)
#print(LineAddress);
StartLine<-LineAddress+1;
EndLine<-TotalLineNumber;
SubCultivarBatchFile<-CultivarBatchFile[StartLine:EndLine];
#print(SubCultivarBatchFile);

NewBatchFile<-c(BatchFileTamplate, SubCultivarBatchFile);
#print(NewBatchFile); 

eval(parse(text=paste("NewBatchFilePath='",OD,"/DSSBatch.v48'",sep = '')));
write(NewBatchFile, file=NewBatchFilePath);
}




 
