data_frame_ready<-0
dtm_ready<-0
fts_ready<-0

data_frame<-data.frame()
save_charts_opt<-0

stopwords1 <-stopwords('english')

get_corp=function(text1){
myCorpus <- Corpus(VectorSource(text1))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, removeWords, stopwords1)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
myCorpus <- tm_map(myCorpus, stripWhitespace)
return(myCorpus)
}

get_dtm=function(){
print("Creating documents-terms matrix for new data")
d.corpus<-Corpus(VectorSource(as.character(coredata(data_frame.xts))))
doc_term_mtrx<<-DocumentTermMatrix(d.corpus, control = list(wordLengths=c(3,Inf),bounds = list(global = c(10,Inf))))
print("Done!")
print(" ")
}

get_data_xts=function(xts_window1=xts_window){
data_frame.xts.all<<-xts(data_frame[[4]],as.POSIXct(data_frame[[2]], tz = "UTC")) 
date_end<<-Sys.Date()+1
date_start<<-date_end-xts_window1
data_frame.xts<<-data_frame.xts.all[paste(date_start,"/",sep="")]
#frame.xts<<-data_frame.xts[!weekdays(index(data_frame.xts)) %in% c("Saturday", "Sunday")]
#data_frame.xts<<-data_frame.xts[.indexwday(data_frame.xts) %in% 1:5]
}

preparedata=function(t1="f"){
print("Preparing data")
if(t1=="f"){get_data_from_file();}
get_data_xts()
get_dtm()
loadfts()
print("Done!")
print(" ")
}

prepare_data_frame=function(){
print("Creating data frame for analysis")
get_data_from_file()
get_data_xts()
print("Done!")
print(" ")
}

get_data_from_file=function()
{
tw_max_id<<-NULL
rss_max_time<<-NULL
if (file.exists(filename)){
data_frame<<-read.table(filename)
}
}

data_save=function(data_frame_save)
{
write.table(data_frame_save,file=filename,append=TRUE, row.names = FALSE,col.names = FALSE)
}