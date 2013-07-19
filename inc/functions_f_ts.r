library("tseries")

getfts=function(xts_window1=xts_window){
fts_ready<<-0
print("Loading financial time series")
date_end<<-Sys.Date()+1
since_date<<-date_end-xts_window1
fchartv<<-get.hist.quote(instrument = stocksmbl[1], start = since_date)$Close
df.zoo<<-(as.zoo(fchartv))
for (i_smbl in stocksmbl[-1]){
fchartv<<-get.hist.quote(instrument = i_smbl, start = since_date)$Close
fchartv.zoo<<-as.zoo(fchartv)
df.zoo<<-merge(df.zoo,fchartv.zoo)
}
names(df.zoo)<<-c(stocksmbl)
df.locf<<-na.locf(df.zoo)
data_fts<<-window(df.locf, start =since_date+1)
data_frame_fts.xts<<-data_fts
write.table(data.frame(date=as.character(index(data_fts)),
coredata(data_fts)),
file=filename_fts, row.names = FALSE)
print("Done!")
}

loadfts=function(xts_window1=xts_window){
if (file.exists(filename_fts)){
data_frame_fts<<-read.table(filename_fts,header = TRUE)
data_frame_fts.xts.all<<-xts(data_frame_fts[-1],as.Date(data_frame_fts[[1]])) 
date_end<<-Sys.Date()+1
date_start<<-date_end-xts_window1
data_frame_fts.xts<<-data_frame_fts.xts.all[paste(date_start,"/",sep="")]
}else {
print("Finance times series are not loaded. Please click the button 'Load finance time series'");
return(FALSE)
}
}