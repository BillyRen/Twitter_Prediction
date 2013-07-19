fts_r<<-zoo()
save_p<<-0
freqterms=function(minfreq=min_freq_terms,maxnterms=500){
if(data_frame_ready==0){prepare_data_frame(); data_frame_ready<<-1;}
if(dtm_ready==0){get_dtm(); dtm_ready<<-1;}

frterms <<- findFreqTerms(doc_term_mtrx, lowfreq=minfreq, highfreq = Inf)
frt1 <<- colSums(as.matrix(doc_term_mtrx))
frt <<- subset(frt1, frt1>=minfreq)
frt<<-sort(frt, decreasing = TRUE)
print(head(frt,maxnterms))
plottermscloud(head(frt,300))
return(head(frt,maxnterms))
}

assoc=function(x,thr=min_corr_assoc,pattern=""){

if(data_frame_ready==0){prepare_data_frame(); data_frame_ready<<-1;}
if(dtm_ready==0){get_dtm(); dtm_ready<<-1;}
x=tolower(x)
if(!(x %in% Terms(doc_term_mtrx))){
print("This term is not included into created index")
return()
}
print(" ")
print(paste("Associations with", x))
assoc_vl=findAssocs(doc_term_mtrx, x, thr)
plottermscloud(head(assoc_vl,100))
return (head(assoc_vl,100))}

freqsets_fun=function(x,frset1){
f_frst=function(x){
v1<-frset1 %in% x
if (length(v1[v1==TRUE])==length(frset1)) { res=1;} else {res=0;}
return(res)
}
tw_t=coredata(x)
tr_splt1=strsplit(as.character(tw_t), "[[:space:]]+")
arr1<-sapply(tr_splt1,f_frst)
if (relative_freq_frsets){return(sum(arr1)/length(tw_t))}
else{return (sum(arr1))}
}

freqsets=function(fr_list_str,fts_chart=NULL,rollmean_p=rollmean_period,rollmean_p2=rollmean_period2){
if(data_frame_ready==0){prepare_data_frame(); data_frame_ready<<-1;}
if(fts_ready==0){
if(!loadfts()){fts_chart=NULL}
else{fts_ready<<-1;}
}
frsets_t_xts<<-xts()
fr_list_1=strsplit(fr_list_str,"[[:space:]]+")[[1]]
print(fr_list_1)
frset1v<-fr_list_1
frsets_t_xts<<-period.apply(data_frame.xts,INDEX=endpoints(data_frame.xts,on="days"),FUN=freqsets_fun,frset1=frset1v)
plot_freqsets1(frsets_t_xts,fr_list_1,fts_chart,rollmean_p,rollmean_p2)
names(frsets_t_xts)<-"Frequent Set"
print(frsets_t_xts)
return(frsets_t_xts)
}

plot_freqsets1=function(frsets_t_xts,fr_list_1,fts_chart,rollmean_p,rollmean_p2){
graphics.off()
frsets_t_1<<-zoo()
frsets_t_1m<<-zoo()
frsets_t_1m2<<-zoo()
ncharts<<-dim(frsets_t_xts)[2];
ylab1<<-c("Frequent Set")
if(!is.null(fts_chart)){if(fts_chart=="None"){fts_chart=NULL;}}
if(!is.null(fts_chart)){
ncharts<<-ncharts+1
ylab1<<-c(ylab1,names(data_frame_fts.xts[,fts_chart]))
frsets_t_xts1<<-frsets_t_xts
fts_chart1<<-fts_chart
frsets_t_1<<-merge(zoo(coredata(frsets_t_xts1),as.Date(index(frsets_t_xts1))),
as.zoo(data_frame_fts.xts[,fts_chart]));
frsets_t_1<<-na.locf(frsets_t_1)
frsets_t_1<<-na.omit(frsets_t_1)
}
else {frsets_t_1<<-as.zoo(frsets_t_xts);}

screens_value<<-1:ncharts;
lwd_value<<-rep(1,ncharts)
colors_list<<-rep("black",ncharts)
 
if(!is.null(rollmean_p)){
frsets_t_1m<<-rollmean(frsets_t_1,rollmean_p,fill=NA,align="right");
frsets_t_1m2<<-rollmean(frsets_t_1,rollmean_p2,fill=NA,align="right");
 
frsets_t_1<<-cbind(frsets_t_1,frsets_t_1m,frsets_t_1m2); 
screens_value<<-c(screens_value,1:ncharts,1:ncharts);
lwd_value<<-c(lwd_value,rep(2,ncharts),rep(2,ncharts))
colors_list<<-c(colors_list,rep("red",ncharts),rep("blue",ncharts))
}
 
if(length(fts_chart)>0){fts_chartv1=paste("vs",fts_chart,sep=" ")}else{fts_chartv1=""}

if(!is.null(fts_chart)){
ccf(as.ts(frsets_t_1[,3]),as.ts(frsets_t_1[,4]),na.action = na.pass, 
main=paste("Correlation: {",paste(fr_list_1,collapse=", "),"}", fts_chartv1, sep=" "))
}

if(!is.null(fts_chart)){
ccf(as.ts(frsets_t_1[,3]),as.ts(frsets_t_1[,4]),na.action = na.pass, 
main=paste("Correlation: {",paste(fr_list_1,collapse=", "),"}", fts_chartv1, sep=" "))
}

dev.new(xpos="-30", ypos="25")
if(save_p==1){png(save_p_file)}
plot(frsets_t_1, type="l",  lwd=lwd_value, screens=screens_value, col=colors_list,
main=paste("Chart: {",paste(fr_list_1,collapse=", "),"}", fts_chartv1, sep=" "), xlab = "Time", ylab = ylab1)
if(save_p==1){dev.off()}
 
}

plottermscloud=function(wordFreq){
set.seed(500)
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=1, random.order=F)
}
 
stock=function(symb,period=xts_window,npredict=15, fr_list_str=""){
sys_date=Sys.Date()+1
until_date=sys_date
since_date=until_date-period
getSymbols(symb, from=since_date, to=until_date)
chart=get(symb)
chart_f1=chart
names(chart_f1)[4]="v4"
vcl=chart_f1$v4
z1=ets(vcl)
z1.f=forecast(z1,h=npredict, fan = TRUE)
dev.new(xpos="-57", ypos="25")
plot(z1.f,main=paste("Forecasting for ",symb))
dev.new(xpos="-75", ypos="55")

fts1<<-zoo(coredata(frsets_t_1m[,1]),as.Date(index(frsets_t_1m[,1])))
fts2<<-zoo(coredata(frsets_t_1m2[,1]),as.Date(index(frsets_t_1m2[,1])))
fts_r<<-cbind(fts1,fts2)
names(fts_r)<<-c("Average 1","Average 2")
chartSeries(chart,theme="white")
plot(addBBands())
plot(addTA(fts_r,lwd=c(2,2),col=c('red','blue'),legend=paste("Moving average of the frequent set {",fr_list_str,"}",sep="")))
 


}

