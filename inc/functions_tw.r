getusrtweets=function(tw_srch_usr=tw_search_usr,n_tweets=n_tw){
data_frame_ready<<-0
dtm_ready<<-0
get_data_from_file()
data_frame_tw<<-data.frame();
print("Loading users' tweets")

for (i_srchstr in tw_srch_usr){
if(length(data_frame)>0){ data_frame_sbst=subset(data_frame,V3==i_srchstr)$V1   
if(length(data_frame_sbst)>0){tw_max_id<<-max(data_frame_sbst)} else {tw_max_id<-NULL}
} else{tw_max_id<-NULL}

twts<-userTimeline(as.character(i_srchstr), n=n_tweets, lang='en', sinceID=tw_max_id);
print(paste("Loading tweets of the user ",i_srchstr," (",length(twts)," new tweets loaded)",sep=""))
if(length(twts)>1){tw_append_dataframe(twts,i_srchstr)}
data_save(data_frame_tw);data_frame_tw<<-data.frame();
}
print("Done!")
}

tw_append_dataframe=function(tw,tw_srchstr)
{
df1 <<- do.call("rbind", lapply(tw, as.data.frame))
df_inconv<<-sapply(df1[['text']],FUN=function(x) iconv(enc2utf8(x), sub = "byte"))
tw_corp<<-get_corp(df_inconv)
tw1<<-sapply(tw_corp,"[[",1)
df_id=as.character(df1$id)
df_created=as.character(as.POSIXct(df1$created))
if(length(tw1)==length(df_id) && length(tw1)==length(df_created) ){
data_frame_tw<<-rbind(data_frame_tw, data.frame(df_id, df_created, tw_srchstr, tw1))
}
}