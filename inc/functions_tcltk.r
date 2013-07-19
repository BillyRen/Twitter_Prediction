require(tcltk)
tclRequire("BWidget")

main_gui=function(){
tt <- tktoplevel()
tkwm.title(tt,"Tweets Miner for Stock Markets")
mainframe <- tkframe(tt)
tkgrid(tklabel(mainframe,text="Tweets Miner for Stock Markets"),ipady = 1,ipadx=10,columnspan=2)
tkgrid(tklabel(mainframe,text="(author: B.Pavlyshenko, e-mail: b.pavlyshenko@gmail.com)"),ipady = 1,ipadx=10,columnspan=2)
tkgrid(tklabel(mainframe,text="Blog: bpavlyshenko.blogspot.com"),ipady = 1,ipadx=10,columnspan=2)
frame1<- tkframe(mainframe,relief="groove",borderwidth=2 )
button1a<-tkbutton(frame1, text="Load new users' tweets", command=function()getusrtweets())
button1b<-tkbutton(frame1, text="Load financial time series", command=function() getfts())
tkgrid(button1a,button1b,pady = 10,padx=20);  
button3<-tkbutton(frame1, text="Find frequent terms", command=function() freqterms())
#minfreq=as.numeric(tclvalue(min_freq_terms1))
tkgrid(button3,columnspan=2, pady = 15,padx=20);  
min_freq_terms1 <- tclVar(min_freq_terms)
textfield1=tkentry(frame1, textvariable=min_freq_terms1,width=5)
tklabelf1=tklabel(frame1,text="Min. frequency:")
tkgrid(tklabelf1 ,textfield1);
tkgrid.configure(tklabelf1,sticky="e");tkgrid.configure(textfield1,sticky="w");
button4<-tkbutton(frame1, text="Find terms associations", 
command=function() print(assoc(tclvalue(textfield_assoc1),as.numeric(tclvalue(min_corr_assoc1)))))
textfield_assoc1 <- tclVar("")
tkgrid(button4,columnspan=2, pady = 15,padx=20); 
textfield2=tkentry(frame1, textvariable=textfield_assoc1,width=15)
tklabelf2=tklabel(frame1,text="Term:")
tkgrid(tklabelf2,textfield2); 
tkgrid.configure(tklabelf2,sticky="e");tkgrid.configure(textfield2,sticky="w");
min_corr_assoc1 <- tclVar(min_corr_assoc)
textfield3=tkentry(frame1, textvariable=min_corr_assoc1,width=5)
tklabelf3=tklabel(frame1,text="Min. correlation:")
tkgrid(tklabelf3,textfield3); 
xvar<- tclVar("")
tkgrid.configure(tklabelf3,sticky="e");tkgrid.configure(textfield3,sticky="w");
tkgrid(tklabel(frame1,text=" "))

button5<-tkbutton(frame1, text="Plot dynamics of frequent sets", command=function() {
if(nchar(as.character(tclvalue(frm_frsetv1)))==0){
tkmessageBox(message = "Please, set up frequent set!", icon = "error", type = "ok")
return()
} else {
freqsets(tclvalue(frm_frsetv1),
fts_chart=tclvalue(frm_chstocksymbl),
rollmean_p=as.numeric(tclvalue(frm_rollmean_period1)),
rollmean_p2=as.numeric(tclvalue(frm_rollmean_period2)))
}
}
)

tkgrid(button5,columnspan=2, pady = 3,padx=20); 
tkgrid(tklabel(frame1,text="Frequent set (terms separated by space):"),columnspan=2, pady = 3,padx=20);
frm_frsetv1<- tclVar("")
textfield4=tkentry(frame1, textvariable=frm_frsetv1,width=35)
tkgrid(textfield4,columnspan=2, pady = 3,padx=20);
frm_rollmean_period1<- tclVar(rollmean_period)
frm_rollmean_period2<- tclVar(rollmean_period2)
tklabelm1=tklabel(frame1,text="Moving average 1:")
textfieldm1=tkentry(frame1, textvariable=frm_rollmean_period1,width=5)
tkgrid(tklabelm1,textfieldm1); tkgrid.configure(tklabelm1,sticky="e");tkgrid.configure(textfieldm1,sticky="w");
tklabelm2=tklabel(frame1,text="Moving average 2:")
textfieldm2=tkentry(frame1, textvariable=frm_rollmean_period2,width=5)
tkgrid(tklabelm2,textfieldm2); tkgrid.configure(tklabelm2,sticky="e");tkgrid.configure(textfieldm2,sticky="w");
tklabelfs1=tklabel(frame1,text="Stock symbol:")
frm_chstocksymbl <- tclVar("None")
dropdownfieldfs1 <- tkwidget(frame1,"ComboBox",editable=FALSE,values=c(stocksmbl,"None"), textvariable=frm_chstocksymbl)
tkgrid(tklabelfs1,dropdownfieldfs1); tkgrid.configure(tklabelfs1,sticky="e");tkgrid.configure(dropdownfieldfs1,sticky="w");

button_chrt<-tkbutton(frame1, text="Chart with candles, volumes & ARIMA forecasting", command=function(){
if(tclvalue(frm_chstocksymbl)=="None"){
tkmessageBox(message = "Please, choose stock symbol!", icon = "error", type = "ok")
return()
}
freqsets(tclvalue(frm_frsetv1),
fts_chart=tclvalue(frm_chstocksymbl),
rollmean_p=as.numeric(tclvalue(frm_rollmean_period1)),
rollmean_p2=as.numeric(tclvalue(frm_rollmean_period2)));
stock(as.character(tclvalue(frm_chstocksymbl)),fr_list_str=tclvalue(frm_frsetv1))} )
tkgrid(button_chrt,pady = 20,padx=10,columnspan=2)

tkgrid(frame1, ipady = 10,ipadx=10)
tkgrid.configure(frame1,pady = 20,ipadx=20)
tkgrid(mainframe,ipady = 10,ipadx=10)
}
main_gui()
