this.dir <- dirname(parent.frame(2)$ofile); setwd(this.dir);options(warn=-1); Sys.setlocale("LC_TIME", "English");
library(twitteR) 
library(tm) 
library("tseries") 
library("zoo") 
library("xts") 
library("forecast")
library("quantmod")
library("wordcloud")
source("inc/config.r")
source("inc/functions_tw.r")
source("inc/functions_txt.r")
source("inc/functions_analysis.r")
source("inc/functions_f_ts.r")
source("inc/functions_tcltk.r")

 

 

 
