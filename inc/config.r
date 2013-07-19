# Set up the list of twitter users
tw_search_usr=c("CNNMoney","TheStreet", "FoxBusiness","SeekingAlpha", "WallstCS","themotleyfool",
"MarketWatch","CNBC","ReutersBiz","WSJ","YahooFinance",
"MicroFundy","chartly","MarketBeat","ReutersTV","BloombergTV","profitly","myrollingstocks",
"BloombergNews","Stockstobuy","tradespoon","stockr","stocktwits",
"FinancialBrand", "Option_Trading","EconomicTimes")

# Set up the list of stock symbols
stocksmbl<-c("AAPL","YHOO", "ORCL","MSFT")

# Set up max number of tweets which can be loaded for each user
n_tw=300;

# Set up time windows for analysis (days)
xts_window=100

min_freq_terms=5;
min_corr_assoc=0.03
filename_fts="loaded_feeds/fin_time_series.txt"
filename="loaded_feeds/users_tweets.txt"
rollmean_period=10
rollmean_period2=20
relative_freq_frsets=TRUE
