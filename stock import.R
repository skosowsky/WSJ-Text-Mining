library(stringr)
library(pander)
clean.data <- read.csv("~/Fall 2/Text Mining/clean.data.csv", stringsAsFactors=FALSE)


excess.return <-vector(length=1532)
for(i in 1:1532){
  
  tryCatch({getSymbols.google(Symbols = clean.data$Ticker[i],env = .GlobalEnv,src="google",from = clean.data$Date[i], to = clean.data$Date[i])
  }, error = function(e){})
  getSymbols.google(Symbols = "SPY",env = .GlobalEnv,src="google",from = clean.data$Date, to = clean.data$Date)
  if(exists(clean.data$Ticker[i])){
    if(is.na(as.numeric(get(clean.data$Ticker[i]))[1])){ 
      excess.return[i] = "N/A"
      print(i)
      print(clean.data$Article[i])
    
    }else {
    excess.return[i] <-(as.numeric(dailyReturn(get(clean.data$Ticker[i])))-as.numeric(dailyReturn(get("SPY"))))*100
    rm(list = ls()[which(ls()!="clean.data" & ls()!="excess.return")])
    }
  }else {
    excess.return[i] = "N/A"
    print(i)
    print(clean.data$Article[i])}
}

final.set<-cbind(clean.data[which(excess.return!="N/A"),],as.numeric(excess.return[which(excess.return!="N/A")]))
colnames(final.set)[19]<-"excess.return"
Sentiment <- final.set[,9:18]
