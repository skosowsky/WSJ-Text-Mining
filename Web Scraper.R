# library(RSelenium)
# library(tm)
# library(RTextTools)
# library(syuzhet)
# library(dplyr)
# library(stringr)
# library(pander)

constituents <- read.csv("C:\\Users\\Sean\\Documents\\Fall 2\\Text Mining\\output text\\Nonsense\\constituents.csv", stringsAsFactors=FALSE )

startServer(invisible = TRUE)
mybrowser <- remoteDriver()
mybrowser$open()
mybrowser$navigate("http://www.wsj.com/login")

usernamebox <- mybrowser$findElement(using = 'css selector', "#username")
pwbox <- mybrowser$findElement(using = 'css selector', "#password")
usernamebox$sendKeysToElement(list(####MYEMAIL#####))
pwbox$sendKeysToElement(list(#####MYPASSWORD######))
loginbutton <- mybrowser$findElement(using = 'css selector', "#submitButton")
loginbutton$clickElement()
Sys.sleep(5)




q=180
tryCatch({
for(p in q:(dim(constituents)[1])){
  
  print(p)
  if( p %% 10 == 9){Sys.sleep(61)}
tryCatch({  

output.dir <-"C:\\Users\\Sean\\Documents\\Fall 2\\Text Mining\\output text"
setwd("C:\\Users\\Sean\\Documents\\Fall 2\\Text Mining\\output text")
ticker=as.character(constituents$Symbol[p])
print(constituents$Sector[p])
print(ticker)
if(dir.exists(paste(output.dir,ticker,sep = "\\"))){
  setwd(paste(output.dir,constituents[which(constituents$Symbol==ticker),3],
              paste(constituents$Name[which(constituents$Symbol==ticker)]," - ",constituents$Symbol[which(constituents$Symbol==ticker)],sep="\\")))
} else{
  dir.create(paste(output.dir,constituents[which(constituents$Symbol==ticker),3],sep = "\\"))
  setwd(paste(output.dir,constituents[which(constituents$Symbol==ticker),3],sep = "\\"))
  dir.create(paste(output.dir,constituents[which(constituents$Symbol==ticker),3],
                   paste(constituents$Name[which(constituents$Symbol==ticker)]," - ",constituents$Symbol[which(constituents$Symbol==ticker)],sep=""),sep = "\\"))
  setwd(paste(output.dir,constituents[which(constituents$Symbol==ticker),3],
              paste(constituents$Name[which(constituents$Symbol==ticker)]," - ",constituents$Symbol[which(constituents$Symbol==ticker)],sep=""),sep = "\\"))
}

mybrowser$navigate("http://www.wsj.com/public/page/news-stock-market-movers.html")


searchbox<- mybrowser$findElement(using = 'css selector', "#globalHeaderSearchInput")
searchbox$clickElement()
searchbox$sendKeysToElement(list(ticker))

searchbox$highlightElement()
searchbutton <- mybrowser$findElement(using = 'css selector', ".hdrSearchBtn")
searchbutton$clickElement()


news_module <- mybrowser$findElements(using = 'css selector', ".headline")
linktext <- unlist(lapply(news_module, function(x){x$getElementText()}))
linktext<-unique(linktext[which(linktext!="")])

links<-vector(length=length(linktext))
for(i in 1:length(links)){
  links[i]<-mybrowser$findElement(using="link text",linktext[i])$getElementAttribute('href')[[1]]
}
links<-links[which((substr(links,12,14)=="wsj" & substr(links,20,24)!="video") | substr(links,12,22)=="marketwatch")]
if(length(links)==1){next}
linktext<-linktext[which((substr(links,12,14)=="wsj" & substr(links,20,24)!="video") | substr(links,12,22)=="marketwatch")]

#fileConn <- file("000Articles Read.txt")
#print(fileConn)
# if(isOpen(fileConn)){
# writeLines(linktext, fileConn)}
# close(fileConn)
write(linktext,"000Articles Read.txt")

text_files<-list()
for(j in 1:length(links)){

  mybrowser$navigate(links[j])
  if(substr(links[j],12,14)=="wsj"){
    read_text <- mybrowser$findElements(using = 'css selector', ".timestamp , .wsj-article-headline , #wsj-article-wrap p")
  } 
  if(substr(links[j],12,22)=="marketwatch"){
    read_text <- mybrowser$findElements(using = 'css selector', "#article-body p , #published-timestamp span , #article-headline")
  }
  temp<-unlist(lapply(read_text, function(x){x$getElementText()}))
  temp[1] <- str_replace_all(temp[1], "[[:punct:]]", " ")
  if (length(temp[2])>0 & length(temp[1]>0) ){
    if(nchar(temp[1])>100){temp[1]<-substr(temp[1],1,75)}
    if(substr(temp[2],1,7)=="Updated"){temp[2] <- substr(temp[2],8,nchar(temp[2]))}
    if(is.na(as.numeric(as.POSIXct(gsub("\\.","",temp[2]), "%b %d, %Y %I:%M %p", tz= "EST")))
       & is.na(as.POSIXct(gsub("\\.","",substr(temp[2],12,nchar(temp[2]))), "%b %d, %Y %I:%M %p", tz= "EST"))){
      text_files[[j]]=NULL
    } else if(length(temp)>3){
        print(temp[1])
        print(temp[2])
         
if(is.na(as.numeric(as.POSIXct(gsub("\\.","",temp[2]), "%b %d, %Y %I:%M %p", tz= "EST")))){
     # fileConn<-file(paste(as.numeric(as.POSIXct(gsub("\\.","",temp[2]), "%b %d, %Y %I:%M %p", tz= "EST")),
     #                      " - ", format(as.POSIXct(gsub("\\.","",temp[2]), "%b %d, %Y %I:%M %p", tz= "EST"),"%Y-%m-%d %Hh%Mm"), " - ", temp[1],".txt",sep = ""))
     # print(fileConn)
     # if(isOpen(fileConn)){
     # writeLines(temp, fileConn)}
     # close(fileConn)
  write(temp,paste(as.numeric(as.POSIXct(gsub("\\.","",temp[2]), "%b %d, %Y %I:%M %p", tz= "EST")),
        " - ", format(as.POSIXct(gsub("\\.","",temp[2]), "%b %d, %Y %I:%M %p", tz= "EST"),"%Y-%m-%d %Hh%Mm"), " - ", temp[1],".txt",sep = ""))
     } else{
     # fileConn<-file(paste(as.numeric(as.POSIXct(gsub("\\.","",temp[2]), "%b %d, %Y %I:%M %p", tz= "EST")),
     #                      " - ", format(as.POSIXct(gsub("\\.","",temp[2]), "%b %d, %Y %I:%M %p", tz= "EST"),"%Y-%m-%d %Hh%Mm"), " - ", temp[1],".txt",sep = ""))
     # print(fileConn)
     # if(isOpen(fileConn)){
     # writeLines(temp, fileConn)}
     # close(fileConn)
  write(temp,paste(as.numeric(as.POSIXct(gsub("\\.","",temp[2]), "%b %d, %Y %I:%M %p", tz= "EST")),
        " - ", format(as.POSIXct(gsub("\\.","",temp[2]), "%b %d, %Y %I:%M %p", tz= "EST"),"%Y-%m-%d %Hh%Mm"), " - ", temp[1],".txt",sep = ""))
     } 
       }
  } else{text_files[[j]]=NULL}
 
  Sys.sleep(2)
#for
}
#TryCatch
})
#for
}
#TryCatch
})

# runwsj()
# running(q=99)
# 
# running.handler<- function(q){
# runwsj()
# tryCatch(running(), error = function(e){
#   mybrowser$close()
#   runwsj()
#   running(q=w,error = 1)
# })
# } 

######################################
##############TEXT MINING##############

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

is.phone.number <- function(phone.number){
  len = length(phone.number)
  ret <- vector(length = len)
  ret[which(substr(phone.number,4,4)=="-" & substr(phone.number,8,8)=="-")] <- TRUE
  ret[which(ret!=TRUE)] <- FALSE
  return(ret)
}

is.url <- function(url){
  len = length(url)
  ret <- vector(length = len)
  ret[which(substr(url,1,4)=="www." | substr(url,1,4)=="http" | substrRight(url,4)==".com")] <- TRUE
  ret[which(ret!=TRUE)] <- FALSE
  return(ret)
}

which.phone.number <- function(phone.number){
  which(is.phone.number(phone.number)==TRUE)
}

which.url <- function(url){
  which(is.url(url)==TRUE)
}

rm.phone.number <- function(text){
  len = length(text)
  v = vector(length= len)
  text[which.phone.number(text)] = list(NULL)
  text = unlist(text)
  v[1:length(text)] = text
  v[which(v==FALSE)] = NA
  return(v)
}

rm.url <- function(text){
 len = length(text)
 v = vector(length= len)
 text[which.url(text)] = list(NULL)
 text = unlist(text)
 v[1:length(text)] = text
 v[which(v==FALSE)] = NA
 return(v)
}

first.null <- function(text){
  min(which(is.null(text)==T))
}

rm.len.zero <- function(text){
  len <- length(text)
  v <- vector(length = len)
  if(min(nchar(text))!=0){return(text)}
  else{
  first.zero <- min(which(nchar(text)==0))
  text[first.zero:len] = list(NULL)
  text <- unlist(text)
  v[1:length(text)] <- text
  v[which(v==FALSE)] = NA
  return(v)}
}

#doc1 <- readLines(con = fileName, n = -1L, skipNul = TRUE)


length = vector(length = length(list.files()))
for(k in 1:length(list.files())){
  length[k] <- length(readLines(con = list.files()[k], n = -1L, skipNul = T))
}

docs<-data.frame(nrow = max(length), ncol=length(list.files()))
data <- data.frame(nrow = 2,ncol = length(list.files()))
for(k in 1:length(list.files())){
  
  docs[(1:length[k]),k] <- removeWords(unlist(readLines(con = list.files()[k], n = -1L, skipNul = T)),
                           c("Comments on the News","Methodology","Additional resources","About Salesforce",
                             "Additional Information","NO WARRANTY", "NOT AN OFFERING", "RESTRICTIONS",
                             "Contact Information:","Media Contact:"))
  
  docs[which(unlist(lapply(docs[,k],substr,1,6))=="Access"),k] = ""
  docs[which(unlist(lapply(docs[,k],substr,1,5))=="Visit"),k] = ""
  docs[which(unlist(lapply(docs[,k],substr,1,8))=="Write to"),k] = ""
  docs[which(unlist(lapply(docs[,k],substr,1,15))=="NOTE TO EDITORS"),k] = ""
  docs[which(unlist(lapply(docs[,k],substr,1,1))=="*"),k] = ""
  docs[which(unlist(lapply(docs[,k],substr,1,13))=="Media Contact"),k] = ""
  docs[which(unlist(lapply(docs[,k],substr,1,13))=="Email Contact"),k] = ""
  docs[which(unlist(lapply(docs[,k],substr,1,9))=="About The"),k] = ""
  docs[which(docs[,k]=="--"),k] = ""
  if(length[k]<max(length[k])){
    docs[((length[k]+1)):max(length),k] = NULL
  }
  docs[,k] <- rm.url(docs[,k])
  docs[,k] <- rm.phone.number(docs[,k])
  docs[,k] <- rm.len.zero(docs[,k])
  colnames(docs)[k]<-paste("doc",k)

if(substr(docs[2,k],1,7)=="Updated"){
  docs[2,k] <-gsub("Updated","",docs[2,k])
}
  
data[1,k] <- docs[2,k] %>%
             sub("\\s+$", "", . , perl = TRUE) #%>%
             #gsub("\r", "", . )
data[2,k] <- docs[3:length(docs[,k]),k] %>%
             .[which(is.na(.)==FALSE)] %>%
             paste(. , sep = " " , collapse="") %>%
             sub("\\s+$", "", . , perl = TRUE) %>%
             gsub('\"','',.)
            
colnames(data)[k] <- docs[1,k] %>%
                     sub("\\s+$", "", . , perl = TRUE)
}


#lapply(docs[3:13,], get_sentiment,method = "afinn")
Sentiment <- list()
Valence <- list()
for(i in 1:ncol(docs)){
  Sentiment[[i]] <- get_nrc_sentiment(docs[3:max(which(is.na(docs[,i])==F)),i])
  Valence[[i]] <- (Sentiment[[i]][,9]*-1) + Sentiment[[i]][,10]
}
for(i in 1:ncol(docs)){
  
}
sent1 <- 
pander::pandoc.table(sent1[, 1:8])
pander::pandoc.table(sent1[, 9:10])
valence <- 
barplot(
  sort(colSums(prop.table(sent1[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)

barplot(
  sort(colSums(prop.table(sent1[, 9:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)

pdf("myOut.pdf")
for (i in 1:ncol(docs)){
  print(docs[1,i])
  print(docs[2,i])
  print("\n")
  print(data[2,i])
  barplot(
    sort(colSums(prop.table(Sentiment[[i]][, 1:8]))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = paste("Emotions in Article",i), xlab="Percentage"
  )
  
  barplot(
    sort(colSums(prop.table(Sentiment[, 9:10]))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = paste("Emotions in Article",i), xlab="Percentage"
  )
}
dev.off()
