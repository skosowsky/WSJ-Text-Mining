# library(tm)
# library(RTextTools)
# library(syuzhet)
# library(dplyr)
# library(stringr)
# library(pander)

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

is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

constituents <- read.csv("C:\\Users\\Sean\\Documents\\Fall 2\\Text Mining\\Nonsense\\constituents.csv", stringsAsFactors=FALSE )
sectors <- unique(constituents$Sector)

Sentiment <- list()
Valence <- list()
txt.file.list<-list()
for(i in 1:10){
  setwd("C:\\Users\\Sean\\Documents\\Fall 2\\Text Mining\\output text")
  setwd(paste(getwd(),as.character(sectors[i]),sep = "/"))
  print(getwd())
  txt.file.list[[i]] <- list()
  Sentiment[[i]] <- list()
  Valence[[i]] <- list()
  names(txt.file.list)[i] <- sectors[i]
  names(Sentiment)[i] <- sectors[i]
  names(Valence)[i] <- sectors[i]
  if(length(list.dirs())>1){
    for(j in 2:length(list.dirs())){
       txt.file.list[[i]][[j-1]] <- list()
       Sentiment[[i]][[j-1]] <- list()
       Valence[[i]][[j-1]] <- list()
       setwd("C:\\Users\\Sean\\Documents\\Fall 2\\Text Mining\\output text")
       setwd(paste(getwd(),as.character(sectors[i]),sep = "/"))
       names(txt.file.list[[i]])[j-1]<-substr(list.dirs()[j],3,nchar(list.dirs()[j]))
       names(Sentiment[[i]])[j-1]<-substr(list.dirs()[j],3,nchar(list.dirs()[j]))
       names(Valence[[i]])[j-1]<-substr(list.dirs()[j],3,nchar(list.dirs()[j]))
       setwd(paste(getwd(),list.files()[j-1],sep = "/"))
       files <- list.files(pattern = "[.]txt$")
       print(getwd())
          if(length(list.files())>1){
            for(k in 2:length(files)){
              txt.file.list[[i]][[j-1]][[k-1]]<- unlist(lapply(files[k], readLines,n=-1L))
              names(txt.file.list[[i]][[j-1]])[k-1]<-files[k]
              
              txt.file.list[[i]][[j-1]][[k-1]]<-rm.url(txt.file.list[[i]][[j-1]][[k-1]])
              txt.file.list[[i]][[j-1]][[k-1]]<-rm.phone.number(txt.file.list[[i]][[j-1]][[k-1]])
              txt.file.list[[i]][[j-1]][[k-1]]<-rm.len.zero(txt.file.list[[i]][[j-1]][[k-1]])

              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,1,6)=="SOURCE")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,1,6)=="Access")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,1,5)=="Visit")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,1,8)=="Write to")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,1,15)=="NOTE TO EDITORS")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,1,1)=="*")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,1,13)=="Media Contact")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,1,13)=="Email Contact")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,1,9)=="About The")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,1,21)=="InvestorsObserver.com")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,1,11)=="View source")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,7,10)=="http")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,6,9)=="http")] = ""
              txt.file.list[[i]][[j-1]][[k-1]][which(lapply(txt.file.list[[i]][[j-1]][[k-1]],substr,1,6)=="To see")] = ""
              txt.file.list[[i]][[j-1]][[k-1]]<-str_replace_all(txt.file.list[[i]][[j-1]][[k-1]], "[[:punct:]]", "")
              txt.file.list[[i]][[j-1]][[k-1]]<-removeWords(txt.file.list[[i]][[j-1]][[k-1]],
                                                c("Comments on the News","Methodology","Additional resources","About Salesforce",
                                                "Additional Information","NO WARRANTY", "NOT AN OFFERING", "RESTRICTIONS",
                                                "Contact Information:","Media Contact","MEDIA","INVESTOR"))
              
              txt.file.list[[i]][[j-1]][[k-1]] <- gsub("\n","",gsub('\"',"",txt.file.list[[i]][[j-1]][[k-1]]))              
              if(length(which(txt.file.list[[i]][[j-1]][[k-1]]=="" | is.na(txt.file.list[[i]][[j-1]][[k-1]])))>0){
              txt.file.list[[i]][[j-1]][[k-1]][min(which(txt.file.list[[i]][[j-1]][[k-1]]=="" | is.na(txt.file.list[[i]][[j-1]][[k-1]]))):length(txt.file.list[[i]][[j-1]][[k-1]])]<-list(NULL)}
              txt.file.list[[i]][[j-1]][[k-1]]<-unlist(txt.file.list[[i]][[j-1]][[k-1]])
             
              if(length(txt.file.list[[i]][[j-1]][[k-1]])<4){txt.file.list[[i]][[j-1]][[k-1]]<-list(NULL)}
              if(is.null(unlist(txt.file.list[[i]][[j-1]][[k-1]][3:max(which(is.na(txt.file.list[[i]][[j-1]][[k-1]])==F & txt.file.list[[i]][[j-1]][[k-1]]!=""))]))==F){
              
              #Sentiment[[i]][[j-1]][[k-1]] <-list()
              #names(Sentiment[[i]][j-1])[k-1] <- files[k]
              #Valence[[i]][[j-1]][[k-1]]<-list()
              #names(Valence[[i]])[j-1] <- files[k]
              #Sentiment[[i]][[j-1]][[k-1]]<-matrix(nrow = length(txt.file.list[[i]][[j-1]][[k-1]][3:max(which(is.na(txt.file.list[[i]][[j-1]][[k-1]])==F & txt.file.list[[i]][[j-1]][[k-1]]!=""))]),ncol = 10)
              #Valence[[i]][[j-1]][[k-1]]<-matrix(nrow = length(txt.file.list[[i]][[j-1]][[k-1]][3:max(which(is.na(txt.file.list[[i]][[j-1]][[k-1]])==F & txt.file.list[[i]][[j-1]][[k-1]]!=""))]), ncol=2)
              
              sent.temp<-get_nrc_sentiment(txt.file.list[[i]][[j-1]][[k-1]][3:max(which(is.na(txt.file.list[[i]][[j-1]][[k-1]])==F & txt.file.list[[i]][[j-1]][[k-1]]!=""))])
              sent<- list()
              
              Sentiment[[i]][[j-1]][[k-1]] <- sent.temp 
              Valence[[i]][[j-1]][[k-1]] <- (Sentiment[[i]][[j-1]][[k-1]][9]*-1) + Sentiment[[i]][[j-1]][[k-1]][10]
              names(Sentiment[[i]][[j-1]])[k-1] <- files[k]
              names(Valence[[i]][[j-1]])[k-1] <- files[k]
              append(sent,assign(paste(names(Sentiment[[i]][[j-1]])[k-1],names(Sentiment[[i]])[j-1],names(Sentiment)[i],sep = " - "),sent.temp),after=length(sent))
              } else {txt.file.list[[i]][[j-1]][[k-1]] <-list(NULL)}
          #k
            }
          #if   
          } else{txt.file.list[[i]][[j-1]][[k-1]]<-list(NULL)}
    #j
    }
  #if
  } else{txt.file.list[[i]][[j-1]]<-list(NULL)}
#i
  txt.file.list[[i]] <-rmNullObs(txt.file.list[[i]])
  Sentiment[[i]] <-rmNullObs(Sentiment[[i]])
  Valence[[i]] <-rmNullObs(Valence[[i]])
}

x1<-as.array(Sentiment)


X<- rmNullObs(txt.file.list)

txt.file.list[[i]][[j-1]][[k-1]]
docs[3:length(docs[,k]),k] %>%
  .[which(is.na(.)==FALSE)] %>%
  paste(. , sep = " " , collapse="") %>%
  sub("\\s+$", "", . , perl = TRUE) %>%
  gsub('\"','',.)


