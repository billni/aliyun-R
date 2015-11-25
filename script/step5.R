################获取最新价格##################################
fun_5_lastedValue <- function(stocklist){
gc()
starttime <- Sys.time()

getlastestvalue <- function(stock, i){
  library(RCurl)
  library(XML)  
  d <- debugGatherer()
  real.stock.url <- paste("http://bdcjhq.hexun.com/quote?s2=", stock[i, "market"], sep="")  
  real.stock.page <- htmlTreeParse(getURL(real.stock.url, .opts = list(debugfunction=d$update, verbose = TRUE)), useInternalNode=T)
  real.stock.content <- getNodeSet(doc=real.stock.page, path = "//script")
  content <- getChildrenStrings(real.stock.content[[2]])
  text <- enc2utf8(content)
  start <- regexpr("pc", text)
  if (start != -1) {
    end <-  regexpr("time", text)
    text <- substring(text, start, end-2)
    text <- gsub("\"", "", text)
    text <- gsub("na", "name", text)
    text <- gsub("pc", "yesterday", text)
    text <- gsub("op", "open", text)
    text <- gsub("vo", "volumn", text)
    text <- gsub("tu", "turn_volumn", text)
    text <- gsub("hi", "high", text)
    text <- gsub("lo", "low", text)
    text <- gsub("la", "lastest_value", text)
    text <- gsub("type", "type", text)
    text <- gsub("time", "time", text)
    text <- gsub("sy", "pe", text)
    text <- gsub("lt", "circulating_shares", text)
    text <- gsub("sz", "market_value", text)
    text <- gsub("hs", "turnover_rate", text)
    splitedtext <- strsplit(text, ",")
    ltext <- lapply(splitedtext, "strsplit", ":")
    stock[i, "yesterday"]<- ltext[[1]][[1]][2]
    stock[i, "open"] <- ltext[[1]][[2]][2]
    stock[i, "volumn"] <- ltext[[1]][[3]][2]
    stock[i,"turn_volumn"] <- ltext[[1]][[4]][2]
    stock[i, "high"] <- ltext[[1]][[5]][2]
    stock[i, "low"] <- ltext[[1]][[6]][2]
    stock[i, "lastest_value"] <- ltext[[1]][[7]][2]
    stock[i, "type"] <- ltext[[1]][[8]][2]
    stock[i,]
  } else {
    gc()
  }
}

#########################并发执行##################################
library(foreach)
library(doParallel)
n <- nrow(stocklist)

cl <- makeCluster(5)
registerDoParallel(cl)
stock.lastest.value <- foreach(j=1:n, .combine="rbind", .errorhandling="remove") %dopar% getlastestvalue(stock.main.cost, j)
stopCluster(cl)
print(Sys.time()-starttime)


######################合并数据######################
filename <- paste("stocklist/stocklist_", Sys.Date(), ".csv", sep="")
write.csv(stock.main.cost, filename, row.names=F, quote=F)

stock.lastest.value$ratio <- round((as.numeric(stock.lastest.value$lastest_value) - as.numeric(stock.lastest.value$main_cost_value))/as.numeric(stock.lastest.value$main_cost_value),3)
recommend <- stock.lastest.value[stock.lastest.value$lastest_value>1 & stock.lastest.value$ratio<0.07, c("code", "name", "main_cost_value","lastest_value", "ratio", "控盘程度")]
recommend <- recommend[order(recommend$code),]

recommend
}