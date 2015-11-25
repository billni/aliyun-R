fun_update_main_cost <- function() {

gc()
starttime <- Sys.time()
dir <- "/var/R"
file <- paste(dir, "/aliyun-stock/res/stocklist.txt", sep="")
###########################################
stocklist <- read.csv(file, header=T, colClasses=c("character","character"), col.names=c("code","name"))
stocklist <- stocklist[substr(stocklist$code,1,1)!=3,]
stocklist <- stocklist[order(stocklist$code),]
checkmarket <- function(stock) {
  ifelse(substr(stock, 1, 1)==6, "sh", "sz")
}
stocklist$market <- apply(stocklist[,1,drop=F], 1, checkmarket)
stocklist$market <- paste(stocklist$code, stocklist$market, sep=".")

getmaincost <- function(stock, i) {
  library(RCurl)
  library(XML)  
  d <- debugGatherer()
  html.url <- paste("http://stockdata.stock.hexun.com/zlkp/s", stock[i, "code"], ".shtml", sep="")
  html.page <- htmlTreeParse(getURL(html.url, .encoding="utf-8", .opts = list(debugfunction=d$update,verbose = TRUE)), useInternalNode=T)
  html.content.1 <- getNodeSet(doc=html.page, path = "//div[@class='s_box']//p[@class='text_01']")
  s <- sapply(html.content.1, xmlValue, encoding="utf-8")
  #yyyymmdd
  start <- regexpr("交易日" , s[1])
  end <- gregexpr("日" , s[1])  
  stock[i, "control_date"] <- substr(s[1], start+3, end[[1]][2]) 
  stock[i, "control_date"]  <- gsub("年", "-", stock[i, "control_date"])
  stock[i, "control_date"]  <- gsub("月", "-", stock[i, "control_date"])
  stock[i, "control_date"]  <- gsub("日", "", stock[i, "control_date"])
    
  # main control level
  start <- regexpr("该股为" , s[1])
  end <- regexpr("盘" , s[1])
  stock[i, "control_status"] <- substr(s[1], start+3, end) 
    
  # main cost value
  start <- regexpr("主力成本" , s[1])
  end <- regexpr("元" , s[1])
  stock[i, "main_cost_value"]<- as.numeric(substr(s[1], start+4, end-1))

  html.content.2 <- getNodeSet(doc=html.page, path = "//div[@class='s_box']/h3[@class='title_01']")
  s <- sapply(html.content.2, xmlValue, encoding="utf-8")
  #股票名称
  start <- 1
  end <- regexpr("主力" , s[1])
  stock[i, "name"] <- substr(s[1], start, end-1) 
  
  stock[i,]
}


  ################并发获取主力成本###########################
  library(foreach)
  library(doParallel)
  n <- nrow(stocklist)
  
  cl <- makeCluster(5)
  registerDoParallel(cl)
  stock.main.cost <- foreach(j=1:n, .combine="rbind", .errorhandling="remove") %dopar% getmaincost(stocklist, j)
  stopCluster(cl)
  print(Sys.time()-starttime)
  stock.main.cost

  path <- paste(dir, "/aliyun-stock/rawdata/", sep="")
  filename <- paste(path, Sys.Date(), ".txt", sep="")
  write.table(stock.main.cost, filename, row.names = F)
  return(stock.main.cost)
}