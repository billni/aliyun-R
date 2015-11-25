###################################大宗交易########################################
fun_4_tradeoff <- function(stocklist){
  gc()
starttime <- Sys.time()

###########################################
getmasstradeofff <- function(stock, i) {  
  library(RCurl)
  library(XML)  
  d <- debugGatherer()
  
  html.url <- paste("http://stockdata.stock.hexun.com/dzjy/outData/ggdzjy.ashx?code=", stock[i, "code"], "&count=1&cjd=180&page=1", sep="")
  html.page <- getURL(html.url, .encoding="gb2312", .opts = list(debugfunction=d$update,verbose = TRUE))
  pattern <- "time:'(\\d{4}-\\d{1,2}-\\d{1,2})',dealPre:'(\\d*.\\d*)'"
  m <- gregexpr(pattern, html.page)  
  tradeoff <- data.frame("code"= stock[i, "code"], "tradeoff"=regmatches(html.page, m)[[1]], stringsAsFactors=F)  
  tradeoff
}


################并发执行###########################
library(foreach)
library(doParallel)
recommend <- stocklist
n <- nrow(recommend)

cl <- makeCluster(5)
registerDoParallel(cl)
recommend.mass.tradeoff <- foreach(j=1:n, .combine="rbind", .errorhandling="remove") %dopar% getmasstradeofff(recommend, j)
stopCluster(cl)
#filename <- paste("masstradeoff/tradeoff_", Sys.Date(), ".csv", sep="")
#write.csv(recommend.mass.tradeoff, filename, row.names=F, quote=F)
print(Sys.time()-starttime)


#####################合并数据#####################################
for (i in 1:nrow(recommend)) {
  s <- recommend.mass.tradeoff[recommend.mass.tradeoff$code == recommend[i, "code"], "tradeoff"]
  recommend[i, "tradeoff"] <- ifelse(length(s)==0, "NA", s)
}
recommend
}