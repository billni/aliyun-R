####################################################################################################
#######################################地域和股票关联###############################################

fun_3_region <- function(stocklist) {
  gc()
starttime <- Sys.time()

province <- read.table("province.txt", header=T, colClasses=c("character","character"), stringsAsFactors=F, fileEncoding="gb2312",  sep = ",")

getprovincestock <- function(province, i) {  
  library(RCurl)
  library(XML)
  d <- debugGatherer()
  province.url <- paste("http://quote.tool.hexun.com/hqzx/stocktype.aspx?columnid=5519&type_code=", province[i, "code"], "&sorttype=3&updown=up&page=1&count=400&time=1103" , sep="")
  html.page <- getURL(province.url, .encoding="gb2312", .opts = list(debugfunction=d$update,verbose = TRUE))
  html.page <- gsub("'","", html.page)
  html.page.trim <- substr(html.page, 14,  as.numeric(gregexpr("\\]]", html.page)))
  
  pattern <- "\\[([0-9]*),"
  m <- gregexpr(pattern, html.page.trim)
  x <- regmatches(html.page.trim, m)  
  province.stock <- gsub("\\[", "" ,x[[1]])  
  province.stock <- gsub(",$", "" , province.stock)
  
  province.stock.df <- read.csv(text=province.stock, colClasses=c("character"), col.names=c("代码"))
  cbind(province.stock.df, "地域"=province[i, "name"], stringsAsFactors=F)  
}

################Parllel Running###########################
library(foreach)
library(doParallel)
n <- nrow(province)

cl <- makeCluster(5)
registerDoParallel(cl)
stocklist.province <- foreach(j=1:n, .combine="rbind", .errorhandling="remove") %dopar% getprovincestock(province, j)
stopCluster(cl)
filename <- paste("provincelist/provincelist_", Sys.Date(), ".csv", sep="")
write.csv(stocklist.province, filename, row.names=F, quote=F)
print(Sys.time()-starttime)

for (i in 1:nrow(stocklist)) {
  s <- stocklist.province[stocklist.province$"代码" == stocklist[i, "code"], "地域"]
  stocklist[i, "province"] <- ifelse(length(s)==0, "NA", s)
}

stocklist
}