###############################################################################
################热点概念和股票#################################################
###############################################################################
fun_2_concept <- function(stock) {
  gc()
  starttime <- Sys.time()
  
  concept <- read.table("concept.txt", header=T, colClasses=c("character","character"), col.names=c("name","code"), stringsAsFactors=F, fileEncoding="gb2312",  sep = ",")
  
  ####################################################################
  getconceptstock <- function(concept, i) {  
    library(RCurl)
    library(XML)
    d <- debugGatherer()
    stock.list.concept  <- data.frame()
    concept.url <- paste("http://quote.tool.hexun.com/hqzx/stocktype.aspx?columnid=5522&type_code=", concept[i,"code"], "&sorttype=3&updown=up&page=1&count=400", sep="")
    html.page <- getURL(concept.url, .encoding="gb2312", .opts = list(debugfunction=d$update,verbose = TRUE))
    html.page <- gsub("'","", html.page)
    html.page.trim <- substr(html.page, 14,  as.numeric(gregexpr("\\]]", html.page)))
    
    pattern <- "\\[([0-9]*),"
    m <- gregexpr(pattern, html.page.trim)
    x <- regmatches(html.page.trim, m)  
    concept.stock <- gsub("\\[", "" ,x[[1]])  
    concept.stock <- gsub(",$", "" , concept.stock)
    
    concept.stock.df <- read.csv(text=concept.stock, colClasses=c("character"), col.names=c("代码"))
    cbind(concept.stock.df, "概念"=concept[i, "name"], stringsAsFactors=F) 
  }
  
  ################Parllel Running###########################
  library(foreach)
  library(doParallel)
  n <- nrow(concept)
  
  cl <- makeCluster(5)
  registerDoParallel(cl)
  stock.concept <- foreach(j=1:n, .combine="rbind", .errorhandling="remove") %dopar% getconceptstock(concept, j)
  stopCluster(cl)
  filename <- paste("conceptlist/conceptlist_", Sys.Date(), ".csv", sep="")
  write.csv(stock.concept, filename, row.names=F, quote=F)
  print(Sys.time()-starttime)
  
  #####################################概念和股票关联###########################################3
  
  stock.concept.summary <- data.frame(by(stock.concept[,"概念"], stock.concept$"代码", paste, collapse=",")[])
  names(stock.concept.summary) <- "concept"
  stock.concept.summary <- data.frame("code"=row.names(stock.concept.summary), stock.concept.summary, stringsAsFactors=F)
  stock.concept.summary <- subset(stock.concept.summary, substr(stock.concept.summary$code,1,1)!=3)
  
  recommend <- stock.concept.summary
  for (i in 1:nrow(recommend)) {
    s <- stock.concept.summary[stock.concept.summary$code == recommend[i, "code"], "concept"]
    recommend[i, "concept"] <- ifelse(length(s)==0, "NA", s)
  }
  recommend
}