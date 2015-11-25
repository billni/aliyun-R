#############
### Merge all files
#############
fun_merge_main_cost <- function() {
    dir <- "/var/R"
    path <- paste(dir, "/aliyun-stock/rawdata/", sep="")
    stock.main.cost <- data.frame(code='000000', name='xxxxxx', control_date='0000-00-00',main_cost_value=0,  stringsAsFactors=F)
    files <- dir(path, full.names=T)
    for(filename in files) {
      file.data <- read.table(filename, header=T, blank.lines.skip = TRUE, stringsAsFactors=F,  colClasses=c("character","character","character"))  
      stock.main.cost <- rbind(stock.main.cost, file.data[,c(1,2,4,6)])
    }
    
    ##### remove first row which initialized by data.frame
    stock.main.cost <- stock.main.cost[-1,] 
    
    path <- paste(dir, "/aliyun-stock/output/main_cost.pdf", sep="")
    pdf(path)    
    opar <- par(no.readonly=T)
    par(lty=2, pch=19, mfrow=c(1,1))
    
    stockcode <- levels(as.factor(stock.main.cost[,1]))
    loopnum <-  length(stockcode)
    for(i in 1:loopnum) {
      target <- stock.main.cost[which(stock.main.cost$code==stockcode[i]),]
      plot(target[,4], type="b", xlab=target[1,2], ylab="Price", xlim=c(1,nrow(target)), xaxt="n")
      text(x=target[,4], labels=target[,3], pos=2, col="red", cex = 0.5)
      text(x=target[,4], labels=target[,4], pos=3, col="blue",  cex=0.7)      
      title(main="The Trend of Banker Control Cost")
    }
    
    par(opar)
    dev.list()
    dev.off(dev.cur())
    print(paste("Please to see pdf: ", path))
}