CTPL <- function(file = NULL){
    if(is.null(file)){
       stop("The TXT file containing Chinese Plant Names must be provided!")
    }
    options(stringsAsFactors = FALSE)
    cn.path <- system.file("extdata", "CN.csv", package = "plantlist")
    dat <- read.csv(cn.path, header = TRUE)
    plant.names <- read.csv(file, header = FALSE)
    res <- merge(x = plant.names, y = dat, by.x = "V1", by.y = "NAME_CN", sort = FALSE, all.x = TRUE)
    write.csv(res, "CTPL_results.csv")
    cat("The file \"CTPL_results.csv\" has been saved to", getwd(), "\n")
}
