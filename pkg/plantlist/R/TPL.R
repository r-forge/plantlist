TPL <- function(plant.names = NULL){

    options(stringsAsFactors = FALSE ) ## 
    if(is.null(plant.names)){
        stop("At least one plant species or genus should be provided.")
    }
    
    get.genus <- function(x){  ### Not only accept species , but also the genus name
        if(is.data.frame(x)){
            x <- as.vector(x)
        }
        first <- toupper(substr(x, 1, 1 ))  ## change the first alphabet to Upper Case.
        start.point <- 2
        if(min(regexpr(" ", x)) > 1){
             end.point <- regexpr(" ", x)
             second <- substr(x, start.point,  end.point -1)
        } else {
             x <- paste(x, " ")
             end.point <- regexpr(" ", x)
             second <- substr(x, start.point,  end.point -1)
        }
        ## the first white space
        
        return(paste(first, second, sep = ""))
    }

    check.syn.kew <- function(x, db){
    
        genera <- get.genus(x) ## Match the word in different cases. 
        genera <- data.frame(x, genera)
        res <- merge(x = genera, y = db, by.x = "genera", by.y = "Query", sort = FALSE, all.x = TRUE)
        res <- res[,c(2,1,3,4)]
        colnames(res) <- c("YourSpecies", "YourGenus", "KewFamily_Ext", "KewGenus_Ext")
        return(res)
    }
    
    kew.path <- system.file("extdata", "KEW_GENERA_EXT.csv", package = "plantlist")
    kew.base <- read.csv(kew.path,  header = TRUE, stringsAsFactors = FALSE)

    dd <- system.file("extdata", "TPL_GENERA.csv", package = "plantlist")
    plantlist.base <- read.csv(dd,  header = TRUE, stringsAsFactors = FALSE)
    
    kew <- check.syn.kew(x = plant.names, db = kew.base)  ## Check against Kew Database
    res.tpl.family <- merge(x = kew, y = plantlist.base, by.x = "KewGenus_Ext", by.y = "Genus", sort = FALSE, all.x = TRUE)

    tpl.orders.path <- system.file("extdata", "APG_ORDERS.csv", package = "plantlist")
    tpl.orders <- read.csv(tpl.orders.path, header = TRUE, stringsAsFactors = FALSE)
    res.tpl.family.orders <- merge(x = res.tpl.family, y = tpl.orders, by = "TPLFamily", sort = FALSE, all.x = TRUE)
    
    return(res.tpl.family.orders[,c(3,4,2,5,1,6,7)])
}
