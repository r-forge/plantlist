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
    
    kew.path <- system.file("extdata", "KEW_GENERA_EXT.csv", package = "plantlist")
    kew.db <- read.csv(kew.path,  header = TRUE, stringsAsFactors = FALSE)

    dd <- system.file("extdata", "TPL_GENERA.csv", package = "plantlist")
    plantlist.db <- read.csv(dd,  header = TRUE, stringsAsFactors = FALSE)
    
    genus <- get.genus(plant.names) ## Match the word in different cases. 
    genus <- data.frame(YOUR_SPECIES = plant.names, YOUR_GENUS = genus)
    res0 <- merge(x = genus, y = kew.db, by = "YOUR_GENUS", by.y = "QUERY", sort = FALSE, all.x = TRUE)
    
    res1 <- merge(x = res0, y = plantlist.db, by.x = "KEW_GENUS_EXT", by.y = "GENUS", sort = FALSE, all.x = TRUE)

    tpl.orders.path <- system.file("extdata", "APG_ORDERS.csv", package = "plantlist")
    tpl.orders <- read.csv(tpl.orders.path, header = TRUE, stringsAsFactors = FALSE)
    res <- merge(x = res1, y = tpl.orders, by = "TPL_FAMILY", sort = FALSE, all.x = TRUE)
    return(data.frame(YOUR_SPECIES = res$YOUR_SPECIES, YOUR_GENUS = res$YOUR_GENUS, TPL_STATUS = res$TPL_STATUS, TPL_FAMILY = res$TPL_FAMILY, TPL_ORDER = res$TPL_ORDER, APGIII_NUMBER = res$APGIII_NUMBER, KEW_GENUS_EXT = res$KEW_GENUS_EXT, KEW_FAMILY_EXT = res$KEW_FAMILY_EXT, TPL_URL = res$TPL_URL))
}




