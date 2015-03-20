taxa.table <-
function(x, file = NULL, substitute.sp.white.space = "_"){
    res <- paste(x$TPLFamily, x$YourGenus, gsub(" ", substitute.sp.white.space, x$YourSpecies), sep = "/")
    if(!is.null(file)){
        writeLines(res, file)
    }
    return(res)
}
