
library(data.table)

options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)

li_fi <- list.files()

##nam <- as.character(args[1])
#y <- as.numeric(args[1])

##nam_fi <- grep(nam,li_fi,value=T)


## li <- list()
## for (i in seq_along(nam_fi)){
##     li[[i]] <- get(nam_fi[i])
## }

for (j in seq_along(args)){
    g <- load(args[j])
    li[[j]] <- get(g)
}

merg <- do.call(li, rbind)

## asdf <- function(x){
##         return(2*x)
##     }
## z = asdf(y)

## strsplit(args[1],"_")[[1]][1]

save(merg,file=paste(strsplit(args[1],"_")[[1]][1],'_combined.RObject',sep=""))


