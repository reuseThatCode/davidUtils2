
library(data.table)

dat <- fread("/InovaOutput5/chr19_sub_singleton.singletons")
vcf_sub <- fread("/InovaCommon/chr19_end.012")


vcf_sub <- fread("/InovaCommon/chr2_2ndpart.012")



lis <- list()

for (i in seq(dim(dat)[1])){
    
    ind <- which(dat$INDV[i]==vcf$asdf_indv)

    # mean of everyone not considering the singleton person
    col_of_interest <- vcf[some_function(dat$pos[i])]
    base <- mean(col_of_interest[-ind])

    alt <- col_of_interest[ind]
    delta <- (alt - base)
    lis[[i]] <- delta

}

# what would the distribution of deltas look like under the null of no singleton variant effect?
# is delta 1 to 1 and/or synony with Z stat from the regression that uses the singleton? any actual advantage to taking a closer look? 
# yes, just linear transformation so no advantage above z-stat or beta from regression



