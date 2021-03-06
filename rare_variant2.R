

library(data.table)
library(SKAT)

options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)

#y <- as.numeric(args[1])
#y <- as.numeric(args[1])

## asdf <- function(x){
##         return(2*x)
##     }

## z <- asdf(y)
## save(z,file='r_ob.RObject')
## dat_indv$V1

##head(cbind(output_id[match_inds],output[match_inds],dat_indv),100)
##dim(output[match_inds])

## head(cbind(output_id[match_inds],output[match_inds]))
## length(match(dat_indv$V1,output_id))

dat_indv <- fread(paste(args[1],".012.indv",sep=""),header=F)
dat_pos <- fread(paste(args[1],".012.pos",sep=""),header=F)
inova_dat <- fread(paste(args[1],".012",sep=""))


dat_indv <- fread("/InovaCommon/chr19/chr19_part2.012.indv",header=F)
dat_pos <- fread("/InovaCommon/chr19/chr19_part2.012.pos",header=F)
inova_dat <- fread("/InovaCommon/chr19/chr19_part2.012")


dat_indv <- fread("/InovaOutput5/chr21/chr21_part13.012.indv",header=F)
dat_pos <- fread("/InovaOutput5/chr21/chr21_part13.012.pos",header=F)
inova_dat <- fread("/InovaOutput5/chr21/chr21_part13.012")



inova_dat[,V1:=NULL,]
output <- fread("/InovaCommon/data/clinicalPost/inova.ped")

## load gene table, hard coded
load("/InovaCommon/gene_dat_table.RObject")

output_id <- sapply(strsplit(output$V2,"-"),function(x)paste(x[c(2,3,1)],collapse="-"))
match_inds <- match(dat_indv$V1,output_id)

##li_fi <- list.files()

m_inds <- grep("M",dat_indv$V1)
f_inds <- grep("F",dat_indv$V1)
nb_inds <- grep("NB",dat_indv$V1)

#output_newb <- "/InovaOutput5/gwas_results/maternal/newbornList.txt"
#output_mat <- "/InovaOutput5/gwas_results/maternal/motherList.txt"

##inova_dat <- fread(as.character(args[1]))
##inova_dat <- fread('/data3/workarea/dswanson/chr19_10K_mat.012')

##samp_size <- dim(inova_dat)[1]

#y_out <- rnorm(samp_size)
#dat_pos <- fread("/InovaCommon/chr19_end.012.pos")
## is it it right to be setting missions values to 0??


## even though many more columns, MUCH faster setting values by column rather than by row
for(ii in seq(dim(inova_dat)[2])){
    ##print(ii)
        tmp_inds <- which(inova_dat[[ii]]==-1)
            if(length(tmp_inds))
                set(inova_dat,i=tmp_inds,ii,0)
                   }


inova_dat_mat <- as.matrix(inova_dat)

#inova_dat_mat[which(inova_dat_mat==-1)] <- 0
#colSums(inova_dat_mat)

obj_m <- SKAT_Null_Model(output$V6[match_inds][m_inds] ~ 1, out_type="C")
obj_f <- SKAT_Null_Model(output$V6[match_inds][f_inds] ~ 1, out_type="C")
obj_nb <- SKAT_Null_Model(output$V6[match_inds][nb_inds] ~ 1, out_type="C")


start <- as.numeric(unlist(lapply(strsplit(as.character(dat$exonStarts),","),function(x){min(x)})))
end <- as.numeric(unlist(lapply(strsplit(as.character(dat$exonEnds),","),function(x){max(x)})))

#mat2 <- dat[(chrom=='chr2' & start2>=30000000 & end2 <= 60000000),][,data.frame(exonStarts,exonEnds)]

#region_inds2 <- (dat$chrom=='chr2' & start>=30000000 & end <= 60000000)
#region_inds19 <- (dat$chrom=='chr19' & start>=30000000 & end <= 60000000)


## assuming here that only one value in dat_pos$V1, which should always be true if vcf's split up by chromosome
dat_chrom <- dat[dat_pos$V1[1] == dat$chrom]
start_chrom <- start[dat_pos$V1[1] == dat$chrom]
end_chrom <- end[dat_pos$V1[1] == dat$chrom]

#(dat_pos$V2>start_chrom)
#(dat_pos$V2<end_chrom)

annot_dat <- dat_chrom[(!(end_chrom < min(dat_pos$V2)) & !(start_chrom > max(dat_pos$V2)))]

start_chrom_use <- start_chrom[(!(end_chrom < min(dat_pos$V2)) & !(start_chrom > max(dat_pos$V2)))]
end_chrom_use <- end_chrom[(!(end_chrom < min(dat_pos$V2)) & !(start_chrom > max(dat_pos$V2)))]


# probably not necessary:
##part_num <- as.numeric(gsub("part","",strsplit(strsplit(args[1],".")[[1]][1],"_")[[1]][2]))


their_sto_baby <- their_sto_dad <- their_sto_mom <- rep(99,length(start_chrom_use))

## their_sto_dad <- list()
## their_sto_baby <- list()
       
### FINDING INDS IN THIS WAY WILL PICK UP GENE FRAGMENTS AS A RESULTS OF THE CUTTING OF THE VCFS INTO PIECES
for (jj in seq_along(start_chrom_use)){    
    test_inds <- which(dat_pos$V2>start_chrom_use[jj] & dat_pos$V2<end_chrom_use[jj])
    #print(test_inds)
    if(length(test_inds)>10 & length(test_inds)<20000){
        their_sto_mom[jj] <- SKAT(inova_dat_mat[f_inds,test_inds], obj_f)$p.value
        their_sto_dad[jj] <- SKAT(inova_dat_mat[m_inds,test_inds], obj_m)$p.value
        their_sto_baby[jj] <- SKAT(inova_dat_mat[nb_inds,test_inds], obj_nb)$p.value
    }
    print(jj)
}

##cbind(annot_dat,their_sto_mom,their_sto_dad,their_sto_baby)

tmp_obj <- cbind(annot_dat,their_sto_mom,their_sto_dad,their_sto_baby)

save(tmp_obj,file = paste(args[1],".RObject",sep=""))
