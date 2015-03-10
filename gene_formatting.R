
dat_tab <- data.table(a=c(seq(10),NA,NA,NA),b=c(NA,NA,NA,seq(10)))
inds <- which(is.na(dat_tab),arr.ind=T)
for (ii in seq(dim(inds)[1])){
    set(dat_tab,i=as.integer(inds[ii,1]),j=as.integer(inds[ii,2]),value=99)
}

dat_tab
set(dat_tab,i=as.integer(inds[,1]),j=as.integer(inds[,2]),value=99)
set(dat_tab,seq(3),1,99)
set(dat_tab,which(is.na(get('b',envir=as.environment(dat_tab)))),2,99)


for(ii in seq(dim(dat_tab)[1])){
    tmp_inds <- which(is.na(dat_tab[ii]))
    if(length(tmp_inds))
    set(dat_tab,i=ii,tmp_inds,99
}
dat_tab


library(lme4)
library(NCBI2R)

lmm.data <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module9/lmm.data.txt",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)


lmm_2 <- lmer(extro ~ open + agree + social + class + (1|school/class), data = lmm.data, REML = TRUE, verbose = FALSE)


#lmm.data$new_col <- apply(cbind(as.character(lmm.data$school),as.character(lmm.data$class)),1,function(x){paste(x,collapse="_")})


lmm_alt <- lmer(extro ~ open + agree + social + class + (1|new_col), data= lmm.data, REML = TRUE, verbose = FALSE)


var(apply(matrix(c(74,68,75,72,79,76,71,77,74,81,75,72,77,73,79),nrow=3,byrow=T),2,mean))*4*3


col_means <- apply(matrix(c(74,68,75,72,79,76,71,77,74,81,75,72,77,73,79),nrow=3,byrow=T),2,mean)



var(as.numeric(t(matrix(c(74,68,75,72,79,76,71,77,74,81,75,72,77,73,79),nrow=3,byrow=T)) - col_means))*14


apply(t(matrix(c(74,68,75,72,79,76,71,77,74,81,75,72,77,73,79),nrow=3,byrow=T)) - col_means,1,var)


library(data.table)

gene <- fread('~/Downloads/knownGene.txt',header=F)
ref = fread('~/Downloads/kgXref.txt',header=F)


setnames(ref,c("kgID","mRNA","spID","spDisplayID","geneSymbol","refseq","protAcc","description","rfamAcc","tRnaName"))
setnames(gene,c("name","chrom","strand","txStart","txEnd","cdsStart","cdsEnd","exonCount","exonStarts","exonEnds","proteinID","alignID"))


setkey(ref,kgID)
setkey(gene,name)


dat <- gene[ref]

dat[geneSymbol=='RETN']

dat[grep('RETN',geneSymbol)]



Complement C2
Monocyte differentiation antigen CD14
Apolipoprotein B
Adipsin
Beta-2-microglobulin
Alpha-1-acid glycoprotein 1
Apolipoprotein A-1
Ceruloplasmin
Hemopexin
Tetranectin
Neural cell adhesion molecule (CD56)
Granulin
Insulin-like growth factor-binding protein 1
Serum paraoxonase/arylesterase 1
Protein Z-dependent protease inhibitor
Serum amyloid A1


new_genes <-c("ADM","NM_002521","COL18A1","CRP","CXCL16","CST3","GDF15","GP5","LPA","MMP9","MPO","REG1A","RETN","uc003oal.2","SELP","ABO","KLKB1")

new_genes <-c("ADM","NPPB","COL18A1","CRP","CXCL16","CST3","GDF15","GP5","LPA","MMP9","MPO","REG1A","RETN","AGER","SELP","ABO","KLKB1")




for (i in seq_along(new_genes)){
        print(dat[grep(new_genes[i],geneSymbol)])
}



dat[grep(new_genes[i],geneSymbol)]


dat[grep(new_genes[i],geneSymbol)]





for (i in seq_along(new_genes)){
        print(dim(dat[new_genes[i]==geneSymbol]))
}

dat[grep(new_genes[14],geneSymbol),get("txStart")]


c(1,4,9,11,13,14,15)



for (i in seq_along(new_genes)){
        print(dim(dat[new_genes[i]==geneSymbol]))
}


for (i in seq_along(new_genes)){
        print(dat[new_genes[i]==geneSymbol,cbind(min(get("txStart")),max(get("txEnd")))])
}

for (i in seq_along(new_genes)){
        print(dat[new_genes[i]==geneSymbol,min(get("txStart"))-max(get("txEnd"))])
}



#### FOR INDEX 14, AGER, (sRAGE) -- don't necessarily have good reason to subset to those rows that have exactly 'chr6' for their chromosome
dat[new_genes[14]==geneSymbol][chrom=='chr6'][,min(get("txStart"))-max(get("txEnd"))]
dat[new_genes[14]==geneSymbol][chrom=='chr6'][,cbind(min(get("txStart")),max(get("txEnd")))]




for (i in seq_along(new_genes)){
        if(i!=14){
            print(dat[new_genes[i]==geneSymbol,cbind(min(get("txStart")),max(get("txEnd")))])
            } else {
            print(dat[new_genes[14]==geneSymbol][chrom=='chr6'][,cbind(min(get("txStart")),max(get("txEnd")))])
            }
}



#dat[grep(new_genes[2],geneSymbol),get("txStart"),get("txEnd")]



for (i in c(1,4,9,11,13,14,15)){
        print(dat[new_genes[i]==geneSymbol])
}


# dat[new_genes[1]==geneSymbol]
# dat[new_genes[4]==geneSymbol]
# dat[new_genes[9]==geneSymbol]
# dat[new_genes[11]==geneSymbol]
# dat[new_genes[13]==geneSymbol]
dat[new_genes[14]==geneSymbol][chrom=='chr6'][,min(get("txStart"))-max(get("txEnd"))]
# dat[new_genes[15]==geneSymbol]



ADM
NM_002521
COL18A1
CRP
CXCL_16
CST3
GDF15
GP5
LPA
MMP9
MPO
REG1A
RETN
uc003oal.2
SELP
ABO
KLKB1
KNG1
KLKB1
















#### DEFINE exonStarts and exonEnds HERE, WHERE YOU'VE ALREADY TAKE MIN AND MAX, RESP. ###

start <- as.numeric(unlist(lapply(strsplit(as.character(dat$exonStarts),","),function(x){min(x)})))
end <- as.numeric(unlist(lapply(strsplit(as.character(dat$exonEnds),","),function(x){max(x)})))

#mat2 <- dat[(chrom=='chr2' & start2>=30000000 & end2 <= 60000000),][,data.frame(exonStarts,exonEnds)]

region_inds2 <- (dat$chrom=='chr2' & start>=30000000 & end <= 60000000)
region_inds19 <- (dat$chrom=='chr19' & start>=30000000 & end <= 60000000)


mat2 <- data.frame(exonStarts=start2[region_inds] ,exonEnds=end2[region_inds] )



#cbind(start2,end2)
# paste(c(start2[region_inds],end2[region_inds]),collapse=",")

# mat19 <- dat[(chrom=='chr19' & exonStarts>=30000000 ),][,data.frame(exonStarts,exonEnds)]
# start19 <- unlist(lapply(strsplit(as.character(mat19$exonStarts),","),function(x){x[1]}))
# end19 <- unlist(lapply(strsplit(as.character(mat19$exonEnds),","),function(x){x[1]}))

cbind(start19,end19)

paste(c(start[region_inds2],end[region_inds2]),collapse=",")
paste(c(start[region_inds19],end[region_inds19]),collapse=",")






