


#' ggplot heatmap, ordered according to native 'heatmap dendrogram
#'
#' @param surv 
#' EventTime survival times
#' EventStatus censoring indicator (1 implies event observed, 0 otherwise)
#' nam string literal of dat indicating variable by which KM curves should be stratified
#' dat dataframe with above 3 mentioned covariates
#' @keywords km kaplan-meier survival curve
#' @export
#' @examples
#' g <- ggSurv(EventTime,EventStatus,'plink_rs2716212_G',dat)
#' ggsave(g,file='~/test_gg.jpg',height=6,width=8)

ggSurv <- function(EventTime,EventStatus,nam,dat){

require(ggplot2)
require(plyr)

# surv_ob <- survfit(Surv(EventTime,EventStatus)~ get(nam,dat),data=dat)

#surv_ob <- survfit(Surv(substitute(EventTime),substitute(EventStatus)) ~ get(nam,dat),data=dat)

#x<-substitute(nam)

surv_ob <- survfit(Surv(EventTime,EventStatus) ~ get(nam,dat),data=dat)

nam_str = names(table(get(nam,dat)))

a1 <- length(surv_ob$strata)
a2 <- surv_ob$strata

surv <- surv_ob$surv
time <- surv_ob$time
strat <- rep(seq(a1),times=a2)

dat_init<-data.frame(surv=surv,time=time,strat=strat)

#library(plyr)
dat_new=ddply(dat_init,'strat',summarise,time_new=c(0,rep(time,each=2)),surv_new=c(rep(1,2),head(rep(surv,each=2),-1)))

g<-ggplot(dat_new, aes(time_new, surv_new, group = as.factor(strat), color=as.factor(strat))) + geom_path(alpha = 0.5,size=4)+xlab('time')+ylab('survival')+scale_colour_discrete(guide=guide_legend(title = nam),labels=nam_str)
return(g)
}






#' ggplot heatmap, ordered according to native 'heatmap dendrogram
#'
#' @param bio4 matrix or dataframe whose heatmap you would like to plot
#' @keywords heatmap correlation covariance
#' @export
#' @examples
#' g <- ggheat(bio4)
#' print(g)

ggHeat <- function(bio4){

require(reshape2)
require(ggplot2)
require(scales)

#options(device=NULL)

d_cor<-cor(bio4,use="pairwise.complete.obs")

pdf('bogus.pdf')
d_h<-heatmap(d_cor)#,dendrogram='none'
dev.off()
d_inds<-d_h$rowInd
d_dat<-melt(d_cor[d_inds,d_inds])

d_dat$Var1<-ordered(d_dat$Var1,levels=as.character(unique(d_dat$Var1)))
d_dat$Var2<-ordered(d_dat$Var2,levels=as.character(unique(d_dat$Var2)))

#options(device=quartz)
#pdf('asdf.pdf')
g<-ggplot(d_dat,aes(x=Var1, y=Var2))+geom_tile(aes(fill=value))+theme(axis.text.x = element_text(angle = 60, hjust = 1),axis.title.x=element_blank(),axis.title.y=element_blank())+labs(fill="correlation")+scale_fill_gradientn(colours=c("lightblue","white", "red", "darkred"), values=rescale(c(-0.5, 0,0.5, 1)),guide="colorbar")
g
#dev.off()
}







#' function for exact log-rank hypothesis tests
#'
#' @param mod the summary of a survfit object from the survival package
#' no other params just test
#' @keywords survival log-rank test
#' @export
#' @examples
#' mod <- summary(survfit(Surv(time,ind)~ grp))
#' exact_log_rank(mod)


exact_log_rank <- function(mod){
	require(survival)
	ind1 <- which(as.numeric(mod$strata)==1)

	if(length(ind1)){
		inds <- ind1[1]:length(mod$strata)
		
		dat0 <- data.frame(ev0 = mod$n.event[-inds],ri0 = mod$n.risk[-inds],ti = mod$time[-inds])
		dat1 <- data.frame(ev1 = mod$n.event[inds],ri1 = mod$n.risk[inds],ti = mod$time[inds])
		dat <- merge(dat0,dat1,by='ti',all=T)
		dat[is.na(dat[c('ev0','ev1')])] <- 0
		dat[is.na(dat['ri0']),'ri0'] <- dat[is.na(dat['ri0']),'ri1']
		dat[is.na(dat['ri1']),'ri1'] <- dat[is.na(dat['ri1']),'ri0']
		
		dat['ri1'] <- dat['ri1'] - dat['ev1']
		dat['ri0'] <- dat['ri0'] - dat['ev0']
	
		tes <- 	array(as.numeric(t(dat[c('ev0','ri0','ev1','ri1')])),dim=c(2,2,dim(dat)[1]))
		
	} else { 
	
		dat <- data.frame('ev0' = mod$n.event,'ri0' = (mod$n.risk - mod$n.event),'ev1' = rep(0,length(mod$n.event)), 'ri1' = mod$n.risk)

		tes <- 	array(as.numeric(t(dat[c('ev0','ri0','ev1','ri1')])),dim=c(2,2,dim(dat)[1]))
		print(tes)
	}
	
	return(mantelhaen.test(tes,exact=T))

}




