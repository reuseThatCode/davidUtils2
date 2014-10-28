
library(data.table)
library(lmerTest)
library(bit64)

library(dplyr)
library(plyr)

install.packages('dplyr',depend=T)

dat <- fread('/data3/workarea/dswanson/kucera_tab.csv')
dat_full_pull <- fread('/home/dswanson/product/full_pop.txt')
#dat_small_pull <- fread('/home/dswanson/product/full_pop_small.txt')

mean(dat_small_pull$origId %in% dat$member_ctg)







dat[,member_ctg,med_elig,rx_elig,med_allowed,rx_allowed,aso_ind,][,lapply(.SD,sum],by=member_ctg,]


setkeyv(dat,c('member_ctg','mbr_dob','mbr_gender'))


dat2 <- dat[,member_ctg,med_elig,rx_elig,med_allowed,rx_allowed,aso_ind,]

dat[,setdiff(colnames(dat),"mos"), with=FALSE][


dat2 <- dat[,setdiff(colnames(dat),"mos"), with=FALSE][,lapply(.SD,sum),by=member_ctg]

dat2 <- dat[,setdiff(colnames(dat),"mos"), with=FALSE][,lapply(.SD,sum),by=member_ ctg]
 
                                     
dat[,id:=as.character(member_ctg),]


setkeyv(dat,c('id','mbr_dob','mbr_gender'))
                                     

dat2 <- dat[,setdiff(colnames(dat),c("member_ctg","mos")), with=FALSE][,lapply(.SD,sum),by=key(dat)]


##dat3 <- dat[,setdiff(colnames(dat),c("mos",'mbr_dob','mbr_gender')), with=FALSE][,lapply(.SD,sum),by='member_ctg']

                                     
dat3 <- dat[mos=='201408']
dat4 <- dat3[aso_ind==0]


mean(dat2$med_elig | dat2$rx_elig)

                                     
mean(dat4$med_elig | dat4$rx_elig)

mean(dat4$med_elig & dat4$rx_elig)


                                     
dat$med_allowed



                                     
setkeyv(dat4,'id')
                                     
#setkeyv(dat2,'id')

dat_full_pull[,id:=as.character(origId),]
setkey(dat_full_pull,'id')



dat4[,tot_cost:=med_allowed+rx_allowed,]


dat_full_pull[,in_targ:=1,]

## big_merge <- dat_full_pull[dat2]
## small_merge <- dat2[dat_full_pull]

big_merge <- dat_full_pull[dat4]
small_merge <- dat4[dat_full_pull]

big_merge$in_targ[is.na(big_merge$in_targ)] <- 0

ret <- strsplit(big_merge$mbr_dob,'-')
age_vec <- 2014 - sapply(ret,function(x){as.numeric(x[1])})
big_merge[,age_year:=age_vec,]

ret2 <- strsplit(small_merge$mbr_dob,'-')
age_vec2 <- 2014 - sapply(ret2,function(x){as.numeric(x[1])})
small_merge[,age_year:=age_vec2]



                                     
### maybe will need to use this:

med_vec <- rep(0,dim(big_merge)[1])
med_vec[big_merge$age_year>65] <- 1

gen_vec <- rep(0,dim(big_merge)[1])
gen_vec[big_merge$mbr_gender=='M'] <- 1
                                     
big_merge[,gender_bin:=gen_vec,]
big_merge[,medicare:=med_vec,]

med_vec2 <- rep(0,dim(small_merge)[1])
med_vec2[small_merge$age_year>65] <- 1

gen_vec2 <- rep(0,dim(small_merge)[1])
gen_vec2[small_merge$mbr_gender=='M'] <- 1

small_merge[,gender_bin:=gen_vec2,]
small_merge[,medicare:=med_vec2,]



+ aso_ind
          
mod_bin <- glm(in_targ ~ age_year + mbr_gender  + tot_cost +medicare + medicare*tot_cost,family=binomial,data=big_merge)

(Intercept)       -4.818e+00
age_year           4.763e-02
mbr_genderM        6.332e-02
tot_cost           5.781e-04
medicare          -2.397e-01
tot_cost:medicare  2.776e-03


                                     
                                     
                                     
mod_one <- glm(in_targ ~ age_year + mbr_gender + med_allowed+ med_elig*med_allowed + aso_ind,family=binomial,data=small_merge)


mod_two <- glm(in_targ ~ age_year + mbr_gender + med_allowed+ med_elig*med_allowed + aso_ind,family=binomial,data=small_merge)


library(reshape2)


small_merge[,c('MedicalAllowed_R12M','med_allowed'),with=F][seq(150)] ## in most cases seem to align



## ALL_DOI


small_merge2 <- within(small_merge,list(COST_ALL_DOI <- sum(c(MedicalAllowed_CUSTOM_44a_R12M,MedicalAllowed_CUSTOM_44b_R12M,MedicalAllowed_CUSTOM_27a_R12M,MedicalAllowed_CUSTOM_27b_R12M,MedicalAllowed_CUSTOM_27c_R12M,MedicalAllowed_CUSTOM_31a_R12M,MedicalAllowed_CUSTOM_32a_R12M,MedicalAllowed_CUSTOM_33a_R12M,MedicalAllowed_CUSTOM_34a_R12M,MedicalAllowed_CUSTOM_35a_R12M,MedicalAllowed_CUSTOM_36a_R12M,MedicalAllowed_CUSTOM_37a_R12M,MedicalAllowed_CUSTOM_37b_R12M,MedicalAllowed_CUSTOM_39a_R12M,MedicalAllowed_CUSTOM_40a_R12M,MedicalAllowed_CUSTOM_83a_R12M),na.rm=T),COST_RESP <- sum(c(MedicalAllowed_CUSTOM_44a_R12M,MedicalAllowed_CUSTOM_44b_R12M),na.rm=T),COST_Cardio_and_CVD <- sum(c(MedicalAllowed_CUSTOM_31a_R12M,MedicalAllowed_CUSTOM_32a_R12M,MedicalAllowed_CUSTOM_33a_R12M,MedicalAllowed_CUSTOM_34a_R12M,MedicalAllowed_CUSTOM_35a_R12M,MedicalAllowed_CUSTOM_36a_R12M,MedicalAllowed_CUSTOM_37a_R12M,MedicalAllowed_CUSTOM_37b_R12M,MedicalAllowed_CUSTOM_39a_R12M,MedicalAllowed_CUSTOM_40a_R12M,MedicalAllowed_CUSTOM_83a_R12M),na.rm=T),COST_Diabetes <- sum(c(MedicalAllowed_CUSTOM_27a_R12M,MedicalAllowed_CUSTOM_27b_R12M,MedicalAllowed_CUSTOM_27c_R12M),na.rm=T)))

### Problem:
sum(!is.na(small_merge[,28:66,with=F]))
# > 0


small_merge2$HOIAdmitInd_R12M[is.na(small_merge2$HOIAdmitInd_R12M)] <- 0
small_merge2$HOIERInd_R12M[is.na(small_merge2$HOIERInd_R12M)] <- 0

## Make new PDC covariates
small_merge2$PDC_Diabetes_ind <- rep(0,dim(small_merge2)[1])
small_merge2$PDC_RESP_ind <- rep(0,dim(small_merge2)[1])
small_merge2$PDC_Cardio_and_CVD_ind <- rep(0,dim(small_merge2)[1])
small_merge2$PDC_ALL_DOI_ind <- rep(0,dim(small_merge2)[1])

small_merge2$PDC_Diabetes_ind[which(!is.na(small_merge2$PDC_Diabetes_R12M))] <- 1
small_merge2$PDC_RESP_ind[which(!is.na(small_merge2$PDC_RESP_R12M))] <- 1
small_merge2$PDC_Cardio_and_CVD_ind[which(!is.na(small_merge2$PDC_Cardio_and_CVD_R12M))] <- 1
small_merge2$PDC_ALL_DOI_ind[which(!is.na(small_merge2$PDC_ALL_DOI_R12M))] <- 1








# modelling whether in certain morbidity grps for later use in modelling PDC and costs
mod_bin_diab <- glm(PDC_Diabetes_ind ~ age_year + mbr_gender  + tot_cost  +medicare + medicare*tot_cost,family=binomial,data=small_merge2)
mod_bin_resp <- glm(PDC_RESP_ind ~ age_year + mbr_gender  + tot_cost +medicare + medicare*tot_cost,family=binomial,data=small_merge2)
mod_bin_cvd <- glm(PDC_Cardio_and_CVD_ind ~ age_year + mbr_gender  + tot_cost +medicare + medicare*tot_cost,family=binomial,data=small_merge2)
mod_bin_all <- glm(PDC_ALL_DOI_ind ~ age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost,family=binomial,data=small_merge2)


### SHOULD ALSO MODEL MED AND DRUG ELIG HERE
small_merge2$rx_elig[is.na(small_merge2$rx_elig)] <- 0
small_merge2$med_elig[is.na(small_merge2$med_elig)] <- 0


mod_bin_med_elig <- glm(med_elig  ~ age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost,family=binomial,data=small_merge2)
mod_bin_rx_elig <- glm(rx_elig ~ age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost,family=binomial,data=small_merge2)


COST_Diabetes
COST_Cardio_and_CVD
COST_RESP
COST_ALL_DOI
## WHen you model cost / visit, visit should be in the model since could regress to mean if not included and prob big diffs b/w pop's when not included and normality assumption perhaps not satisfied but this last thing not big issue and other big issues even if satisfied

## mod_cont_diab <- glm(COST_Diabetes ~ age_year + mbr_gender + tot_cost + medicare + medicare*tot_cost, data = small_merge2[PDC_Diabetes_ind==1])
## mod_cont_resp <- glm(COST_RESP ~ age_year + mbr_gender + tot_cost + medicare + medicare*tot_cost, data = small_merge2[PDC_RESP_ind==1])
## mod_cont_cvd <- glm( COST_Cardio_and_CVD ~ age_year + mbr_gender + tot_cost + medicare + medicare*tot_cost, data = small_merge2[PDC_Cardio_and_CVD_ind==1])
## mod_cont_all <- glm( COST_ALL_DOI ~ age_year + mbr_gender + tot_cost + medicare + medicare*tot_cost,data = small_merge2[PDC_ALL_DOI_ind==1])

## modelling inpatient and ER eligibility
mod_bin_hoi_admit <- glm(HOIAdmitInd_R12M ~ age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost,family=binomial,data=small_merge2)
mod_bin_hoi_er <- glm(HOIERInd_R12M ~ age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost,family=binomial,data=small_merge2)


## ## Modelling counts 
## glm(HOIAdmitCount_R12M ~ age_year + mbr_gender + tot_cost +medicare + medicare*tot_cost,family=poisson,data=small_merge2)
## glm( HOIERCount_R12M ~ age_year + mbr_gender + tot_cost +medicare + medicare*tot_cost,family=poisson,data=small_merge2)

## Modelling counts here
admit_count <- glm( HOIAdmitCount_R12M ~ age_year + mbr_gender + tot_cost +medicare + medicare*tot_cost,family=poisson,data=small_merge2[HOIAdmitInd_R12M==1])
er_count <- glm( HOIERCount_R12M ~ age_year + mbr_gender + tot_cost +medicare + medicare*tot_cost,family=poisson,data=small_merge2[HOIERInd_R12M==1])


## special bc additionally have counts as covars
mod_cont_hoiAdmit_allowed<- glm( HOIAdmitAllowed_R12M ~ HOIAdmitCount_R12M + age_year + mbr_gender + tot_cost + medicare + medicare*tot_cost,  data = small_merge2[HOIAdmitInd_R12M==1])
mod_cont_hoiER_allowed <- glm( HOIERAllowed_R12M ~ HOIERCount_R12M + age_year + mbr_gender + tot_cost + medicare + medicare*tot_cost, data = small_merge2[HOIERInd_R12M==1])


## Don't work for now because cost is not being calculated:.  Note the additional covariates here coming from the counts
mod_cost_diab <- glm(COST_Diabetes ~  HOIAdmitCount_R12M +    age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost, data = small_merge2[PDC_Diabetes_ind==1])
mod_cost_resp <- glm(COST_RESP ~ HOIAdmitCount_R12M + age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost, data = small_merge2[PDC_RESP_ind==1])
mod_cost_cvd <- glm(COST_Cardio_and_CVD ~ HOIAdmitCount_R12M + age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost, data = small_merge2[PDC_Cardio_and_CVD_ind==1])
mod_cost_doi <- glm( COST_ALL_DOI ~ HOIAdmitCount_R12M + age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost,data = small_merge2[PDC_ALL_DOI_ind==1])


### Modelling PDC here:

mod_pdc_diab <- glm( PDC_Diabetes_R12M ~ age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost, data = small_merge2[PDC_Diabetes_ind==1])
mod_pdc_resp <- glm( PDC_RESP_R12M ~  age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost, data = small_merge2[PDC_RESP_ind==1])
mod_pdc_cvd <- glm( PDC_Cardio_and_CVD_R12M  ~  age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost, data = small_merge2[PDC_Cardio_and_CVD_ind==1])
mod_pdc_doi <- glm( PDC_ALL_DOI_R12M    ~  age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost,data = small_merge2[PDC_ALL_DOI_ind==1])


 "PDC_Diabetes_R12M"
[19] "PDC_RESP_R12M"                  "PDC_Cardio_and_CVD_R12M"
[21] "PDC_ALL_DOI_R12M"  




mod_med_allowed <- glm(med_allowed ~ age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost,data = small_merge2[med_elig==1])

mod_rx_allowed <- glm(rx_allowed ~ age_year + mbr_gender  + tot_cost + medicare + medicare*tot_cost,data = small_merge2[rx_elig==1])



## LIST of MODELS
coef(mod_bin_diab)
coef(mod_bin_resp)
coef(mod_bin_cvd)
coef(mod_bin_all)
coef(mod_bin_med_elig)
coef(mod_bin_rx_elig)
coef(mod_bin_hoi_admit)
coef(mod_bin_hoi_er)
coef(admit_count)
coef(er_count)

coef(mod_cont_hoiAdmit_allowed)
coef(mod_cont_hoiER_allowed)

coef(mod_med_allowed)
coef(mod_rx_allowed)

aov(mod_med_allowed)
aov(mod_rx_allowed)


(aov(mod_cont_hoiAdmit_allowed))
(aov(mod_cont_hoiER_allowed))

coef(mod_cost_diab)
coef(mod_cost_resp)
coef(mod_cost_cvd)
coef(mod_cost_doi)

coef(mod_pdc_diab)
coef(mod_pdc_resp)
coef(mod_pdc_cvd)
coef(mod_pdc_doi)

aov(mod_pdc_diab)
aov(mod_pdc_resp)
aov(mod_pdc_cvd)
aov(mod_pdc_doi)







### OUTPUT:

> coef(mod_bin)  ## for target population
      (Intercept)          age_year       mbr_genderM          tot_cost
    -4.8181867337      0.0476307970      0.0633239295      0.0005780893
         medicare tot_cost:medicare
    -0.2396812990      0.0027761940
> coef(mod_bin_diab)
      (Intercept)          age_year       mbr_genderM          tot_cost 
    -2.435798e+00      1.199367e-02      1.468929e-01      7.030167e-05 
         medicare tot_cost:medicare 
    -1.530238e-01      5.295045e-05
> coef(mod_bin_resp)
      (Intercept)          age_year       mbr_genderM          tot_cost 
     1.686147e+00     -5.560321e-02     -3.892255e-01      5.727159e-05 
         medicare tot_cost:medicare 
     6.630592e-01      1.406538e-04 
> coef(mod_bin_cvd)
      (Intercept)          age_year       mbr_genderM          tot_cost 
    -2.743292e+00      8.553620e-02      4.684639e-01      4.912053e-05 
         medicare tot_cost:medicare 
    -1.854867e-01     -7.203396e-05 
> coef(mod_bin_all)
      (Intercept)          age_year       mbr_genderM          tot_cost 
     2.656606e+01     -5.656651e-10     -7.095018e-09     -2.094166e-13 
         medicare tot_cost:medicare 
     1.144701e-08      4.068750e-13 
> coef(mod_bin_med_elig)
      (Intercept)          age_year       mbr_genderM          tot_cost 
     2.656606e+01     -5.656651e-10     -7.095018e-09     -2.094166e-13 
         medicare tot_cost:medicare 
     1.144701e-08      4.068750e-13 
> coef(mod_bin_rx_elig)
      (Intercept)          age_year       mbr_genderM          tot_cost 
     2.656606e+01     -5.656651e-10     -7.095018e-09     -2.094166e-13 
         medicare tot_cost:medicare 
     1.144701e-08      4.068750e-13 
> coef(mod_bin_hoi_admit)
      (Intercept)          age_year       mbr_genderM          tot_cost 
    -6.164532e+00      4.237194e-02      1.853048e-01      1.664039e-04 
         medicare tot_cost:medicare 
    -2.552882e-02      3.715256e-05 
> coef(mod_bin_hoi_er)
      (Intercept)          age_year       mbr_genderM          tot_cost 
    -3.402228e+00      1.317376e-02     -1.364858e-01      1.544880e-04 
         medicare tot_cost:medicare 
     1.732363e-01      6.484062e-05 
> coef(admit_count)
      (Intercept)          age_year       mbr_genderM          tot_cost 
     1.503513e-01      1.098938e-03     -2.298072e-02      1.780370e-05 
         medicare tot_cost:medicare 
     3.437930e-02      1.923175e-05 
> coef(er_count)
      (Intercept)          age_year       mbr_genderM          tot_cost 
     2.053270e-01      1.399527e-03     -5.712021e-02      3.403646e-05 
         medicare tot_cost:medicare 
     9.652053e-02     -1.599924e-05
> coef(mod_cont_hoiAdmit_allowed)
       (Intercept) HOIAdmitCount_R12M           age_year        mbr_genderM 
      1.587756e+04       1.692435e+04      -1.221757e+02       4.018496e+03 
          tot_cost           medicare  tot_cost:medicare 
     -5.664562e-02      -1.311798e+04       8.390785e-01 
> coef(mod_cont_hoiER_allowed)
     (Intercept)   HOIERCount_R12M          age_year       mbr_genderM 
     8.902975e+02      7.733673e+02      8.436298e+00      2.408804e+02 
         tot_cost          medicare tot_cost:medicare 
     9.773301e-02     -1.571199e+03     -6.495687e-02 

> coef(mod_pdc_diab)
      (Intercept)          age_year       mbr_genderM          tot_cost 
     6.345654e-01      2.437165e-03      1.594373e-02      7.075073e-06 
         medicare tot_cost:medicare 
     2.857319e-02     -7.239585e-06 
> coef(mod_pdc_resp)
      (Intercept)          age_year       mbr_genderM          tot_cost 
     3.897885e-01      1.936526e-03      1.612315e-02      1.064087e-05 
         medicare tot_cost:medicare 
     4.112006e-02      1.629088e-06 
> coef(mod_pdc_cvd)
      (Intercept)          age_year       mbr_genderM          tot_cost 
     7.801911e-01      9.363285e-04     -1.325000e-02      1.770782e-06 
         medicare tot_cost:medicare 
     2.670236e-02     -2.508877e-06 
> coef(mod_pdc_doi)
      (Intercept)          age_year       mbr_genderM          tot_cost 
     4.945957e-01      5.010174e-03      6.242036e-03      2.971402e-06 
         medicare tot_cost:medicare 
    -2.043499e-02     -5.839029e-06 

> coef(mod_med_allowed)
      (Intercept)          age_year       mbr_genderM          tot_cost 
      21.27863124        0.01280465      -13.48304543        0.43305497 
         medicare tot_cost:medicare 
       8.53496003        0.02687051 
> coef(mod_rx_allowed)
      (Intercept)          age_year       mbr_genderM          tot_cost 
     -21.27863124       -0.01280465       13.48304543        0.56694503 
         medicare tot_cost:medicare 
      -8.53496003       -0.02687051 


  
> 
> aov(mod_pdc_diab)
Call:
   aov(formula = mod_pdc_diab)

Terms:
                 age_year mbr_gender  tot_cost  medicare tot_cost:medicare
Sum of Squares    51.6069     1.4321    0.6038    1.5159            0.3915
Deg. of Freedom         1          1         1         1                 1
                Residuals
Sum of Squares  1068.4579
Deg. of Freedom     25748

Residual standard error: 0.2037075
Estimated effects may be unbalanced
8994 observations deleted due to missingness
> aov(mod_pdc_resp)
Call:
   aov(formula = mod_pdc_resp)

Terms:
                 age_year mbr_gender  tot_cost  medicare tot_cost:medicare
Sum of Squares   108.6525     2.2535    4.9876    5.7882            0.0203
Deg. of Freedom         1          1         1         1                 1
                Residuals
Sum of Squares  3014.5200
Deg. of Freedom     31472

Residual standard error: 0.3094902
Estimated effects may be unbalanced
14847 observations deleted due to missingness


> aov(mod_pdc_cvd)
Call:
   aov(formula = mod_pdc_cvd)

Terms:
                age_year mbr_gender tot_cost medicare tot_cost:medicare
Sum of Squares    81.911      6.528    0.094    9.045             0.243
Deg. of Freedom        1          1        1        1                 1
                Residuals
Sum of Squares   4800.808
Deg. of Freedom    140612

Residual standard error: 0.1847762
Estimated effects may be unbalanced
46680 observations deleted due to missingness
> aov(mod_pdc_doi)
Call:
   aov(formula = mod_pdc_doi)

Terms:
                age_year mbr_gender tot_cost medicare tot_cost:medicare
Sum of Squares   995.432      1.683    0.283    9.155             1.424
Deg. of Freedom        1          1        1        1                 1
                Residuals
Sum of Squares   7731.238
Deg. of Freedom    160170

Residual standard error: 0.219702
Estimated effects may be unbalanced
57970 observations deleted due to missingness







targ_params = np.array([-4.818, 0.04763, 0.06332, 0.0005781, -0.2397, 0.002776])

bin_diab_pars = np.array([-2.435798e+00,1.199367e-02,1.468929e-01,7.030167e-05,-1.530238e-01,5.295045e-05])
bin_resp_pars = np.array([1.686147e+00,-5.560321e-02,-3.892255e-01,5.727159e-05,6.630592e-01,1.406538e-04])
bin_cvd_pars = np.array([-2.743292e+00,8.553620e-02,4.684639e-01,4.912053e-05,-1.854867e-01,-7.203396e-05])
bin_all_pars = np.array([2.656606e+01,-5.656651e-10,-7.095018e-09,-2.094166e-13,1.144701e-08,4.068750e-13]) 
bin_med_elig_pars = np.array([2.656606e+01,-5.656651e-10,-7.095018e-09,-2.094166e-13,1.144701e-08,4.068750e-13 ])
bin_rx_elig_pars = np.array([2.656606e+01,-5.656651e-10,-7.095018e-09,-2.094166e-13,1.144701e-08,4.068750e-13])
bin_hoi_admit_pars = np.array([-6.164532e+00,4.237194e-02,1.853048e-01,1.664039e-04,-2.552882e-02,3.715256e-05 ])
bin_hoi_er_pars = np.array([-3.402228e+00,1.317376e-02,-1.364858e-01,1.544880e-04,1.732363e-01,6.484062e-05])
admit_count_pars = np.array([1.503513e-01,1.098938e-03,-2.298072e-02,1.780370e-05,3.437930e-02,1.923175e-05])
er_count_pars = np.array([2.053270e-01,1.399527e-03,-5.712021e-02,3.403646e-05,9.652053e-02,-1.599924e-05])
hoi_admit_allowed_pars = np.array([1.587756e+04,1.692435e+04,-1.221757e+02,4.018496e+03,-5.664562e-02,-1.311798e+04,8.390785e-01])
hoi_er_allowed_pars = np.array([8.902975e+02,7.733673e+02,8.436298e+00,2.408804e+02,9.773301e-02,-1.571199e+03,-6.495687e-02])



med_allowed_pars = np.array([21.27863124,0.01280465,-13.48304543,0.43305497,8.53496003,0.02687051])
rx_allowed_pars = np.array([ -21.27863124,-0.01280465,13.48304543,0.56694503,-8.53496003,-0.02687051])

med_allowed_sd = 508
rx_allowed_sd = 508




pdc_diab_pars = np.array([ 6.345654e-01,2.437165e-03,1.594373e-02,7.075073e-06,2.857319e-02,-7.239585e-06 ])
pdc_resp_pars = np.array([ 3.897885e-01,1.936526e-03,1.612315e-02,1.064087e-05,4.112006e-02,1.629088e-06 ])
pdc_cvd_pars = np.array([ 7.801911e-01,9.363285e-04,-1.325000e-02,1.770782e-06,2.670236e-02,-2.508877e-06 ])
pdc_doi_pars = np.array([ 4.945957e-01,5.010174e-03,6.242036e-03,2.971402e-06,-2.043499e-02,-5.839029e-06 ])

hoi_admit_sd = 34182.9
hoi_er_sd = 2545.923

pdc_diab_sd = 0.204
pdc_resp_sd = 0.3095
pdc_cvd_sd = 0.1848
pdc_doi_sd = 0.220
  














## small_merge2 <- transform(small_merge,COST_ALL_DOI <- sum(c(MedicalAllowed_CUSTOM_44a_R12M,MedicalAllowed_CUSTOM_44b_R12M,MedicalAllowed_CUSTOM_27a_R12M,MedicalAllowed_CUSTOM_27b_R12M,MedicalAllowed_CUSTOM_27c_R12M,MedicalAllowed_CUSTOM_31a_R12M,MedicalAllowed_CUSTOM_32a_R12M,MedicalAllowed_CUSTOM_33a_R12M,MedicalAllowed_CUSTOM_34a_R12M,MedicalAllowed_CUSTOM_35a_R12M,MedicalAllowed_CUSTOM_36a_R12M,MedicalAllowed_CUSTOM_37a_R12M,MedicalAllowed_CUSTOM_37b_R12M,MedicalAllowed_CUSTOM_39a_R12M,MedicalAllowed_CUSTOM_40a_R12M,MedicalAllowed_CUSTOM_83a_R12M),na.rm=T),COST_RESP <- sum(c(MedicalAllowed_CUSTOM_44a_R12M,MedicalAllowed_CUSTOM_44b_R12M),na.rm=T),COST_Cardio_and_CVD <- sum(c(MedicalAllowed_CUSTOM_31a_R12M,MedicalAllowed_CUSTOM_32a_R12M,MedicalAllowed_CUSTOM_33a_R12M,MedicalAllowed_CUSTOM_34a_R12M,MedicalAllowed_CUSTOM_35a_R12M,MedicalAllowed_CUSTOM_36a_R12M,MedicalAllowed_CUSTOM_37a_R12M,MedicalAllowed_CUSTOM_37b_R12M,MedicalAllowed_CUSTOM_39a_R12M,MedicalAllowed_CUSTOM_40a_R12M,MedicalAllowed_CUSTOM_83a_R12M),na.rm=T),COST_Diabetes <- sum(c(MedicalAllowed_CUSTOM_27a_R12M,MedicalAllowed_CUSTOM_27b_R12M,MedicalAllowed_CUSTOM_27c_R12M),na.rm=T))

## NEED to INCLUDE PDC STUFF FROM BRUCE's CODE


group("PDC_ALL"=(PDCNum::12a+PDCNum::27a+PDCNum::27b+PDCNum::27c+PDCNum::28a+PDCNum::30a+PDCNum::31a+PDCNum::32a+PDCNum::33a+PDCNum::34a+PDCNum::35a+PDCNum::36a+PDCNum::37a+PDCNum::37b+PDCNum::39a+PDCNum::40a+PDCNum::44a+PDCNum::44b+PDCNum::49a+PDCNum::51a+PDCNum::52a+PDCNum::56a+PDCNum::57a+PDCNum::58a+PDCNum::59a+PDCNum::60a+PDCNum::61a+PDCNum::62a+PDCNum::62b+PDCNum::66a+PDCNum::66b+PDCNum::68a+PDCNum::72a+PDCNum::73a+PDCNum::75a+PDCNum::82a+PDCNum::83a+PDCNum::85a+PDCNum::86a)/(PDCDen::12a+PDCDen::27a+PDCDen::27b+PDCDen::27c+PDCDen::28a+PDCDen::30a+PDCDen::31a+PDCDen::32a+PDCDen::33a+PDCDen::34a+PDCDen::35a+PDCDen::36a+PDCDen::37a+PDCNum::37b+PDCDen::39a+PDCDen::40a+PDCDen::44a+PDCDen::44b+PDCDen::49a+PDCDen::51a+PDCDen::52a+PDCDen::56a+PDCDen::57a+PDCDen::58a+PDCDen::59a+PDCDen::60a+PDCDen::61a+PDCDen::62a+PDCDen::62b+PDCDen::66a+PDCDen::66b+PDCDen::68a+PDCDen::72a+PDCDen::73a+PDCDen::75a+PDCDen::82a+PDCDen::83a+PDCDen::85a+PDCDen::86a), "", aggForward(12 month,["Annual"]))

group("PDC_ALL_DOI"=(PDCNum::44a+PDCNum::44b+PDCNum::27a+PDCNum::27b+PDCNum::27c+PDCNum::31a+PDCNum::32a+PDCNum::33a+PDCNum::34a+PDCNum::35a+PDCNum::36a+PDCNum::37a+PDCNum::37b+PDCNum::39a+PDCNum::40a+PDCNum::83a)/(PDCDen::44a+PDCDen::44b+PDCDen::27a+PDCDen::27b+PDCDen::27c+PDCDen::31a+PDCDen::32a+PDCDen::33a+PDCDen::34a+PDCDen::35a+PDCDen::36a+PDCDen::37a+PDCNum::37b+PDCDen::39a+PDCDen::40a+PDCDen::83a), "",  aggForward(12 month,["Annual"]))
                                     
group("PDC_Cardio_and_CVD"=(PDCNum::31a+PDCNum::32a+PDCNum::33a+PDCNum::34a+PDCNum::35a+PDCNum::36a+PDCNum::37a+PDCNum::37b+PDCNum::39a+PDCNum::40a+PDCNum::83a)/(PDCDen::31a+PDCDen::32a+PDCDen::33a+PDCDen::34a+PDCDen::35a+PDCDen::36a+PDCDen::37a+PDCNum::37b+PDCDen::39a+PDCDen::40a+PDCDen::83a), "",  aggForward(12 month,["Annual"]))
                                     
group("PDC_RESP"=(PDCNum::44a+PDCNum::44b)/(PDCDen::44a+PDCDen::44b), "", aggForward(12 month,["Annual"]))

group("PDC_Diabetes"=(PDCNum::27a+PDCNum::27b+PDCNum::27c)/(PDCDen::27a+PDCDen::27b+PDCDen::27c), "",aggForward(12 month,["Annual"]))

                                     











                                     



                                     
##small_merge[,c('MedicalAllowed_R12M','med_allowed'),with=F][seq(150)]


predicted_er
predicted_admit
DOICost
CardioCost
DiabetesCost
RespiratoryCost
PDC_All_DOI
PDC_Cardio
PDC_Resp
PDC_Diabetes




                                     

"MedicalAllowed_CUSTOM_12a_R12M" "MedicalAllowed_CUSTOM_27a_R12M"
[25] "MedicalAllowed_CUSTOM_27b_R12M" "MedicalAllowed_CUSTOM_27c_R12M"
[27] "MedicalAllowed_CUSTOM_28a_R12M" "MedicalAllowed_CUSTOM_30a_R12M"
[29] "MedicalAllowed_CUSTOM_31a_R12M" "MedicalAllowed_CUSTOM_32a_R12M"
[31] "MedicalAllowed_CUSTOM_33a_R12M" "MedicalAllowed_CUSTOM_34a_R12M"
[33] "MedicalAllowed_CUSTOM_35a_R12M" "MedicalAllowed_CUSTOM_36a_R12M"
[35] "MedicalAllowed_CUSTOM_37a_R12M" "MedicalAllowed_CUSTOM_37b_R12M"
[37] "MedicalAllowed_CUSTOM_39a_R12M" "MedicalAllowed_CUSTOM_40a_R12M"
[39] "MedicalAllowed_CUSTOM_44a_R12M" "MedicalAllowed_CUSTOM_44b_R12M"
[41] "MedicalAllowed_CUSTOM_49a_R12M" "MedicalAllowed_CUSTOM_51a_R12M"
[43] "MedicalAllowed_CUSTOM_52a_R12M" "MedicalAllowed_CUSTOM_56a_R12M"
[45] "MedicalAllowed_CUSTOM_57a_R12M" "MedicalAllowed_CUSTOM_58a_R12M"
[47] "MedicalAllowed_CUSTOM_59a_R12M" "MedicalAllowed_CUSTOM_60a_R12M"
[49] "MedicalAllowed_CUSTOM_61a_R12M" "MedicalAllowed_CUSTOM_62a_R12M"
[51] "MedicalAllowed_CUSTOM_62b_R12M" "MedicalAllowed_CUSTOM_66a_R12M"
[53] "MedicalAllowed_CUSTOM_66b_R12M" "MedicalAllowed_CUSTOM_68a_R12M"
[55] "MedicalAllowed_CUSTOM_72a_R12M" "MedicalAllowed_CUSTOM_73a_R12M"
[57] "MedicalAllowed_CUSTOM_75a_R12M" "MedicalAllowed_CUSTOM_82a_R12M"
[59] "MedicalAllowed_CUSTOM_83a_R12M" "MedicalAllowed_CUSTOM_85a_R12M"
[61] "MedicalAllowed_CUSTOM_86a_R12M" "MedicalAllowed_R12M"         




            

MedicalAllowed_R12M  ## has this been aggregated properly? should medical_allowed be used instead?

rx_allowed ## same question here.  Not an aggregation, I think....nope, actually aggregated above in dat2



admitsPerPerson = HOIAdmitCount/HOIAdmitInd
dollarsPerAdmit = HOIAdmitAllowed/HOIAdmitCount
erVisitsPerPerson = HOIERCount/HOIERInd
dollarsPerERVist = HOIERAllowed/HOIERCount
avgDrugCost = RxAllowed
avgMedicalCost = MedicalAllowed

            
summary(lm(cbind(PDC_Diabetes_R12M,PDC_Cardio_and_CVD_R12M) ~ Age ,data=small_merge))
summary(lm(PDC_Diabetes_R12M ~ Age ,data=small_merge))

adm_pp <- HOIAdmitCount_R12M/HOIAdmitInd_R12M
doll_pa <- HOIAdmitAllowed_R12M/HOIAdmitCount_R12M
ervis_pp <- HOIERCount_R12M/HOIERInd_R12M
doll_pa <- HOIERAllowed_R12M/HOIERCount_R12M
avg_rx_cos <- rx_allowed
avg_med_cos <- med_allowed


sum(small_merge$HOIAdmitAllowed_R12M,na.rm=T)/sum(small_merge$HOIAdmitCount_R12M,na.rm= T)



melt(,id.var='id',measure.vars=c())


mbr_gender med_elig rx_elig med_allowed rx_allowed aso_ind   PDC_Diabetes_R12M PDC_RESP_R12M PDC_Cardio_and_CVD_R12M PDC_ALL_DOI_R12M   HOIAdmitCount_R12M HOIAdmitAllowed_R12M HOIERCount_R12M HOIERAllowed_R12M MedicalAllowed_R12M age_year
                                     

med_vec <- rep(0,dim(big_merge)[1])
med_vec[big_merge$age_year>65] <- 1

gen_vec <- rep(0,dim(big_merge)[1])
gen_vec[big_merge$mbr_gender=='M'] <- 1

big_merge[,gender_bin:=gen_vec,]
big_merge[,medicare:=med_vec,]


big_to_write <- big_merge[,c("gender_bin","med_elig","rx_elig","med_allowed","rx_allowed","aso_ind","age_year","medicare"),with=F]
write.csv(big_to_write,file="/home/dswanson/product/cam_samp.csv",row.names=F)



which(!is.na(big_merge$Age))
samp <- sample(which(!is.na(big_merge$Age)),10000,replace=F)
big_to_write2 <- big_merge[samp,c(  "age_year", "gender_bin","aso_ind","tot_cost","medicare"),with=F]
write.csv(big_to_write2,file="/home/dswanson/product/cam_samp2.csv",row.names=F)





### should per member per month costs be both med_allowed and rx_allowed?  Currently just med_allowed.


lmerTest(,)

admitsPerPerson dollarsPerAdmit erVisitsPerPerson dollarsPerERVist avgDrugCost avgMedicalCost
                                     
## PLAN -- Oct 26 2014,
## 1. model being in target population with aso.  When calculating the total target pop number, condition on aso being zero, then sum the p-hats of those
## 2. Models fit without aso.  should be modelling med_elig and rx_elig as well as other things already intended
## 3. when feed shifted data in to prediction models, should not included aso





