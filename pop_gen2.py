
## how to run from command line: python pop_gen2.py 500000 100000 400 300 45 75 20 10 20 25 "cambia_test_full"

import numpy as np
import pandas as pd
#import statsmodels as sm
import sys

# samp_size_out = 10000
tot_mem = float(sys.argv[1])
aso_mem = float(sys.argv[2])
medicare_cos = float(sys.argv[3])
commerc_cos = float(sys.argv[4])

pct_fem = float(sys.argv[5])

pct_commerc = float(sys.argv[6])
mem_65 = float(sys.argv[7])
mem_50 = float(sys.argv[8])
mem_35 = float(sys.argv[9])
mem_18 = float(sys.argv[10])
mem_0 = 100-mem_65-mem_50-mem_35-mem_18
out_path = str(sys.argv[11])

# tot_mem = 500000
# aso_mem = 100000
# medicare_cos = 400
# commerc_cos = 300

# pct_fem = 45

# pct_commerc = 75
# mem_65 = 20
# mem_50 = 10
# mem_35 = 20
# mem_18 = 25
# mem_0 = 100-mem_65-mem_50-mem_35-mem_18
# out_path = 'cambia_test_full'


# (Intercept)       -4.818e+00
# age_year           4.763e-02
# mbr_genderM        6.332e-02
# tot_cost           5.781e-04
# medicare          -2.397e-01
# tot_cost:medicare  2.776e-03



def calc_exp_age(mem_65,mem_50,mem_35,mem_18):
	me = (float(mem_65)/100*75+float(mem_50)/100*57+
            float(mem_35)/100*42+float(mem_18)/100*26+
            (100-(float(mem_65)+float(mem_50)+
                  float(mem_35)+float(mem_18)))/100*10)
	me2 = (float(mem_65)/100*75**2+float(mem_50)/100*57**2+
            float(mem_35)/100*42**2+float(mem_18)/100*26**2+
            (100-(float(mem_65)+float(mem_50)+
                  float(mem_35)+float(mem_18)))/100*10**2)
	v = me2 - me**2
	return [me,v]


# def shift_dat(dat,mean1, mean2, mean3, mean4, mean5):
#     """ takes Cambia data as an argument and shifts it, returning 
#     what will be used as input data in the fitted model in order to 
#     generate new samples"""
#     #means = colmeans(dat)
#     new0 = change_cont_mean(dat[:,0],mean1)
#     new1 = change_cont_mean(dat[:,1],mean2)
#     new2 = change_bin_mean(dat[:,2],mean3)
#     new3 = change_bin_mean(dat[:,3],mean4)
#     new4 = change_bin_mean(dat[:,4],mean5)
#     #new5 = change_cont_mean(dat[:,5],mean6)
#     #new6 = change_bin_mean(dat[:,6],mean7)
#     dat_shifted = np.vstack((new0,new1,new2,new3,new4)).T
#     return dat_shifted


#(age,gender,aso, med_cost,medicare_ind)

def shift_dat(dat,age_mean, gen_mean,aso_mean,med_cos_mean,com_cos_mean,med_ind_mean):
    """ takes Cambia data as an argument and shifts it, returning 
    what will be used as input data in the fitted model in order to 
    generate new samples"""
    #means = colmeans(dat)
    new1 = change_cont_mean(dat[:,0],age_mean)
    #new1 = change_cont_mean(dat[:,1],mean2)
    not_inds = [not bool(x) for x in dat_john[:,4]]
    new4 = dat[:,3]    
    new4[np.where(dat[:,4])] *=  med_cos_mean/np.mean(new4[np.where(dat[:,4])])
    new4[np.where(not_inds)] *=  com_cos_mean/np.mean(new4[np.where(not_inds)])
    #new1_commer = change_cont_mean(dat[:,1],mean2)
    
    new2 = change_bin_mean(dat[:,1],gen_mean)
    new3 = change_bin_mean(dat[:,2],aso_mean)
    new5 = change_bin_mean(dat[:,4],med_ind_mean)
    #new5 = change_cont_mean(dat[:,5],mean6)
    #new6 = change_bin_mean(dat[:,6],mean7)

    dat_shifted = np.vstack((new1,new2,new3,new4,new5)).T
    return dat_shifted


def pred_output_params(dat, beta_block,covari):
    """with parameters and MVN error distribution coming from the 
    fitted model along with shifted distribution, calculated predicted 
    parameters corresponding to the 'output' part of the population 
    shaper page"""
    si_beta = beta_block.shape[1] # where number columns is num outcomes
    li = dat.shape[0]
    dat_tmp = np.hstack((np.ones(li).reshape(li,1),dat))
    #np.diag(np.ones(si_beta))
    ran = np.random.multivariate_normal(mean = np.zeros(si_beta),cov=covari,size = li)   
    dat_ret = np.dot(dat_tmp,beta_block) + ran
    return np.apply_along_axis(np.mean,0,dat_ret)


## Right now, output population will be the same size as what's used as input, in this case John's data
def pred_output_pop(dat,beta_block,covari):
    """with parameters and MVN error distribution coming from the 
    second of the two fitted models and shifted data, we generate 
    the new population, which will ultimately be written out to csv"""
    si_beta = beta_block.shape[1] # where number columns is num outcomes
    li = dat.shape[0]
    dat_tmp = np.hstack((np.ones(li).reshape(li,1),dat))
    #np.diag(np.ones(si_beta))
    ran = np.random.multivariate_normal(mean = np.zeros(si_beta),cov=covari,size = li)   
    dat_ret = np.dot(dat_tmp,beta_block) + ran
    return dat_ret



# def pred_targ(dat,params):
#     """for shifted input data and already-fitted model, gives a p-hat for each 
#     individual and associated size of target population"""
#     si = dat.shape[0]
#     #print si

#     dat_int = np.hstack((np.ones(si).reshape((si,1)),dat))
#     #np.array([param1,param2,param3,param4,param5,param6])
    
#     inds_to_use = [not bool(x) for x in dat$aso]

#     dat_int2 = dat[inds_to_use,:]

#     lin_pred = np.dot(dat_int2,params)
#     #print lin_pred.shape
#     p_hat = np.exp(lin_pred)/(1+np.exp(lin_pred))
#     #print p_hat.shape
#     #print p_hat[:50]
#     return np.sum(p_hat)


# data plus seven parameters, 6 for covariates and one intercept
# params is a length 7 np array of betas with the intercept first
# like: inter,param1,param2,param3,param4,param5,param6
# should be in order: age_year + mbr_gender + aso_ind + tot_cost + +medicare + medicare*tot_cost
def pred_targ(dat,params):
    """for shifted input data and already-fitted model, gives a p-hat for each 
    individual and associated size of target population"""
    si = dat.shape[0]
    #print si
    dat_int = np.hstack((np.ones(si).reshape((si,1)),dat))
    #np.array([param1,param2,param3,param4,param5,param6])
    #inds_to_use = [not bool(x) for x in dat$aso]
    #dat_int2 = dat[inds_to_use,:]
    lin_pred = np.dot(dat_int,params)
    #print lin_pred.shape
    p_hat = np.exp(lin_pred)/(1+np.exp(lin_pred))
    #print p_hat.shape
    #print p_hat[:50]
    return p_hat



def change_cont_mean(vec,new_me):
    sto_me = np.mean(vec)
    new_vec = vec
    new_vec *= new_me/sto_me
    return new_vec

def change_bin_mean(vec, new_me):
    sto_me = np.mean(vec)
    me_diff = new_me - sto_me
    len_vec = len(vec)
    
    if me_diff>0:
        change_inds = np.random.choice(np.where([not bool(x) for x 
                                                 in vec])[0],
                                       size = np.round(me_diff*len_vec)
                                       ,replace=False)
        new_vec = vec
        new_vec[change_inds] = 1
    else:
        #print -me_diff*len_vec
        #print me_diff
        #print len_vec
        #print len(np.where([bool(x) for x in vec])[0])
        #print len(np.where(vec)[0])
        #print vec[0:30]
        change_inds = np.random.choice(np.where([bool(x) for x 
                                                 in vec])[0],
                                       size = np.round(-me_diff*len_vec)
                                       ,replace=False)
        new_vec = vec
        new_vec[change_inds] = 0
    
    return new_vec


def samp_pois(dat,params):
    """dat is a dataframe numpy array and params is a 1 dimensional vector of parameters
    including the intercept"""
    #si_beta = beta_block.shape[1] # where number columns is num outcomes
    si_beta = 1
    li = dat.shape[0]
    #params.reshape((len_param,1))
    dat_tmp = np.hstack((np.ones(li).reshape(li,1),dat))
    #np.diag(np.ones(si_beta))
    lam_num = np.exp(np.dot(dat_tmp,params))
    dat_ret = np.random.poisson(lam = lam_num, size = li)
    # ran = np.random.multivariate_normal(mean = np.zeros(si_beta),cov=covari,size = li)
    # dat_ret = np.dot(dat_tmp,beta_block) + ran
    return dat_ret


def samp_bin(dat,params):
    """dat is a dataframe numpy array and params is a 1 dimensional vector of parameters
    including the intercept"""
    li = dat.shape[0]
    dat_tmp = np.hstack((np.ones(li).reshape(li,1),dat))

    exp_num = np.exp(np.dot(dat_tmp,params))
    pi_num = exp_num/(1+exp_num)
    dat_ret = np.random.binomial(n=1, p=pi_num,size = li)
    return dat_ret


def samp_normal(dat,params,err):
    """dat is a dataframe numpy array and params is a 1 dimensional vector of parameters
    including the intercept"""
    li = dat.shape[0]
    dat_tmp = np.hstack((np.ones(li).reshape(li,1),dat))

    lin_pred = np.dot(dat_tmp,params)
    pi_num = lin_pred
    dat_ret = np.random.normal(lin_pred,err,size = li)
    return dat_ret




bin_diab_pars = np.array([-2.435798e+00,1.199367e-02,1.468929e-01,7.030167e-05,-1.530238e-01,5.295045e-05])
bin_resp_pars = np.array([ 1.686147e+00,-5.560321e-02,-3.892255e-01,5.727159e-05,6.630592e-01,1.406538e-04])
bin_cvd_pars = np.array([ -2.743292e+00,8.553620e-02,4.684639e-01,4.912053e-05,-1.854867e-01,-7.203396e-05])
bin_all_pars = np.array([ 2.656606e+01,-5.656651e-10,-7.095018e-09,-2.094166e-13,1.144701e-08,4.068750e-13])
bin_med_elig_pars = np.array([ 2.656606e+01,-5.656651e-10,-7.095018e-09,-2.094166e-13,1.144701e-08,4.068750e-13 ])
bin_rx_elig_pars = np.array([ 2.656606e+01,-5.656651e-10,-7.095018e-09,-2.094166e-13,1.144701e-08,4.068750e-13])
bin_hoi_admit_pars = np.array([ -6.164532e+00,4.237194e-02,1.853048e-01,1.664039e-04,-2.552882e-02,3.715256e-05 ])
bin_hoi_er_pars = np.array([-3.402228e+00,1.317376e-02,-1.364858e-01,1.544880e-04,1.732363e-01,6.484062e-05])
admit_count_pars = np.array([1.503513e-01,1.098938e-03,-2.298072e-02,1.780370e-05,3.437930e-02,1.923175e-05])
er_count_pars = np.array([2.053270e-01,1.399527e-03,-5.712021e-02,3.403646e-05,9.652053e-02,-1.599924e-05])
hoi_admit_allowed_pars = np.array([1.587756e+04,1.692435e+04,-1.221757e+02,4.018496e+03,-5.664562e-02,-1.311798e+04,8.390785e-01])
hoi_er_allowed_pars = np.array([8.902975e+02,7.733673e+02,8.436298e+00,2.408804e+02,9.773301e-02,-1.571199e+03,-6.495687e-02])

pdc_diab_pars = np.array([ 6.345654e-01,2.437165e-03,1.594373e-02,7.075073e-06,2.857319e-02,-7.239585e-06 ])
pdc_resp_pars = np.array([ 3.897885e-01,1.936526e-03,1.612315e-02,1.064087e-05,4.112006e-02,1.629088e-06 ])
pdc_cvd_pars = np.array([ 7.801911e-01,9.363285e-04,-1.325000e-02,1.770782e-06,2.670236e-02,-2.508877e-06 ])
pdc_doi_pars = np.array([ 4.945957e-01,5.010174e-03,6.242036e-03,2.971402e-06,-2.043499e-02,-5.839029e-06 ])

med_allowed_pars = np.array([21.27863124,0.01280465,-13.48304543,0.43305497,8.53496003,0.02687051])
rx_allowed_pars = np.array([ -21.27863124,-0.01280465,13.48304543,0.56694503,-8.53496003,-0.02687051])

hoi_admit_sd = 34182.9
hoi_er_sd = 2545.923

pdc_diab_sd = 0.204
pdc_resp_sd = 0.3095
pdc_cvd_sd = 0.1848
pdc_doi_sd = 0.220

med_allowed_sd = 508
rx_allowed_sd = 508

## stand-ins for now until models fit
cost_diab_pars = np.random.normal(0,0.3,6)
cost_resp_pars = np.random.normal(0,0.3,6)
cost_cvd_pars = np.random.normal(0,0.3,6)
cost_doi_pars = np.random.normal(0,0.3,6)

## made up numbers here
cost_diab_sd = 5
cost_resp_sd = 5
cost_cvd_sd = 5
cost_doi_sd = 5













##### PIPE #####

## parameters between 0 and 100, sum not greater than 100
age_mean, age_var  = calc_exp_age(mem_65,mem_50,mem_35,mem_18)

#dat_cambia = pd.read_csv('/Volumes/dswanson/product/full_pop.txt')
#kucera_tab.csv

### READ IN DATA WHICH CONTAINS NECESSARY JOINT DISTRIBUTIONS ### 
dat_john = np.array(pd.read_csv('/Volumes/dswanson/product/cam_samp2.csv'))

age = np.random.normal(30,4,1000)
gender  = np.random.choice([0,1],1000,replace=True)
aso  = np.random.choice([0,0,1],1000,replace=True)
med_cost = np.random.normal(500,100,1000)
medicare_ind  = np.random.choice([0,0,0,0,0,1],1000,replace=True)

## fake data for now
# dat_john = np.vstack((age,gender,aso, med_cost,medicare_ind)).T


age_mean, age_var  = calc_exp_age(mem_65,mem_50,mem_35,mem_18)

## first we will shift the data and then add another column for interaction and then 

## need to modify fcn to include both medicare cost and commercial cost


#(age,gender,aso, med_cost,medicare_ind)

shifted_data = shift_dat(dat_john,age_mean,float(100-pct_fem)/100,(float(aso_mem)/float(tot_mem)),
    medicare_cos,commerc_cos,float(100-pct_commerc)/100)

#shifted_data = shift_dat(dat_john,age_mean,float(100-pct_fem)/100,(float(aso_mem)/float(tot_mem)),medicare_cos,commerc_cos,float(100-pct_commerc)/100)
#shifted_data = shift_dat(dat_john, 40,400,0.3,0.2,0.8)


# we augment the data with the interaction term between medicare and cost
new_vec = (shifted_data[:,3]*shifted_data[:,4]).reshape((shifted_data.shape[0],1))
shifted_data_aug = np.hstack((shifted_data,new_vec))


## multiply num_targ by tot_pop_size/1000 (bc we're using 1000 as 
## fake data, will change with John's data) and output it.  note that the array should be 
## including the intercept



## shifted data without the aso members


#[0,1,3,4,5]

non_aso_inds = [not bool(x) for x in shifted_data[:,2]]
shifted_no_aso = shifted_data_aug[np.where(non_aso_inds)[0],:][:,[0,1,3,4,5]]

targ_params = np.array([-4.818, 0.04763, 0.06332, 0.0005781, -0.2397, 0.002776])
#targ_params = np.array([0.001,0.002,0.001,0.005,0.05,0.007,0.09])
size_john = dat_john.shape[0]
# num_targ = (pred_targ(shifted_data_aug, targ_params))*tot_mem/size_john
num_targ = (np.sum(pred_targ(shifted_no_aso, targ_params)))*tot_mem/size_john


# 7 is number of input betas including intercept, and 6 is the number of outcomes
# beta_block  = np.random.normal(1,1,size=7*6).reshape((7,6))


in_out_admit = samp_bin(shifted_no_aso,bin_hoi_admit_pars)
#shifted_with_count = np.vstack(in_out_admit, shifted_no_aso.T).T
count_admit = samp_pois(shifted_no_aso, admit_count_pars)
# [np.where(in_out_admit),:]

in_out_er_admit = samp_bin(shifted_no_aso,bin_hoi_er_pars)
#shifted_with_count = np.vstack(in_out_admit, shifted_no_aso.T).T
count_er_admit = samp_pois(shifted_no_aso,er_count_pars)
# [np.where(in_out_er_admit),:]

in_out_med_elig = samp_bin(shifted_no_aso,bin_med_elig_pars)
in_out_rx_elig = samp_bin(shifted_no_aso,bin_rx_elig_pars)

np.mean(count_admit)
np.mean(count_er_admit)

# hoi_admit_se = 34182.9
# hoi_er_se = 2545.923
# hoi_admit_allowed_pars = np.array([1.587756e+04,1.692435e+04,-1.221757e+02,4.018496e+03,-5.664562e-02,-1.311798e+04,8.390785e-01])
# hoi_er_allowed_pars = np.array([8.902975e+02,7.733673e+02,8.436298e+00,2.408804e+02,9.773301e-02,-1.571199e+03,-6.495687e-02])

# admit_cost  = samp_normal(shifted_no_aso,hoi_admit_allowed_pars,hoi_admit_sd)
# [np.where(in_out_admit),:]

# er_cost = samp_normal(shifted_no_aso,hoi_er_allowed_pars,hoi_er_sd)
# [np.where(in_out_er_admit),:]



tmp_size = shifted_no_aso.shape[0]

shifted_no_aso_tmp1 = np.hstack((count_admit.reshape((tmp_size,1)),shifted_no_aso))
shifted_no_aso_tmp2 = np.hstack((count_er_admit.reshape((tmp_size,1)),shifted_no_aso))

admit_cost  = samp_normal(shifted_no_aso_tmp1,hoi_admit_allowed_pars,hoi_admit_sd)
# [np.where(in_out_admit),:]

er_cost = samp_normal(shifted_no_aso_tmp2,hoi_er_allowed_pars,hoi_er_sd)





in_out_diab = samp_bin(shifted_no_aso, bin_diab_pars)
in_out_resp = samp_bin(shifted_no_aso, bin_resp_pars)
in_out_cvd = samp_bin(shifted_no_aso, bin_cvd_pars)
in_out_doi = samp_bin(shifted_no_aso, bin_all_pars)

med_allowed = samp_normal(shifted_no_aso,med_allowed_pars,med_allowed_sd)
rx_allowed = samp_normal(shifted_no_aso,rx_allowed_pars,rx_allowed_sd)



## OUTPUT parameters
p1 = num_targ

p2 =np.mean(count_admit[np.where(in_out_admit)[0]])
p3 = np.sum(admit_cost[np.where(in_out_admit)[0]])/np.sum(count_admit[np.where(in_out_admit)[0]])

p4 = np.mean(count_er_admit[np.where(in_out_er_admit)[0]])
p5 = np.sum(er_cost[np.where(in_out_er_admit)[0]])/np.sum(count_er_admit[np.where(in_out_er_admit)[0]])

p6 = np.mean(rx_allowed[np.where(in_out_rx_elig)[0]])
p7 = np.mean(med_allowed[np.where(in_out_med_elig)[0]])
##### END OUTPUT PARAMS






temp = np.array(pd.DataFrame(index=range(tmp_size),columns =range(10)))

col2_comp = count_admit
# [np.where(in_out_admit),:]
temp[np.where(in_out_admit)[0],1] = col2_comp[np.where(in_out_admit)[0]]

col1_comp = count_er_admit 
temp[np.where(in_out_er_admit)[0],0] = col1_comp[np.where(in_out_er_admit)[0]]
# [np.where(in_out_er_admit),:]

col10_comp = samp_normal(shifted_no_aso, pdc_diab_pars,pdc_diab_sd)
temp[np.where(in_out_diab)[0],9] = col10_comp[np.where(in_out_diab)[0]]

# [np.where(in_out_diab),:]
col9_comp = samp_normal(shifted_no_aso, pdc_resp_pars,pdc_resp_sd)
# [np.where(in_out_resp),:]
temp[np.where(in_out_resp)[0],8] = col9_comp[np.where(in_out_resp)[0]]

col8_comp = samp_normal(shifted_no_aso, pdc_cvd_pars,pdc_cvd_sd)
# [np.where(in_out_cvd),:]
temp[np.where(in_out_cvd)[0],7] = col8_comp[np.where(in_out_cvd)[0]]

col7_comp = samp_normal(shifted_no_aso, pdc_doi_pars,pdc_doi_sd)
# [np.where(in_out_doi),:]
temp[np.where(in_out_doi)[0],6] = col7_comp[np.where(in_out_doi)[0]]


col6_comp = samp_normal(shifted_no_aso,cost_diab_pars,cost_diab_sd)
# [np.where(in_out_diab),:]
temp[np.where(in_out_diab)[0],5] = col6_comp[np.where(in_out_diab)[0]]


col5_comp = samp_normal(shifted_no_aso,cost_resp_pars,cost_resp_sd)
# [np.where(in_out_resp),:]
temp[np.where(in_out_resp)[0],4] = col5_comp[np.where(in_out_resp)[0]]


col4_comp = samp_normal(shifted_no_aso,cost_cvd_pars,cost_cvd_sd)
# [np.where(in_out_cvd),:]
temp[np.where(in_out_cvd)[0],3] = col4_comp[np.where(in_out_cvd)[0]]

col3_comp = samp_normal(shifted_no_aso,cost_doi_pars,cost_doi_sd)
# [np.where(in_out_doi),:]
temp[np.where(in_out_doi)[0],2] = col3_comp[np.where(in_out_doi)[0]]






#### WRITE-OUT

header_nams_small = ["targetPopulation","admitsPerPerson","dollarsPerAdmit","erVisitsPerPerson","dollarsPerERVist","avgDrugCost","avgMedicalCost"]
pd.DataFrame(np.array([p1,p2,p3,p4,p5,p6,p7]).reshape((1,7))).to_csv(out_path+'_params.csv',index=False,header=header_nams_small)


# pd.DataFrame(np.hstack((num_targ,more_out_params)).reshape((1,7))).to_csv(out_path+'_params.csv',index=False,header=header_nams_small)


# 7 is number of input betas including intercept, and 10 is the number of columns to be written to csv
# beta_block2  = np.random.normal(1,1,size=7*10).reshape((7,10))

## in general, covariance matrix will not be identity -- still working on modelling
# pop_array = pred_output_pop(shifted_data_aug,beta_block2,covari = np.diag(np.ones(10)))

head_nams = ["predicted_er","predicted_admit","DOICost","CardioCost","DiabetesCost","RespiratoryCost","PDC_All_DOI","PDC_Cardio","PDC_Resp","PDC_Diabetes"]
# pd.DataFrame(pop_array).to_csv(out_path+'_pop.csv',index=False,header=head_nams)
pd.DataFrame(temp).to_csv(out_path+'_pop.csv',index=False,header=head_nams)


#dat_cambia = pd.read_csv('/Volumes/dswanson/product/full_pop.txt')








