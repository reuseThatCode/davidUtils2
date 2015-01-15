

import numpy as np
import pandas as pd
from scipy.linalg import sqrtm


def rank(vec):
    temp = vec.argsort()
    ranks = np.empty(len(vec), int)
    ranks[temp] = np.arange(len(vec))
    return ranks

def create_null(y_out,dat,w,its,sqrt_P):
    n = dat.shape[0]
    K = (dat.dot(np.diag(w))).dot(dat.T)
    num = len(y_out)
    sym_mat = (sqrt_P.dot(K)).dot(sqrt_P)
#     return sym_mat
    lam = np.linalg.eig(sym_mat)
    chi_mat = np.random.chisquare(1,size=its*num).reshape((its,num))
    
    null = chi_mat.dot(lam[0].astype('float64'))
    return null
    

# find_sqrt_P <- function(y_out){
#         n <- length(y_out)
#         sd_y <- sd(y_out)
#         P <- diag(sd_y,n) - matrix(sd_y/n,nrow=n,ncol=n)
#         sqrt_P <- find_sqrt(P)

#         return(sqrt_P)
# }

def find_sqrt_P(y_out):
    n = len(y_out)
    sd_y = np.std(y_out)
    P = sd_y* np.diag(np.ones(n)) - ((sd_y/n)*np.ones(n**2)).reshape((n,n))
    sqrt_P = sqrtm(P)
#     sqrt_P = find_sqrt(P)
    return sqrt_P.astype('float64')


# ## not using in current implementation -- 1-15-15 4:47pm
# def find_sqrt(mat):
#     eig_mat = np.linalg.eig(mat)
#     eig_values = eig_mat[0]
# #     eig_values[len(eig_values)-1] = 0
#     sqrt_mat_try = ((eig_mat[1]).dot(np.diag(np.sqrt(eig_values)))).dot(eig_mat[1].T)
#     return sqrt_mat_try
    
#         eig_mat <- eigen(mat)
#         eig_values <- eig_mat$values
#         # do this to correct for some numerical issues in computation matrix root                                  
#         eig_values[dim(mat)[1]] <- 0
#         sqrt_mat_try <- (eig_mat$vectors)%*%diag(sqrt(eig_values))%*%t(eig_mat$vectors)
#         return(sqrt_mat_try)


#         eig_mat <- eigen(mat)
#         eig_values <- eig_mat$values
#         # do this to correct for some numerical issues in computation matrix root                                  
#         eig_values[dim(mat)[1]] <- 0
#         sqrt_mat_try <- (eig_mat$vectors)%*%diag(sqrt(eig_values))%*%t(eig_mat$vectors)
#         return(sqrt_mat_try)



def SKAT_stat(y_out,dat,w,null_dist):
    n = dat.shape[0]
    K = (dat.dot(np.diag(w))).dot(dat.T)
    sd_y = np.std(y_out)
    
    y_cent = y_out - np.mean(y_out)
    # for vectors, the '.dot' method seems to orient vector so as to always achieve inner product 
    # so that no transpositions needed here
    test_stat = (y_cent.dot(K)).dot(y_cent)    
#     print test_stat
    return (1-rank(np.concatenate(([test_stat],null_dist)))[0]/float(len(null_dist)+2))


dat = pd.read_csv('/Users/davidswanson/rare_var_dat.csv')

# dat2 = np.array(dat)

y_out = np.array(dat.iloc[:,0])
x_dat = np.array(dat.iloc[:,1:(dat.shape[1])])

sqrt_P = find_sqrt_P(y_out)
w = np.ones(30)
its = 4000

null_dist = create_null(y_out,x_dat,w,its,sqrt_P)

SKAT_stat(y_out,x_dat,w,null_dist)

