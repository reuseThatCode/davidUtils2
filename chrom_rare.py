import sys
import re
import subprocess
import os
import time


print os.getcwd()

## how to run: python chrom_rare.py asdf.vcf or asdf.vcf.gz

## to be run from /Groups/..../GWAS-results/ like:
## python job_recovery ceruplasmin

## sys.argv[1] specifies the relative path up to the biomarker whose jobs you're interesting in resubmitting

# p1 = os.getcwd()+'/'+sys.argv[1]+'/emmax'
# p2 = p1+'/tmp'

# subprocess.call('echo '+p1,shell=True)
# subprocess.call('echo '+p2,shell=True)

# # emmax_out = os.listdir('ceruloplasmin/emmax/')
# # tmp_out = os.listdir('ceruloplasmin/emmax/tmp')

# emmax_out = os.listdir(p1)
# tmp_out = os.listdir(p2)

# ps_test = filter(lambda x:re.search(r'.+\.ps',x), emmax_out)
# sh_test = filter(lambda x:re.search(r'.+.sh',x), tmp_out)

# ps_rm = [re.sub('\.ps','',x) for x in ps_test]
# sh_rm = [re.sub('\-emmax\.sh','',x) for x in sh_test]

# inds = [x not in ps_rm for x in sh_rm]
# sh_to_submit = [sh_test[i] for i in range(len(sh_test)) if inds[i]]

# sh_to_submit2 = ['qsub '+p2 + '/'+ x for x in sh_to_submit]

## show's in stdout what I'm submitting
#[subprocess.call('echo ' +x,shell=True) for x in sh_to_submit2]

## actual submission here
#[subprocess.call(x,shell=True) for x in sh_to_submit2]


#! /bin/bash

## MAKE TMP DIRECTORY TO STORE VCF
#subprocess.call('mkdir ./tmp/',shell=True)
## UNZIP .GZ VCF FILE
## subprocess.call('gunzip '+sys.argv[1] +' ./tmp/',shell=True)

if re.search('gz$',sys.argv[1]):
    subprocess.call('gunzip '+sys.argv[1] ,shell=True)
    nam_vcf = re.sub('\.gz','',sys.argv[1])
else:
    nam_vcf = sys.argv[1]


### where sys.argv[1] points to the .gz vcf file
##nam_vcf = re.sub('\.gz','',sys.argv[1])
nam_dir = re.sub('\.vcf','',nam_vcf)


print nam_dir

new_dir = os.getcwd() + '/' + nam_dir
subprocess.call('mkdir ' + new_dir ,shell=True)



## chromosome sizes for 
chrom_size = [249213345,243102469,197955678,191013442,180848396,171006035,158937649,146281416,141139819,135498458,134856693,133812422,115092803,107283085,102519296,90289081,81188573,78005397,59110852,62934707,48085155,51238065]

##gunzip [ ] ./tmp/
## APPLY VCFTOOLS TO

## /InovaCommon/software/vcftools_0.1.12b/bin/vcftools --vcf /InovaCommon/chr19.vcf --chr chr19 --from-bp 1 --to-bp 10000000  --012 --singletons --out chr19_sub_singleton

p = re.compile('[0-9]+')
chr_num = int(p.search(str(nam_vcf)).group())

## taking the ceiling in this contrived way would not work if any of the entries in chrom_size was a multiple of the number we divide by, but no such issue


size_piece = 2000000

iter_times = int(chrom_size[(chr_num-1)]/float(size_piece) + 1)

print 'The number of pieces of this chromosome is '+ str(iter_times)

#i=2
# print '/InovaCommon/software/vcftools_0.1.12b/bin/vcftools --vcf ' +os.getcwd()+ '/' + str(nam_vcf) +' --chr chr'+ str(chr_num) +'  --from-bp '

# print  str(float(size_piece)*(i-1))+ ' --to-bp ' + str(float(size_piece)*i) + ' --maf 0.000000001 --max-maf 0.05 --012 --out ' + new_dir + '\/chr'

# print  chr_num  + '_part' + str(i) + '  > chr' + str(chr_num)  +'_part' + str(i) +'.sh'

#for i in range(1,3):
for i in range(1,iter_times+1):
    subprocess.call('echo \"/InovaCommon/software/vcftools_0.1.12b/bin/vcftools --vcf ' +os.getcwd()+ '/' + str(nam_vcf) +' --chr chr'+ str(chr_num) +'  --from-bp '+ str(int(float(size_piece)*(i-1)))+ ' --to-bp ' + str(int(float(size_piece)*i)) + ' --maf 0.000000001 --max-maf 0.05 --012 --out ' +new_dir+ '/chr' + str(chr_num)  +'_part'  +str(i) + '\"  > '+new_dir+'/chr' + str(chr_num)  +'_part' + str(i) +'.sh',shell=True)
    subprocess.call('echo \"Rscript /InovaOutput5/david_code/rare_variant.R ' + new_dir +'/chr' + str(chr_num) + '_part' + str(i) + '\" >> '+new_dir+'/chr' + str(chr_num)  +'_part' +str(i) +'.sh',shell=True)
## note that this commented line reads files from where python was run, not where you defined the -wd flag, so need to make change
#    subprocess.call('qsub -wd \"'+new_dir+"\" chr"+ str(chr_num) +'_part' +str(i) +'.sh',shell=True)


######################### COMMENTED OUT FOR MANUAL BASH SUBMISSION ###########

#     subprocess.call('qsub -V -wd \"'+new_dir+"\" "+new_dir+ "/chr"+ str(chr_num) +'_part' +str(i) +'.sh',shell=True)

# ## g.issuperset(g3)
# file_dir = set(os.listdir('.'))

# for i in range(1,iter_times+1):
#     r_ob_nam = 'chr' + str(chr_num) + '_part'+str(i) + '.RObject'

# lis_r_objects = ['chr' + str(chr_num) + '_part'+str(j) + '.RObject' for j in range(1,iter_times+1)]

# while not file_dir.issuperset(lis_r_objects):
#     time.sleep(30)
    
# ##subprocess.call(   ,shell=True)
# ## monitor queue as well somewhere?

# subprocess.call('Rscript concat.R ' + " ".join(lis_r_objects), shell=True)

##############  COMMENTING OUT ABOVE REGION FOR MANUAL BASH SUBMISSION ##############

# echo "mkdir test_dir4" | qsub -wd
# while len([x for x in file_dir if x in file_dir]:
#     time.sleep(20)