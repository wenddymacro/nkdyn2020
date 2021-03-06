##### MAIN FILE #####

# This file executes the scripts used in the paper
# "Taylor rule(s) inflation", by E. Franceschi - PSE and Paris 1
# 
# The execution of the full batch of scripts requires
# a working version of Matlab equipped with Dynare.
# 
# The scripts executes the following tasks:
#		- simulate with Dynare 4 DSGE models, stock the inflation
# 			series and run the same econometric tests as the real inflation
#			data, plotting the AR properties of these models;
#		- solve and simulate the liquidity NKDSGE model with two monetary rules,
#			generate the IRFs; simulate the standard NKDSGE model for regular,
#			extremely aggressive and almost passive Central Banks. For all these
#			models and cases plot and compare IRFs.
#
# v0.8


# Functions
source('functs.R', verbose=F, echo=F)
tic('Total time')

# ##### I - Inflation simulations #####
tic('Models and simulations')
get_matlab()
run_matlab_script('./models/models_main.m',
                  verbose = F)
toc()


##### II - AR(p*) estimates on simulated inflation ######
# set up the parallel plan
future::plan(multiprocess)
tic('AR(p*) estimates')
source('sim_pi.R')
toc()

##### III - VISUALIZATION ####
tic('Graphing')
source('plotte.R', verbose=F, echo=F)
toc()


# housekeeping
rm()
toc()