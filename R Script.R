################################
## Step One: Loading Packages ##
################################

library(readr)
library(haven)
library(tidyverse)
library(lavaan)
library(psych)
library(mirt)
library(rsample)
library(gtsummary)

############################
## Step Two: Loading Data ##
############################

FullData <- read_csv("C:/Users/zkunicki/Documents/Research/Dissertation Secondary Analyses/FullData.csv")


#####################################################
## Step Three: Selecting for Variables of Interest ##
#####################################################

BPA_Data <- FullData %>% select(URI1MTurk2, BPA1, BPA2, BPA3, BPA4, BPA5, BPA6, BPA7, BPA8, BPA9, BPA10, BPA11,
                                BPA12, BPA13, GodinS, GodinM, GodinL, Gender1F2M) 

table(is.na(BPA_Data))

## Checking for missing data ##


#(56 / (56 + 21904))*100 = 0.26
# FALSE  TRUE 
# 21904    56 

URI_Sample <- BPA_Data %>% filter(URI1MTurk2 == 1) %>% drop_na()   #N = 697
MTurk_Sample <- BPA_Data %>% filter(URI1MTurk2 == 2) %>% drop_na() #N = 484

######################################
## Step Four: Splitting for EFA/IRT ##
######################################

## Remember that we want larger sample sizes for IRT, so training for IRT and testing for PCA.
## 25% of data used for PCA, 75% used for IRT.

PCA_IRT_MTurk_Split <- initial_split(MTurk_Sample)

PCA.MTurk <- testing(PCA_IRT_MTurk_Split)
IRT.MTurk <- training(PCA_IRT_MTurk_Split)

PCA_IRT_URI_Split <- initial_split(URI_Sample)

PCA.URI <- testing(PCA_IRT_URI_Split)
IRT.URI <- training(PCA_IRT_URI_Split)

#############################
## Step Five: Descriptives ##
#############################

PCA.URI.BPAonly <- PCA.URI %>% select(BPA1, BPA2, BPA3, BPA4, BPA5, BPA6, BPA7, BPA8, BPA9, BPA10, BPA11,
                                      BPA12, BPA13)

describe(PCA.URI.BPAonly)

# vars     n mean   sd median trimmed  mad min max range  skew kurtosis   se
# BPA1     1 174 3.07 1.19      3    3.09 1.48   1   5     4 -0.30    -0.84 0.09
# BPA2     2 174 2.73 1.29      3    2.66 1.48   1   5     4  0.16    -1.07 0.10
# BPA3     3 174 1.90 1.01      2    1.75 1.48   1   5     4  0.98     0.25 0.08
# BPA4     4 174 2.57 1.31      2    2.47 1.48   1   5     4  0.37    -0.99 0.10
# BPA5     5 174 2.19 1.18      2    2.06 1.48   1   5     4  0.66    -0.60 0.09
# BPA6     6 174 2.84 1.46      3    2.80 1.48   1   5     4  0.08    -1.40 0.11
# BPA7     7 174 3.55 1.23      4    3.67 1.48   1   5     4 -0.56    -0.63 0.09
# BPA8     8 174 2.98 1.56      3    2.98 2.97   1   5     4 -0.04    -1.55 0.12
# BPA9     9 174 3.40 1.23      4    3.50 1.48   1   5     4 -0.43    -0.77 0.09
# BPA10   10 174 1.74 1.04      1    1.56 0.00   1   5     4  1.15     0.16 0.08
# BPA11   11 174 3.29 1.15      3    3.32 1.48   1   5     4 -0.16    -0.84 0.09
# BPA12   12 174 2.86 1.20      3    2.86 1.48   1   5     4 -0.14    -0.96 0.09
# BPA13   13 174 3.79 1.13      4    3.93 1.48   1   5     4 -0.72    -0.16 0.09

# All looks good

PCA.MTurk.BPAonly <- PCA.MTurk %>% select(BPA1, BPA2, BPA3, BPA4, BPA5, BPA6, BPA7, BPA8, BPA9, BPA10, BPA11,
                                      BPA12, BPA13)

describe(PCA.MTurk.BPAonly)

#   vars   n mean   sd median trimmed  mad min max range  skew kurtosis   se
# BPA1     1 121 3.24 1.21      3    3.29 1.48   1   5     4 -0.21    -0.94 0.11
# BPA2     2 121 2.86 1.37      3    2.82 1.48   1   5     4  0.06    -1.26 0.12
# BPA3     3 121 2.09 1.13      2    1.96 1.48   1   5     4  0.73    -0.51 0.10
# BPA4     4 121 3.00 1.32      3    3.00 1.48   1   5     4 -0.11    -1.17 0.12
# BPA5     5 121 2.50 1.25      2    2.44 1.48   1   5     4  0.25    -1.15 0.11
# BPA6     6 121 2.80 1.39      3    2.75 1.48   1   5     4  0.06    -1.31 0.13
# BPA7     7 121 3.53 1.18      4    3.64 1.48   1   5     4 -0.53    -0.46 0.11
# BPA8     8 121 3.22 1.58      3    3.28 2.97   1   5     4 -0.23    -1.51 0.14
# BPA9     9 121 3.26 1.28      3    3.33 1.48   1   5     4 -0.43    -0.81 0.12
# BPA10   10 121 2.27 1.16      2    2.16 1.48   1   5     4  0.51    -0.69 0.11
# BPA11   11 121 3.05 1.19      3    3.06 1.48   1   5     4  0.08    -0.73 0.11
# BPA12   12 121 2.82 1.35      3    2.77 1.48   1   5     4  0.13    -1.16 0.12
# BPA13   13 121 3.24 1.29      3    3.30 1.48   1   5     4 -0.31    -0.93 0.12

# All looks good

###################
## Step Six: EFA ##
###################

VSS(PCA.URI.BPAonly)
# The Velicer MAP achieves a minimum of 0.03  with  2  factors 

VSS(PCA.MTurk.BPAonly)
# The Velicer MAP achieves a minimum of 0.03  with  1  factors 

fa(PCA.URI.BPAonly, nfactors = 2, rotate = "Promax", fm = "wls")
#Drop items 9 (complex loadings), 12 (complex loadings)
#SRMR = 0.06

fa(PCA.MTurk.BPAonly, nfactors = 2, rotate = "Promax", fm = "wls")
#Retain all items
#SRMR = 0.07

#F1 = 1, 2, 3, 4, 5, 6, 8, 10, 11
#F2 = 7, 9, 12, 13

#####################
## Step Seven: IRT ##
#####################

mirt.model <- 'F1 = 1-6, 8, 10, 11
               F2 = 7, 9, 12, 13
               COV = F1*F2'

IRT.URI.BPAOnly <- IRT.URI %>% select(BPA1, BPA2, BPA3, BPA4, BPA5, BPA6, BPA7, BPA8, BPA9, BPA10, BPA11,
                                      BPA12, BPA13)

IRT.MTurk.BPAOnly <- IRT.MTurk %>% select(BPA1, BPA2, BPA3, BPA4, BPA5, BPA6, BPA7, BPA8, BPA9, BPA10, BPA11,
                                      BPA12, BPA13)

IRT.URI.GRM <- mirt(IRT.URI.BPAOnly, mirt.model, itemtype = "graded")

summary(IRT.URI.GRM)

M2(IRT.URI.GRM)

coef(IRT.URI.GRM)

IRT.MTurk.GRM <- mirt(IRT.MTurk.BPAOnly, mirt.model, itemtype = "graded")

summary(IRT.MTurk.GRM)

M2(IRT.MTurk.GRM)

coef(IRT.MTurk.GRM)

## DIF ##

IRT.MTurk.BPADif <- IRT.MTurk.BPAOnly 
IRT.URI.BPADif <- IRT.URI.BPAOnly 

IRT.DIF <- rbind(IRT.MTurk.BPADif, IRT.URI.BPADif)

group <- c(rep('MT', 363), rep('URI', 523))

DIF <- multipleGroup(IRT.DIF, mirt.model, group = group)

M2(DIF)

coef(DIF)

DIF(DIF, c('d1', 'd2', 'd3', 'd4'))

#           AIC    AICc   SABIC      HQ    BIC     X2 df     p
# BPA1  -15.327 -12.323  -8.884  -8.008  3.819 23.327  4 0.000
# BPA2  -13.465 -10.460  -7.021  -6.145  5.682 21.465  4 0.000
# BPA3    6.387   9.392  12.831  13.707 25.534  1.613  4 0.806
# BPA4   -1.681   1.324   4.763   5.639 17.466  9.681  4 0.046
# BPA5    0.650   3.655   7.094   7.970 19.797  7.350  4 0.119
# BPA6    7.666  10.671  14.110  14.986 26.813  0.334  4 0.988
# BPA7    4.468   7.473  10.912  11.788 23.615  3.532  4 0.473
# BPA8   -5.689  -2.685   0.754   1.630 13.458 13.689  4 0.008
# BPA9    4.575   7.580  11.019  11.895 23.722  3.425  4 0.489
# BPA10 -21.194 -18.189 -14.750 -13.874 -2.047 29.194  4 0.000
# BPA11   4.525   7.529  10.968  11.845 23.672  3.475  4 0.482
# BPA12   3.433   6.438   9.877  10.753 22.580  4.567  4 0.335
# BPA13   3.744   6.749  10.188  11.064 22.891  4.256  4 0.372

DIF(DIF, which.par = c('a2'), items2test = 1:13)