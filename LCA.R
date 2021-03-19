######################
## Loading Packages ##
######################

library(readr)
library(haven)
library(tidyverse)
library(psych)
library(rsample)
library(gtsummary)
library(poLCA)
library(mice)

##################
## Loading Data ##
##################

FullData <- read_csv("C:/Users/zkunicki/Documents/Research/Dissertation Secondary Analyses/FullData.csv")


#########################################
## Selecting for Variables of Interest ##
#########################################

BPA_Data <- FullData %>% dplyr::select(URI1MTurk2, BPA1, BPA2, BPA3, BPA4, BPA5, BPA6,
                                       BPA7, BPA8, BPA9, BPA10, BPA11, BPA12, BPA13, GodinF) %>% 
  dplyr::mutate(GodinF_recode = dplyr::if_else(GodinF == "Never/Rarely", 1,
                                dplyr::if_else(GodinF == "Sometimes", 2,
                                dplyr::if_else(GodinF == "Often", 3, NA_real_)))) %>% 
  dplyr::select(-GodinF)

##########
#### Missing Data
##########

table(is.na(BPA_Data))

# FALSE  TRUE 
# 18245    55

# Analyze all the data, we must.

set.seed(101) #because 101 is better than 100 (joke doesn't make sense here)

BPA_imputed <- mice(BPA_Data, m = 10) # 10 imputations

BPA_imp_long <- complete(BPA_imputed, action = "long") # Make long form data

BPA_imp <- BPA_imp_long %>%  # Aggregate into single dataset
  group_by(.id) %>% 
  summarise(BPA1 =  round(median(BPA1),0),
            BPA2 =  round(median(BPA2),0),
            BPA3 =  round(median(BPA3),0),
            BPA4 =  round(median(BPA4),0),
            BPA5 =  round(median(BPA5),0),
            BPA6 =  round(median(BPA6),0),
            BPA7 =  round(median(BPA7),0),
            BPA8 =  round(median(BPA8),0),
            BPA9 =  round(median(BPA9),0),
            BPA10 = round(median(BPA10),0),
            BPA11 = round(median(BPA11),0),
            BPA12 = round(median(BPA12),0),
            BPA13 = round(median(BPA13),0),
            URI1MTurk2 = median(URI1MTurk2),
            GodinF = median(GodinF_recode)) %>% 
  ungroup()


## Checking imputation

table(BPA_imp$BPA13) # all looks good

##########
#### Splitting Sample
##########

uri_data <- BPA_imp %>% 
  filter(URI1MTurk2 == 1) %>% 
  dplyr::select(BPA1:BPA13)

mturk_data <- BPA_imp %>% 
  filter(URI1MTurk2 == 2) %>% 
  dplyr::select(BPA1:BPA13)

####
## URI LCA
####

lca_formula <- as.formula(cbind(BPA1, BPA2, BPA3, BPA4, BPA5, BPA6, BPA7, BPA8, BPA9, BPA10, BPA11, BPA12, BPA13)~1)

URI_LCA_2class <- poLCA(lca_formula, data = uri_data, nclass = 2)
URI_LCA_3class <- poLCA(lca_formula, data = uri_data, nclass = 3)
URI_LCA_4class <- poLCA(lca_formula, data = uri_data, nclass = 4)
URI_LCA_5class <- poLCA(lca_formula, data = uri_data, nclass = 5)
URI_LCA_6class <- poLCA(lca_formula, data = uri_data, nclass = 6)


URI_lowest_BIC <- min(cbind(URI_LCA_2class$bic,URI_LCA_3class$bic,
                            URI_LCA_4class$bic,URI_LCA_5class$bic,URI_LCA_6class$bic))

#Lowest BIC is the 3 class model

URI_LCA_3class

# Conditional item response (column) probabilities,
# by outcome variable, for each class (row) 
# 
# $BPA1
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0714 0.1934 0.4052 0.2867 0.0434
# class 2:  0.2124 0.2698 0.3340 0.1198 0.0640
# class 3:  0.0245 0.1134 0.2462 0.3887 0.2271
# 
# $BPA2
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0492 0.2217 0.3679 0.2795 0.0816
# class 2:  0.4114 0.3479 0.1497 0.0685 0.0226
# class 3:  0.1102 0.1309 0.2176 0.2919 0.2494
# 
# $BPA3
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.1233 0.3835 0.3429 0.1075 0.0429
# class 2:  0.7115 0.2165 0.0219 0.0500 0.0000
# class 3:  0.3907 0.2936 0.2151 0.0525 0.0481
# 
# $BPA4
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0000 0.2073 0.3649 0.2455 0.1823
# class 2:  0.5525 0.2582 0.1221 0.0444 0.0228
# class 3:  0.1839 0.1665 0.2098 0.2372 0.2026
# 
# $BPA5
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0817 0.3085 0.3634 0.1709 0.0755
# class 2:  0.6152 0.2063 0.1258 0.0489 0.0037
# class 3:  0.2388 0.2038 0.2549 0.1610 0.1415
# 
# $BPA6
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0389 0.1782 0.3223 0.3235 0.1370
# class 2:  0.5773 0.2095 0.1269 0.0388 0.0474
# class 3:  0.0917 0.1200 0.1308 0.2771 0.3803
# 
# $BPA7
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0178 0.1205 0.4060 0.3826 0.0732
# class 2:  0.1568 0.1560 0.2723 0.2533 0.1615
# class 3:  0.0200 0.0302 0.0866 0.2112 0.6520
# 
# $BPA8
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0381 0.0994 0.1984 0.3427 0.3214
# class 2:  0.5095 0.1919 0.1558 0.1048 0.0380
# class 3:  0.1599 0.0735 0.1066 0.2003 0.4596
# 
# $BPA9
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0000 0.0810 0.2698 0.5612 0.0880
# class 2:  0.2218 0.2512 0.3130 0.1973 0.0167
# class 3:  0.0347 0.0144 0.0992 0.2970 0.5546
# 
# $BPA10
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.3253 0.2715 0.2918 0.1022 0.0092
# class 2:  0.8770 0.0854 0.0376 0.0000 0.0000
# class 3:  0.5237 0.2009 0.1551 0.0727 0.0475
# 
# $BPA11
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0398 0.2222 0.3538 0.3379 0.0462
# class 2:  0.1327 0.2616 0.3094 0.1944 0.1020
# class 3:  0.0468 0.0289 0.1974 0.3075 0.4194
# 
# $BPA12
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0308 0.1547 0.4220 0.3563 0.0362
# class 2:  0.3767 0.2984 0.2411 0.0683 0.0155
# class 3:  0.0667 0.0898 0.2459 0.3308 0.2668
# 
# $BPA13
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0211 0.1101 0.2961 0.4711 0.1016
# class 2:  0.1241 0.1765 0.3008 0.2318 0.1668
# class 3:  0.0072 0.0048 0.0131 0.2124 0.7624
# 
# Estimated class population shares 
# 0.3257 0.3871 0.2872 
# 
# Predicted class memberships (by modal posterior prob.) 
# 0.3319 0.3861 0.2819 
# 
# ========================================================= 
#   Fit for 3 latent classes: 
#   ========================================================= 
#   number of observations: 720 
# number of estimated parameters: 158 
# residual degrees of freedom: 562 
# maximum log-likelihood: -12780.05 
# 
# AIC(3): 25876.11
# BIC(3): 26599.63
# G^2(3): 16091.53 (Likelihood ratio/deviance statistic) 
# X^2(3): 2207264084 (Chi-square goodness of fit) 

####
## MTurk LCA
####

lca_formula <- as.formula(cbind(BPA1, BPA2, BPA3, BPA4, BPA5, BPA6, BPA7, BPA8, BPA9, BPA10, BPA11, BPA12, BPA13)~1)

mturk_LCA_2class <- poLCA(lca_formula, data = mturk_data, nclass = 2)
mturk_LCA_3class <- poLCA(lca_formula, data = mturk_data, nclass = 3)
mturk_LCA_4class <- poLCA(lca_formula, data = mturk_data, nclass = 4)
mturk_LCA_5class <- poLCA(lca_formula, data = mturk_data, nclass = 5)
mturk_LCA_6class <- poLCA(lca_formula, data = mturk_data, nclass = 6)


mturk_lowest_BIC <- min(cbind(mturk_LCA_2class$bic,mturk_LCA_3class$bic,
                            mturk_LCA_4class$bic,mturk_LCA_5class$bic,mturk_LCA_6class$bic))

#Lowest BIC is the 3 class model

mturk_LCA_3class

# Conditional item response (column) probabilities,
# by outcome variable, for each class (row) 
# 
# $BPA1
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0933 0.1414 0.1272 0.4021 0.2360
# class 2:  0.2195 0.2739 0.1981 0.2111 0.0973
# class 3:  0.0211 0.2367 0.3343 0.3833 0.0246
# 
# $BPA2
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.1527 0.1572 0.1456 0.2249 0.3195
# class 2:  0.4175 0.1992 0.1635 0.1874 0.0325
# class 3:  0.0872 0.2538 0.2494 0.2974 0.1123
# 
# $BPA3
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.4313 0.2786 0.1294 0.0657 0.0950
# class 2:  0.7078 0.1773 0.0420 0.0598 0.0131
# class 3:  0.1929 0.3588 0.2966 0.1438 0.0078
# 
# $BPA4
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.2053 0.1328 0.2188 0.1751 0.2681
# class 2:  0.4340 0.2299 0.1519 0.1116 0.0726
# class 3:  0.0000 0.2975 0.2149 0.4075 0.0800
# 
# $BPA5
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.2756 0.1782 0.1604 0.2145 0.1713
# class 2:  0.6378 0.1812 0.1156 0.0443 0.0210
# class 3:  0.1122 0.3035 0.2680 0.2503 0.0660
# 
# $BPA6
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.1609 0.1366 0.2057 0.2154 0.2814
# class 2:  0.5851 0.1232 0.1537 0.0655 0.0725
# class 3:  0.0385 0.2741 0.2709 0.3104 0.1061
# 
# $BPA7
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0128 0.0131 0.1513 0.2224 0.6005
# class 2:  0.2846 0.1557 0.2427 0.2698 0.0471
# class 3:  0.0140 0.1068 0.3639 0.4005 0.1149
# 
# $BPA8
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.1691 0.0572 0.1338 0.1616 0.4784
# class 2:  0.4833 0.1018 0.1266 0.1271 0.1612
# class 3:  0.1240 0.1628 0.1402 0.2631 0.3098
# 
# $BPA9
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0447 0.0163 0.0866 0.3669 0.4856
# class 2:  0.3411 0.1940 0.3316 0.1202 0.0130
# class 3:  0.0263 0.1340 0.3053 0.4244 0.1101
# 
# $BPA10
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.4020 0.2374 0.1814 0.0902 0.0891
# class 2:  0.7029 0.1603 0.0926 0.0306 0.0136
# class 3:  0.1699 0.4019 0.2884 0.1379 0.0020
# 
# $BPA11
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0221 0.0865 0.2848 0.2802 0.3263
# class 2:  0.2345 0.3540 0.2226 0.1378 0.0511
# class 3:  0.0231 0.2258 0.3719 0.3222 0.0570
# 
# $BPA12
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0909 0.1160 0.1950 0.1911 0.4069
# class 2:  0.4977 0.2339 0.1868 0.0816 0.0000
# class 3:  0.0607 0.1880 0.3439 0.3881 0.0193
# 
# $BPA13
# Pr(1)  Pr(2)  Pr(3)  Pr(4)  Pr(5)
# class 1:  0.0132 0.0219 0.0628 0.2726 0.6295
# class 2:  0.3433 0.1804 0.2913 0.1544 0.0305
# class 3:  0.0275 0.1395 0.3221 0.4167 0.0943
# 
# Estimated class population shares 
# 0.3517 0.2899 0.3585 
# 
# Predicted class memberships (by modal posterior prob.) 
# 0.34 0.294 0.366 
# 
# ========================================================= 
#   Fit for 3 latent classes: 
#   ========================================================= 
#   number of observations: 500 
# number of estimated parameters: 158 
# residual degrees of freedom: 342 
# maximum log-likelihood: -9241.255 
# 
# AIC(3): 18798.51
# BIC(3): 19464.42
# G^2(3): 12273.45 (Likelihood ratio/deviance statistic) 
# X^2(3): 1422668155 (Chi-square goodness of fit) 


####
## Plots
####

plot(URI_LCA_3class)

plot(mturk_LCA_3class)


####
## GodinF
####

URI_GodinF <- BPA_imp %>% dplyr::filter(URI1MTurk2 == 1) %>% dplyr::select(GodinF)
URI_GodinF$URI_pred_classes <- URI_LCA_3class$predclass

MTurk_GodinF <- BPA_imp %>% dplyr::filter(URI1MTurk2 == 2) %>% dplyr::select(GodinF)
MTurk_GodinF$MTurk_pred_classes <- mturk_LCA_3class$predclass

uri_validity <- aov(GodinF ~ as.factor(URI_pred_classes), data = URI_GodinF)

summary(uri_validity)
TukeyHSD(uri_validity)
plot(TukeyHSD(uri_validity))

MTurk_validity <- aov(GodinF ~ as.factor(MTurk_pred_classes), data = MTurk_GodinF)

summary(MTurk_validity)
TukeyHSD(MTurk_validity)
plot(TukeyHSD(MTurk_validity))
