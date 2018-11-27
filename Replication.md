---
title: 'Citizen Attitudes towards Modern and Traditional Authority: Substitutes or
  Complements?'
author: 'Authors: Peter van der Windt, Macartan Humphreys, Lily Medina, Jeffrey Timmons, and Maarten Voors'
date: "October 22, 2018"
md_document:
    variant: github_document
output: 
  html_document: 
    highlight: tango
    keep_md: yes
    theme: cerulean
    toc: yes
---


[This .Rmd file]: https://github.com/medinali/VDW-H-M-T-V/blob/master/Replication.Rmd 
[data]: https://github.com/medinali/VDW-H-M-T-V/tree/master/data
[code]: https://github.com/medinali/VDW-H-M-T-V/tree/master/code
[output]: https://github.com/medinali/VDW-H-M-T-V/tree/master/output
["_Citizen Attitudes towards Modern and Traditional Authority: Substitutes or
  Complements?_"]: https://journals.sagepub.com/eprint/XiYVWJ8hAmTvFk6igcrg/full
[A pre-printed version of the paper]: http://www.macartan.nyc/wp-content/uploads/2018/11/VDW-H-M-T-V_paper.pdf
[here]: https://www.dropbox.com/sh/1knr88h3m0zagc9/AABcxXXQEqN8RgIx7_1LaO1Ga?dl=0


* [This .Rmd file] replicates all tables and figures reported in the manuscript ["_Citizen Attitudes towards Modern and Traditional Authority: Substitutes or
  Complements?_"].
* Data comes from the local folder "[data]". Helper functions are called up in the process from the local folder "[code]". Figures are put into local folder "[output]".
* [A pre-printed version of the paper] can be found online.



#### Options

Indicate your preferences here. Fyi, the structural model (which can be turned on below) takes several hours to complete. The results from the fitted models can be found [here]. You need to download these files to your local folder whenever `run_1re`  or `run_2re` are set to FALSE.


```r
# Stan Setup
# 1re: model with random effects at chiefdom level
# 2re: model with random effects at chiefdom level and village level

seed <- 1
# Run structural model
run_1re <- FALSE
run_2re <- FALSE
# Simulate samples of the posterior predictive distribution
run_posteriors <- FALSE

outname_1re <- "stan_1re.Rdata"
outname_2re <- "stan_2re.Rdata"

options(mc.cores = parallel::detectCores())

chains <- 20
iters <- 4000
max_divergent <- 0 # Maximum divervent transitions accepted
```

#### Load Data and Helper Files


```r
# Load data from the local folder "data"
# The cleaning process and creation of shock variables has been kept out, 
# as this required village-level GPS information, which cannot be made public.
hh <- read.csv(file = "data/hh.csv")
vill_data <- read.csv(file = "data/vill_data.csv")
shocks <- read.csv(file = "data/shocks.csv")

# Load additional helper functions
source("code/1_helper_functions.R")
```

# Table 1: Measures Related to the Modern Contract


```r
G1 <-  c( "q078_a_elections" , 
          "q078_b_complaints" ,
          "q078_c_suggestions" ,
          "q078_corruption" ,
          "q078_d_sensibiliser", 
          "q078_f_consult", 
          "q078_g_pork")
G2  <-  c("q077_a_vote", 
          "q077_b_complain",
          "q077_c_suggest", 
          "q077_d_meetings", 
          "q077_e_obey", 
          "q077_f_taxes", 
          "q077_g_support")
G3  <-  c("q076_edu_e", 
          "q076_sante_e")
G4  <-  c("q079_f_police", 
          "q079_g_state")
C1  <-  c("qd2_chief" , 
          "qd6_chief" , 
          "qd7_chief" )

summ <- function(var, my_data = hh) cbind( sum(!is.na(my_data[,`var`])), 
                                  round(mean(my_data[,`var`],na.rm = TRUE),3), 
                                  round(sd(my_data[,`var`],  na.rm = TRUE), 3), 
                                  min(my_data[,`var`], na.rm = TRUE),
                                  max(my_data[,`var`], na.rm = TRUE))

gvars <- data.frame(family = c("Responsibilities of the state", rep("",length(G1)),
                                "Responsibilities of citizens", rep("",length(G2)),
                                "Value of state activities",    rep("",length(G3)),
                                "Citizen activities",           rep("",length(G4)),
                                "Overall mean modern contract"),
                    

                       dep_var = c("Accept elections results", 
                                    "React to complaints", 
                                    "React to suggestions",
                                    "Avoid corruption", 
                                    "Keep people informed", 
                                    "Consult population",
                                    "Contribute resources",
                                    "---Family Mean---",
                                    "Vote",
                                    "Complain" ,
                                    "Make suggestions",
                                    "Attend meetings",
                                    "Obey the law", 
                                    "Pay Tax",
                                    "Support government",
                                    "---Family Mean---",
                                    "Providing education",
                                    "Providing health care",
                                    "---Family Mean---",
                                    "Contact police",
                                    " Visit to government agency", 
                                   "---Family Mean---",
                                   "")
                                    )

 gsumm <- cbind(gvars, t(sapply(c(G1, "G1", G2, "G2", G3, "G3", G4, "G4", "G"), summ)))

 kable( gsumm, row.names = FALSE, col.names = c("Family", "Dependent Variable", "Obs", "Mean", "St. Dev", "Min", "Max"), digits = 3)
```



Family                          Dependent Variable             Obs    Mean   St. Dev   Min   Max
------------------------------  ---------------------------  -----  ------  --------  ----  ----
Responsibilities of the state   Accept elections results      3734   0.239     0.427     0     1
                                React to complaints           3734   0.521     0.500     0     1
                                React to suggestions          3734   0.446     0.497     0     1
                                Avoid corruption              3734   0.302     0.459     0     1
                                Keep people informed          3734   0.273     0.446     0     1
                                Consult population            3734   0.310     0.463     0     1
                                Contribute resources          3734   0.562     0.496     0     1
                                ---Family Mean---             3734   0.379     0.319     0     1
Responsibilities of citizens    Vote                          3733   0.334     0.472     0     1
                                Complain                      3733   0.287     0.452     0     1
                                Make suggestions              3733   0.216     0.412     0     1
                                Attend meetings               3733   0.246     0.431     0     1
                                Obey the law                  3733   0.597     0.490     0     1
                                Pay Tax                       3733   0.459     0.498     0     1
                                Support government            3733   0.203     0.402     0     1
                                ---Family Mean---             3733   0.335     0.300     0     1
Value of state activities       Providing education           3699   0.151     0.358     0     1
                                Providing health care         3686   0.133     0.339     0     1
                                ---Family Mean---             3736   0.143     0.307     0     1
Citizen activities              Contact police                3721   0.033     0.178     0     1
                                Visit to government agency    3733   0.032     0.176     0     1
                                ---Family Mean---             3734   0.033     0.152     0     1
Overall mean modern contract                                  3758   0.222     0.180     0     1

# Table 2: Measures Related to the Traditional Contract


```r
cvars <- data.frame(family = c("Desired role of chief as development broker ",rep("",length(C1))),
                       dep_var = c("Beneficiary choice", 
                                    "Fund allocation", 
                                    "Project supervision",
                                    "---Family Mean---"))

csumm <- cbind(cvars, t(sapply(c(C1, "C1"), summ)))
    
kable(csumm , row.names = FALSE, col.names = c("Family", "Dependent Variable", "Obs", "Mean", "St. Dev", "Min", "Max"))
```



Family                                        Dependent Variable      Obs    Mean   St. Dev   Min   Max
--------------------------------------------  --------------------  -----  ------  --------  ----  ----
Desired role of chief as development broker   Beneficiary choice     3853   0.517     0.500     0     1
                                              Fund allocation        3853   0.422     0.494     0     1
                                              Project supervision    3853   0.380     0.485     0     1
                                              ---Family Mean---      3853   0.439     0.383     0     1


# Table 3: Measures Related to the Shocks


```r
summ2 <- function(x) c(length(x), mean(x), sd(x), min(x), max(x))

shocks_summ <- sapply(list(shocks$avg_rain_dev, shocks$Confl_Shock), summ2) %>% t()

rownames(shocks_summ) <- c("Rainfall Shock", "Conflict Shock")

kable(shocks_summ , col.names =  c(	"Obs.", "Mean", "St. Dev."," Min" ,"Max") , digits = 3 )
```

                  Obs.     Mean   St. Dev.     Min     Max
---------------  -----  -------  ---------  ------  ------
Rainfall Shock    1020    3.550      1.898    0.98   8.267
Conflict Shock    1020   -0.028      0.328   -2.50   2.250


# Table 4. Correlation between Modern and Traditional Contracts


```r
y <- c("G1", "G2" ,"G3", "G4", "G")

stargazer.lmrobust <- function(lmrobust_object, fe = "Y"){
 rows <- 10
 output <- rep("",rows)   
 pvalues <- lmrobust_object$p.value[c("C1", "age", "age2", "lit", "male")]
 stars   <- ifelse(pvalues < 0.01 ,"***", 
                   ifelse(pvalues < 0.05 & pvalues>=0.01, "**", 
                          ifelse(pvalues < 0.1 & pvalues>=0.05, "*", "")))
 
  output[1:rows%%2!=0] <- paste0(round(lmrobust_object$coefficients[c("C1", "age", "age2", "lit", "male")],3), stars)
  output[1:rows%%2==0] <- paste0("(", round(lmrobust_object$std.error[2:5],3), ")") # names are not recognized..need to use index
 
 output <- c( output, fe, lmrobust_object$N, round(lmrobust_object$r.squared , 3))
 
}


## regressions without fixed effects 
no_fe <- lapply(y, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ C1 + age + age2 + lit + male")), data = hh ))

## regressions with fixed effects 
fe <- lapply(y, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ C1 + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))

## Create Correlations table 
corrs <- cbind (sapply(no_fe, stargazer.lmrobust, fe = "N"), sapply(fe, stargazer.lmrobust))
rownames(corrs) <- c("Desired role of chief", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")

kable(corrs, col.names = rep(c("Resp. State" , "Resp. citizen"  , "Valuation State", "Citizen Activities","Index" ),2))
```

                        Resp. State   Resp. citizen   Valuation State   Citizen Activities   Index      Resp. State   Resp. citizen   Valuation State   Citizen Activities   Index     
----------------------  ------------  --------------  ----------------  -------------------  ---------  ------------  --------------  ----------------  -------------------  ----------
Desired role of chief   -0.104***     -0.067***       -0.053***         -0.015**             -0.06***   -0.076***     -0.055***       -0.037***         -0.008               -0.045*** 
                        (0.013)       (0.012)         (0.013)           (0.006)              (0.007)    (0.013)       (0.012)         (0.013)           (0.006)              (0.007)   
Age                     0.013         0.034*          0.015             0.018**              0.019*     0.035*        0.04**          0.03*             0.027***             0.032***  
                        (0.019)       (0.018)         (0.017)           (0.008)              (0.01)     (0.019)       (0.018)         (0.017)           (0.008)              (0.01)    
Age2                    -0.001        -0.003*         -0.002            -0.002*              -0.002     -0.003        -0.004*         -0.003*           -0.003***            -0.003*** 
                        (0.002)       (0.002)         (0.002)           (0.001)              (0.001)    (0.002)       (0.002)         (0.002)           (0.001)              (0.001)   
Literate                0.029**       0.028**         0.026**           0.014***             0.027***   0.026**       0.027**         0.024**           0.015***             0.025***  
                        (0.012)       (0.011)         (0.011)           (0.005)              (0.006)    (0.012)       (0.011)         (0.011)           (0.005)              (0.006)   
Male                    0.04***       0.056***        -0.006            0.016***             0.027***   0.045***      0.057***        -0.005            0.016***             0.028***  
                        (0.013)       (0.012)         (0.013)           (0.006)              (0.007)    (0.013)       (0.012)         (0.013)           (0.006)              (0.007)   
FE                      N             N               N                 N                    N          Y             Y               Y                 Y                    Y         
N                       3508          3507            3509              3507                 3528       3508          3507            3509              3507                 3528      
R2                      0.029         0.027           0.007             0.012                0.039      0.078         0.062           0.053             0.044                0.087     

# Table A3-A14. Relationship Modern and Traditional Contracts (individual level)

Script to produce tables for relationship modern and traditional contracts using individual level measures. These analyses make use of chiefdom fixed effects.



## Results for Responsibilities of the State


```r
Names <- c("Accept elections results" , "React to complaints"  , "React to suggestions", "Avoid corruption","Keep people informed", "Consult population", "Contribute resources" )

# Chief: Beneficiary Choice (QD2)
fe <- lapply(y_G1, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ qd2_chief + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))
corrs <- sapply(fe, stargazer.lmrobust_qd2, fe = "Y")
rownames(corrs) <- c("Chief: Beneficiary Choice", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")
corrs<-corrs[c(1:2,11:13),]
kable(corrs, col.names = Names)
```

                            Accept elections results   React to complaints   React to suggestions   Avoid corruption   Keep people informed   Consult population   Contribute resources 
--------------------------  -------------------------  --------------------  ---------------------  -----------------  ---------------------  -------------------  ---------------------
Chief: Beneficiary Choice   -0.02*                     -0.03**               -0.05***               -0.08***           -0.03*                 -0.04**              0                    
                            (0.01)                     (0.02)                (0.02)                 (0.02)             (0.01)                 (0.02)               (0.02)               
FE                          Y                          Y                     Y                      Y                  Y                      Y                    Y                    
N                           3508                       3508                  3508                   3508               3508                   3508                 3508                 
R2                          0.05                       0.06                  0.08                   0.05               0.05                   0.04                 0.05                 

```r
# Chief: Fund Allocation (QD6)
fe <- lapply(y_G1, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ qd6_chief + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))
corrs <- sapply(fe, stargazer.lmrobust_qd6, fe = "Y")
rownames(corrs) <- c("Chief: Fund Allocation", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")
corrs<-corrs[c(1:2,11:13),]
kable(corrs, col.names = Names)
```

                         Accept elections results   React to complaints   React to suggestions   Avoid corruption   Keep people informed   Consult population   Contribute resources 
-----------------------  -------------------------  --------------------  ---------------------  -----------------  ---------------------  -------------------  ---------------------
Chief: Fund Allocation   -0.05***                   -0.08***              -0.07***               -0.1***            -0.06***               -0.05***             0                    
                         (0.01)                     (0.02)                (0.02)                 (0.02)             (0.01)                 (0.02)               (0.02)               
FE                       Y                          Y                     Y                      Y                  Y                      Y                    Y                    
N                        3508                       3508                  3508                   3508               3508                   3508                 3508                 
R2                       0.06                       0.06                  0.08                   0.06               0.05                   0.04                 0.05                 

```r
# Chief: Project Supervision (QD7)
fe <- lapply(y_G1, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ qd7_chief + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))
corrs <- sapply(fe, stargazer.lmrobust_qd7, fe = "Y")
rownames(corrs) <- c("Chief: Project Supervision", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")
corrs<-corrs[c(1:2,11:13),]
kable(corrs, col.names = Names)
```

                             Accept elections results   React to complaints   React to suggestions   Avoid corruption   Keep people informed   Consult population   Contribute resources 
---------------------------  -------------------------  --------------------  ---------------------  -----------------  ---------------------  -------------------  ---------------------
Chief: Project Supervision   -0.04**                    -0.05***              -0.07***               -0.08***           -0.07***               0                    0                    
                             (0.01)                     (0.02)                (0.02)                 (0.02)             (0.02)                 (0.02)               (0.02)               
FE                           Y                          Y                     Y                      Y                  Y                      Y                    Y                    
N                            3508                       3508                  3508                   3508               3508                   3508                 3508                 
R2                           0.05                       0.06                  0.08                   0.05               0.05                   0.04                 0.05                 


## Results for Responsibilities of Citizens


```r
Names <- c("Vote","Complain", "Make suggestions","Attend meetings", "Obey the law","Pay tax", "Support government")

# Chief: Beneficiary Choice (QD2)
fe <- lapply(y_G2, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ qd2_chief + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))
corrs <- sapply(fe, stargazer.lmrobust_qd2, fe = "Y")
rownames(corrs) <- c("Chief: Beneficiary Choice", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")
corrs<-corrs[c(1:2,11:13),]
kable(corrs, col.names = Names)
```

                            Vote       Complain   Make suggestions   Attend meetings   Obey the law   Pay tax   Support government 
--------------------------  ---------  ---------  -----------------  ----------------  -------------  --------  -------------------
Chief: Beneficiary Choice   -0.05***   -0.02      -0.04***           -0.04***          0              -0.01     -0.02              
                            (0.02)     (0.02)     (0.01)             (0.01)            (0.02)         (0.02)    (0.01)             
FE                          Y          Y          Y                  Y                 Y              Y         Y                  
N                           3507       3507       3507               3507              3507           3507      3507               
R2                          0.05       0.03       0.03               0.04              0.06           0.05      0.04               

```r
# Chief: Fund Allocation (QD6)
fe <- lapply(y_G2, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ qd6_chief + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))
corrs <- sapply(fe, stargazer.lmrobust_qd6, fe = "Y")
rownames(corrs) <- c("Chief: Fund Allocation", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")
corrs<-corrs[c(1:2,11:13),]
kable(corrs, col.names = Names)
```

                         Vote       Complain   Make suggestions   Attend meetings   Obey the law   Pay tax   Support government 
-----------------------  ---------  ---------  -----------------  ----------------  -------------  --------  -------------------
Chief: Fund Allocation   -0.07***   -0.06***   -0.05***           -0.05***          -0.02          -0.01     0                  
                         (0.02)     (0.02)     (0.01)             (0.01)            (0.02)         (0.02)    (0.01)             
FE                       Y          Y          Y                  Y                 Y              Y         Y                  
N                        3507       3507       3507               3507              3507           3507      3507               
R2                       0.05       0.03       0.03               0.04              0.06           0.05      0.04               

```r
# Chief: Project Supervision (QD7)
fe <- lapply(y_G2, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ qd7_chief + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))
corrs <- sapply(fe, stargazer.lmrobust_qd7, fe = "Y")
rownames(corrs) <- c("Chief: Project Supervision", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")
corrs<-corrs[c(1:2,11:13),]
kable(corrs, col.names = Names)
```

                             Vote       Complain   Make suggestions   Attend meetings   Obey the law   Pay tax   Support government 
---------------------------  ---------  ---------  -----------------  ----------------  -------------  --------  -------------------
Chief: Project Supervision   -0.06***   -0.04**    -0.04***           -0.04***          -0.04**        -0.03**   0                  
                             (0.02)     (0.02)     (0.01)             (0.01)            (0.02)         (0.02)    (0.01)             
FE                           Y          Y          Y                  Y                 Y              Y         Y                  
N                            3507       3507       3507               3507              3507           3507      3507               
R2                           0.05       0.03       0.03               0.04              0.06           0.05      0.04               

## Results for Valuation of the State


```r
Names <- c("Providing education" , "Providing health care")

# Chief: Beneficiary Choice (QD2)
fe <- lapply(y_G3, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ qd2_chief + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))
corrs <- sapply(fe, stargazer.lmrobust_qd2, fe = "Y")
rownames(corrs) <- c("Chief: Beneficiary Choice", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")
corrs<-corrs[c(1:2,11:13),]
kable(corrs, col.names = Names)
```

                            Providing education   Providing health care 
--------------------------  --------------------  ----------------------
Chief: Beneficiary Choice   -0.02                 -0.03**               
                            (0.01)                (0.01)                
FE                          Y                     Y                     
N                           3477                  3465                  
R2                          0.05                  0.04                  

```r
# Chief: Fund Allocation (QD6)
fe <- lapply(y_G3, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ qd6_chief + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))
corrs <- sapply(fe, stargazer.lmrobust_qd6, fe = "Y")
rownames(corrs) <- c("Chief: Fund Allocation", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")
corrs<-corrs[c(1:2,11:13),]
kable(corrs, col.names = Names)
```

                         Providing education   Providing health care 
-----------------------  --------------------  ----------------------
Chief: Fund Allocation   -0.01                 -0.01                 
                         (0.01)                (0.01)                
FE                       Y                     Y                     
N                        3477                  3465                  
R2                       0.05                  0.04                  

```r
# Chief: Project Supervision (QD7)
fe <- lapply(y_G3, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ qd7_chief + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))
corrs <- sapply(fe, stargazer.lmrobust_qd7, fe = "Y")
rownames(corrs) <- c("Chief: Project Supervision", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")
corrs<-corrs[c(1:2,11:13),]
kable(corrs, col.names = Names)
```

                             Providing education   Providing health care 
---------------------------  --------------------  ----------------------
Chief: Project Supervision   -0.04***              -0.02*                
                             (0.01)                (0.01)                
FE                           Y                     Y                     
N                            3477                  3465                  
R2                           0.05                  0.04                  

## Results for Citizen Activities


```r
Names <- c("Contact police" , "Visit to government agency")

# Chief: Beneficiary Choice (QD2)
fe <- lapply(y_G4, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ qd2_chief + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))
corrs <- sapply(fe, stargazer.lmrobust_qd2, fe = "Y")
rownames(corrs) <- c("Chief: Beneficiary Choice", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")
corrs<-corrs[c(1:2,11:13),]
kable(corrs, col.names = Names)
```

                            Contact police   Visit to government agency 
--------------------------  ---------------  ---------------------------
Chief: Beneficiary Choice   0                -0.01                      
                            (0.01)           (0.01)                     
FE                          Y                Y                          
N                           3495             3506                       
R2                          0.04             0.03                       

```r
# Chief: Fund Allocation (QD6)
fe <- lapply(y_G4, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ qd6_chief + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))
corrs <- sapply(fe, stargazer.lmrobust_qd6, fe = "Y")
rownames(corrs) <- c("Chief: Fund Allocation", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")
corrs<-corrs[c(1:2,11:13),]
kable(corrs, col.names = Names)
```

                         Contact police   Visit to government agency 
-----------------------  ---------------  ---------------------------
Chief: Fund Allocation   0                -0.01                      
                         (0.01)           (0.01)                     
FE                       Y                Y                          
N                        3495             3506                       
R2                       0.04             0.03                       

```r
# Chief: Project Supervision (QD7)
fe <- lapply(y_G4, function(y)
  estimatr::lm_robust(formula = as.formula(paste0(y, "~ qd7_chief + age + age2+lit + male + as.factor(IDS_CHEF)")), data = hh ))
corrs <- sapply(fe, stargazer.lmrobust_qd7, fe = "Y")
rownames(corrs) <- c("Chief: Project Supervision", "", "Age", "","Age2", "","Literate", "","Male","", "FE", "N", "R2")
corrs<-corrs[c(1:2,11:13),]
kable(corrs, col.names = Names)
```

                             Contact police   Visit to government agency 
---------------------------  ---------------  ---------------------------
Chief: Project Supervision   0                -0.01                      
                             (0.01)           (0.01)                     
FE                           Y                Y                          
N                            3495             3506                       
R2                           0.04             0.03                       

# Table 5. Structural Model Parameter Estimates

The table below presents the results from estimating equations (4) and (5) in the manuscript. First, data is prepared:


```r
#write.csv(hh, file = "data_temp/hh.csv")
stan_hh <- dplyr::filter(hh, complete.cases(IDS_CHEF, IDV, rain, confl, C1, G ))

d    <- length(unique(stan_hh$IDS_CHEF))
k    <- length(unique(stan_hh$IDV))


stan_hh  %<>% mutate(chiefdom = as.numeric(factor(IDS_CHEF, labels = 1:d)), 
                      village  = as.numeric(factor(IDV, labels = 1:k) ))
            
idv_chef <- dplyr::group_by( stan_hh,  village, chiefdom )%>%
             dplyr::summarise(n())


x      <- dplyr::select(stan_hh, C1,  G)

n      <- nrow(x)

sdata  <- list(n = n, 
               k = k, 
               d = d, 
               x = x ,
               rain  = stan_hh$rain, 
               confl = stan_hh$confl, 
               chiefdom = stan_hh$chiefdom , 
               idv = stan_hh$village) # to be passed on to Stan
```

Next, run Stan model with random effects at the chiefdom level:


```r
## Random effects at chiefdom level 

model_1re <- "
// Pearson Correlation
data { 
int<lower=0> n;
int<lower=0> k;
int<lower=0> d;
vector[2] x[n];
vector[n] rain;
vector[n] confl;
int chiefdom[n];
int idv[n];


}
parameters {
real<lower=0.1>        CES;
vector<lower=0.001>[2]     sigma;
real<lower=-.999,upper=.999> r;
real  a_g;
real  b_g;
real  g_g;
real  a_c;
real  b_c;
real  g_c;

real <lower=0.001> sigma_fe_g;

vector[d] fe_g;


real <lower=0.001> sigma_fe_c;

vector[d] fe_c;



} 

transformed parameters {

cov_matrix[2] Sigma;

// Reparameterization
Sigma[1,1] = square(sigma[1]);
Sigma[1,2] = r * sigma[1] * sigma[2];
Sigma[2,1] = r * sigma[1] * sigma[2];
Sigma[2,2] = square(sigma[2]);

}

model {
vector[2]  s[n];
vector[n]  v_g;
vector[n]  v_c;

v_c =  exp(a_c + b_c*rain + g_c*confl);
v_g =  exp(a_g + b_g*rain + g_g*confl);

for (j in 1:n) {
s[j,1] = (v_c[j]^(CES))/(v_c[j]^(CES-1) + v_g[j]^(CES-1)) +   fe_c[chiefdom[j]] ; 
s[j,2] = (v_g[j]^(CES))/(v_c[j]^(CES-1) + v_g[j]^(CES-1)) +   fe_g[chiefdom[j]] ;
} ;



  // Chiefdom level
    fe_g ~ normal(0, sigma_fe_g);
    fe_c ~ normal(0, sigma_fe_c);
    
  // Data
   x ~ multi_normal(s,   Sigma);

}
"
```

```r
if(run_1re){
samples <- stan(model_code = model_1re,   
                # file = "model_1re.stan",
                data    = sdata, 
                iter    = iters, 
                chains  = 4, 
                thin    = 2,
                refresh = 10, 
                control = list(adapt_delta = 0.99))
save(samples, file = outname_1re)}
```

Next, run Stan model with random effects at the chiefdom level and the village level:


```r
# Random effects at chiefdom and village level

model_2re <- "
// Pearson Correlation
data { 
int<lower=0> n;
int<lower=0> k;
int<lower=0> d;
vector[2] x[n];
vector[n] rain;
vector[n] confl;
int chiefdom[n];
int idv[n];

}
parameters {
real<lower=0.1>        CES;
vector<lower=0.001>[2]     sigma;
real<lower=-.999,upper=.999> r;
real  a_g;
real  b_g;
real  g_g;
real  a_c;
real  b_c;
real  g_c;

real <lower=0.001> sigma_fe_g;
real <lower=0.001> sigma_re_g;
vector[d] fe_g;
vector[k] re_g;

real <lower=0.001> sigma_fe_c;
real <lower=0.001> sigma_re_c;
vector[d] fe_c;
vector[k] re_c;



} 

transformed parameters {

cov_matrix[2] Sigma;

// Reparameterization
Sigma[1,1] = square(sigma[1]);
Sigma[1,2] = r * sigma[1] * sigma[2];
Sigma[2,1] = r * sigma[1] * sigma[2];
Sigma[2,2] = square(sigma[2]);

}

model {
vector[2]  s[n];
vector[n]  v_g;
vector[n]  v_c;

v_c =  exp(a_c + b_c*rain  + g_c*confl);
v_g =  exp(a_g + b_g*rain  + g_g*confl);

for (j in 1:n) {
s[j,1] = (v_c[j]^(CES))/(v_c[j]^(CES-1) + v_g[j]^(CES-1)) +   fe_c[chiefdom[j]] +  re_c[idv[j]]  ; 
s[j,2] = (v_g[j]^(CES))/(v_c[j]^(CES-1) + v_g[j]^(CES-1)) +   fe_g[chiefdom[j]]  + re_g[idv[j]] ;
} ;

//  Model
  // IDV RE level

    re_c  ~  normal(0, sigma_re_c);
    re_g  ~  normal(0, sigma_re_g);
  
    
  // Chiefdom level
    fe_g ~ normal(0, sigma_fe_g);
    fe_c ~ normal(0, sigma_fe_c);
    
  // Data
   x ~ multi_normal(s, Sigma);

}
"

if(run_2re){
samples <- stan(model_code=model_2re,   
                data    = sdata, 
                iter    = iters, 
                chains  = 4, 
                thin    = 2,
                refresh = 10, 
                control = list(adapt_delta = 0.99))
save(samples, file = outname_2re)}
```



We run the structural model using Stan with $n =$ ``20`` Markov chains and only select those chains that had less than ``0`` divergent iterations. Lastly, we compute estimates using draws from the posterior distribution that were extracted from the selected chains.


```r
# Model 1 FE
load(outname_1re) 
divergent <-sapply(1:chains, function(i)
	get_sampler_params(samples, inc_warmup=FALSE)[[i]][,'divergent__'])

# Divergences by chain
chains_1re <- which( colSums(divergent) <= max_divergent)


# Model 2 FE
load(outname_2re) 
divergent <-sapply(1:chains, function(i)
	get_sampler_params(samples, inc_warmup=FALSE)[[i]][,'divergent__'])


# Divergences by chain
chains_2re <- which( colSums(divergent) <= max_divergent)
```











```r
kable(tab5, col.names = c("Parameter", "Definition", "(i)", "(ii)"), row.names = FALSE, caption = "\\label{table:params} Structural model parameter estimates. Columns (i) and (ii) show, repectively, parameter estimates for model with random effects at the chiefdom level and for a model with random effects at the chiefdom and village level. Credibility intervals in parentheses.", align = c("clccccc"))
```



Table: \label{table:params} Structural model parameter estimates. Columns (i) and (ii) show, repectively, parameter estimates for model with random effects at the chiefdom level and for a model with random effects at the chiefdom and village level. Credibility intervals in parentheses.

    Parameter       Definition                      (i)                 (ii)       
------------------  -----------------------  ------------------  ------------------
     $\sigma$       CES parameter                  0.205               0.209       
                                               (0.101, 0.705)      (0.101, 0.69)   
 \hline$\sigma_c$   Variance on $g$                0.139               0.126       
                                               (0.133, 0.146)      (0.12, 0.133)   
    $\sigma_g$      Variance on $c$                 0.03               0.023       
                                               (0.029, 0.031)      (0.022, 0.024)  
  $\sigma_{gc}$     Covariance on $g,c$            -0.008              -0.008      
                                              (-0.01, -0.006)     (-0.01, -0.007)  
 \hline$\alpha_g$   State intercept                -1.283              -1.386      
                                              (-1.508, -0.784)    (-1.612, -0.891) 
    $\beta_g$       Rain shock (State)             -0.07               -0.029      
                                              (-0.137, -0.014)    (-0.097, 0.034)  
    $\gamma_g$      Conflict shock (State)         -0.133              -0.136      
                                              (-0.249, -0.033)    (-0.294, 0.002)  
 \hline$\alpha_c$   Chief intercept                3.621               3.968       
                                               (0.25, 6.922)       (0.261, 7.566)  
    $\beta_c$       Rain shock (Chief)             0.076               -0.123      
                                              (-0.316, 0.588)     (-0.555, 0.313)  
    $\gamma_c$      Conflict shock (Chief)         0.874               0.755       
                                               (0.042, 2.017)     (-0.028, 1.924)  

# Figure A1: Mediation Effects of Valuations on Support for the Rain Shock


```r
n         <- nrow(stan_hh )
n_sims    <- 1000
out <- posteriors1_re


Sigma3    <- dplyr::select(as.data.frame(out),  Sigma.1.1 = Sigma.1.1,
                                                Sigma.2.2 = Sigma.2.2,
                                                Sigma.1.2 = Sigma.1.2) %>% as.matrix()

Rain_low  <- cbind(1, quantile(stan_hh$rain, probs = 0.025),stan_hh $confl )
Rain_high <- cbind(1, quantile(stan_hh$rain, probs = 0.975), stan_hh $confl)
Conflict_low  <- cbind(1, stan_hh $rain, quantile(stan_hh $confl, probs = 0.025))
Conflict_high <- cbind(1, stan_hh $rain, quantile(stan_hh $confl, probs = 0.0975))
```


```r
# Letting changes in the rain shock from low to high only affect v_C while holding v_G fixed at low levels of rain

gen_posteriors <- function(n_sims,   low, high){
  
  v_given_shock <- function(s, shock, authority ){
  if(authority == "c") coeff <-  c(out$a_c[s], out$b_c[s], out$g_c[s])
  if(authority == "g") coeff <-  c(out$a_g[s], out$b_g[s], out$g_g[s])
  exp(shock%*%coeff)
}

out$v_c_low  <- sapply(1:n_sims, v_given_shock, shock = low, authority = "c")
out$v_c_high <- sapply(1:n_sims, v_given_shock, shock = high, authority = "c")
out$v_g_low  <- sapply(1:n_sims, v_given_shock, shock = low, authority = "g")
out$v_g_high <- sapply(1:n_sims, v_given_shock, shock = high, authority = "g")



s_G_low_vc   <- with( out, sapply(1:n_sims, function(s){
              
              (v_g_low[,s]^(CES[s]))/(v_c_low[,s]^(CES[s]-1) + v_g_low[,s]^(CES[s]-1)) + rnorm(n, mean = 0, sd = sigma_fe_g[s])   }))

s_G_high_vc   <- with( out, sapply(1:n_sims, function(s){
              (v_g_low[,s]^(CES[s]))/(v_c_high[,s]^(CES[s]-1) + v_g_low[,s]^(CES[s]-1)) + rnorm(n, mean = 0, sd = sigma_fe_g[s]) }))

s_C_low_vc   <- with( out, sapply(1:n_sims, function(s){
              (v_c_low[,s]^(CES[s]))/(v_c_low[,s]^(CES[s]-1) + v_g_low[,s]^(CES[s]-1)) + rnorm(n, mean = 0, sd = sigma_fe_g[s]) }))

s_C_high_vc   <- with( out, sapply(1:n_sims, function(s){
               (v_c_high[,s]^(CES[s]))/(v_c_high[,s]^(CES[s]-1) + v_g_low[,s]^(CES[s]-1)) + rnorm(n, mean = 0, sd = sigma_fe_g[s]) }))


s_G_low_vg   <- with( out, sapply(1:n_sims, function(s){
              (v_g_low[,s]^(CES[s]))/(v_c_low[,s]^(CES[s]-1) + v_g_low[,s]^(CES[s]-1)) + rnorm(n, mean = 0, sd = sigma_fe_g[s]) }))

s_G_high_vg   <- with( out, sapply(1:n_sims, function(s){
              (v_g_high[,s]^(CES[s]))/(v_c_low[,s]^(CES[s]-1) + v_g_high[,s]^(CES[s]-1)) + rnorm(n, mean = 0, sd = sigma_fe_g[s]) }))

s_C_low_vg   <- with( out, sapply(1:n_sims, function(s){
              (v_c_low[,s]^(CES[s]))/(v_c_low[,s]^(CES[s]-1) + v_g_low[,s]^(CES[s]-1)) + rnorm(n, mean = 0, sd = sigma_fe_g[s]) }))

s_C_high_vg   <- with( out, sapply(1:n_sims, function(s){
               (v_c_low[,s]^(CES[s]))/(v_c_low[,s]^(CES[s]-1) + v_g_high[,s]^(CES[s]-1)) + rnorm(n, mean = 0, sd = sigma_fe_g[s]) }))

simx_low_vc   <- lapply(1:n_sims, function(s){
          vcv <-  cbind(rbind(Sigma3[s, 1],Sigma3[s,3]) , rbind(Sigma3[s,3],Sigma3[s,2] ))
          rx <- sapply(1:n, function(i){
          mvrnorm(1, cbind(s_C_low_vc[i,s], s_G_low_vc[i, s]), vcv ) })
          t(rx)  })

simx_high_vc   <- lapply(1:n_sims, function(s){
          vcv <-  cbind(rbind(Sigma3[s, 1],Sigma3[s,3]) , rbind(Sigma3[s,3],Sigma3[s,2] ))
          rx <- sapply(1:n, function(i){
          mvrnorm(1, cbind(s_C_high_vc[i,s], s_G_high_vc[i, s]), vcv ) })
          t(rx)  })

simx_low_vg   <- lapply(1:n_sims, function(s){
          vcv <-  cbind(rbind(Sigma3[s, 1],Sigma3[s,3]) , rbind(Sigma3[s,3],Sigma3[s,2] ))
          rx <- sapply(1:n, function(i){
          mvrnorm(1, cbind(s_C_low_vg[i,s], s_G_low_vg[i, s]), vcv ) })
          t(rx)  })

simx_high_vg   <- lapply(1:n_sims, function(s){
          vcv <-  cbind(rbind(Sigma3[s, 1],Sigma3[s,3]) , rbind(Sigma3[s,3],Sigma3[s,2] ))
          rx <- sapply(1:n, function(i){
          mvrnorm(1, cbind(s_C_high_vg[i,s], s_G_high_vg[i, s]), vcv ) })
          t(rx)  })

# Output simulations ALL
s_C_low_vc   <- t(sapply(1:n_sims, function(i) simx_low_vc[[i]][,1]))
s_G_low_vc   <- t(sapply(1:n_sims, function(i) simx_low_vc[[i]][,2]))
s_C_high_vc  <- t(sapply(1:n_sims, function(i) simx_high_vc[[i]][,1]))
s_G_high_vc  <- t(sapply(1:n_sims, function(i) simx_high_vc[[i]][,2]))

s_C_low_vg   <- t(sapply(1:n_sims, function(i) simx_low_vg[[i]][,1]))
s_G_low_vg   <- t(sapply(1:n_sims, function(i) simx_low_vg[[i]][,2]))
s_C_high_vg  <- t(sapply(1:n_sims, function(i) simx_high_vg[[i]][,1]))
s_G_high_vg  <- t(sapply(1:n_sims, function(i) simx_high_vg[[i]][,2]))

# Creates long dataframe nrow = 1000+ 1000; first 1000 rows: posteriors given low levels of shocks; rows 1001:2000 posterior hihg levels of shocks

               data.frame(v_c           = c(colMeans(out$v_c_low),
                                          colMeans(out$v_c_high)), 
                        v_g           = c(colMeans(out$v_g_low),
                                          colMeans(out$v_g_high)), 
                        sG_given_vc  = c(rowMeans(s_G_low_vc),
                                          rowMeans((s_G_high_vc))),
                        sG_given_vg  = c(rowMeans(s_G_low_vg),
                                          rowMeans((s_G_high_vg))),
                        sC_given_vc  = c(rowMeans(s_C_low_vc),
                                          rowMeans((s_C_high_vc))),
                        sC_given_vg  = c(rowMeans(s_C_low_vg),
                                          rowMeans((s_C_high_vg))),
                        level = c(rep("low", n_sims), rep("high", n_sims)))

}
```




```r
set.seed(1)

if(run_posteriors){ 
rain_med_DF  <- gen_posteriors(n_sims, low = Rain_low,     high = Rain_high)    
confl_med_DF <- gen_posteriors(n_sims, low = Conflict_low, high = Conflict_high) 

save(rain_med_DF,  file = "posterior_rain.Rdata")
save(confl_med_DF, file = "posterior_confl.Rdata")
}


load("posterior_rain.Rdata")
load("posterior_confl.Rdata")
                  
direct_rain_sg <- ggplot(data = rain_med_DF, aes(x = v_g, y =  sG_given_vg )) +
  xlab(expression('Valuation'[g])) +
  ylab(expression('Support'[g])) +
  ggtitle("Within effects" , subtitle = NULL) +
  ylim(0.1, 0.7) +
  geom_point(aes(color = level), alpha = 0.5) +
  scale_color_manual(values = c("grey3", "grey69"))+
  theme(legend.position="none")
   

indirect_rain_sg <- ggplot(data = rain_med_DF) +
  geom_point(aes(x = v_c, y =  sG_given_vc, color = level), alpha = 0.5) +
  ylim(0.1, 0.7) +
  xlab(expression('Valuation'[c]))  +
  ggtitle("Across effects", subtitle = NULL) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_color_manual(values = c("grey3", "grey69"))   +
  theme(panel.background=element_rect(fill = "white")) +
  theme(legend.position="none")


direct_rain_sc <- ggplot(data = rain_med_DF , aes(x = v_c, y =  sC_given_vc )) +
  xlab(expression('Valuation'[c])) +
  ylab(expression('Support'[c])) +
  ylim(0.1, 1.2) +
  geom_point(aes(color = level),  alpha = 0.5) +
  scale_color_manual(values = c("grey3", "grey69"))+
  theme(legend.position="none")
   

indirect_rain_sc <- ggplot(data = rain_med_DF , aes(x = v_g, y =  sC_given_vg )) +
   xlab(expression('Valuation'[g]))  +
   theme(axis.title.y=element_blank(),
   axis.text.y=element_blank(),
   axis.ticks.y=element_blank()) +
   ylim(0.1, 1.2) +
   geom_point(aes(color = level),  alpha = 0.5)+
   scale_color_manual(values = c("grey3", "grey69"))   +
   theme(legend.position="none")

f1 <- plot_grid(direct_rain_sg, indirect_rain_sg, direct_rain_sc, indirect_rain_sc, rel_widths=c(1,0.9))
```

```
## Warning: Removed 11 rows containing missing values (geom_point).
```

```r
png(filename = "mediation1.png")
f1
dev.off()
```

```
## png 
##   2
```

```r
f1
```

![](Replication_files/figure-html/figs-1.png)<!-- -->

```r
  png("output/FigA1_Rain.png")
  f1
  dev.off()
```

```
## png 
##   2
```


## Table 6. The Impact of Complementarity: Within and Across Effects (Rain)


```r
# Confidence Intervals Rain
low <- rain_med_DF[rain_med_DF$level == "low", c("v_c", "v_g", "sG_given_vg", "sG_given_vc", "sC_given_vc", "sC_given_vg" )]

high <- rain_med_DF[rain_med_DF$level == "high",c("v_c", "v_g", "sG_given_vg", "sG_given_vc", "sC_given_vc", "sC_given_vg" ) ]

# low
apply(low, 2, quantile, probs = c(0.025, 0.5, 0.975))
```

```
##               v_c       v_g sG_given_vg sG_given_vc sC_given_vc
## 2.5%     1.042357 0.2145054   0.2096376   0.2102068   0.4569728
## 50%     59.116714 0.2468122   0.2385931   0.2376566   0.5137502
## 97.5% 1204.983267 0.4493158   0.2707590   0.2696756   0.5643755
##       sC_given_vg
## 2.5%    0.4552045
## 50%     0.5149792
## 97.5%   0.5656201
```

```r
# high
apply(high, 2, quantile, probs = c(0.025, 0.5, 0.975))
```

```
##                v_c       v_g sG_given_vg sG_given_vc sC_given_vc
## 2.5%     0.7539725 0.1142419   0.1089424   0.2033997   0.3434441
## 50%     77.2719605 0.1619342   0.1563478   0.2382617   0.5337331
## 97.5% 7348.8590792 0.2947581   0.2041866   0.2796816   0.8159559
##       sC_given_vg
## 2.5%    0.2497117
## 50%     0.3645722
## 97.5%   0.5288197
```

```r
ACME <-   high - low

# Difference in mean CI
kable(apply(ACME, 2, quantile, probs = c(0.025, 0.5, 0.975)), digits = 4)
```

               v_c       v_g   sG_given_vg   sG_given_vc   sC_given_vc   sC_given_vg
------  ----------  --------  ------------  ------------  ------------  ------------
2.5%     -788.3894   -0.1881       -0.1460       -0.0180       -0.1878       -0.2565
50%         2.7755   -0.0892       -0.0833        0.0005        0.0190       -0.1462
97.5%    7164.1305   -0.0174       -0.0173        0.0202        0.3288        0.0078



# Figure A2: Mediation Effects of Valuations on Support for the Conflict Shock



```r
png(filename = "mediation2.png")

direct_conflict_sg <- ggplot(data = confl_med_DF , aes(x = v_g, y =  sG_given_vg )) +
  xlab(expression('Valuation'[g])) +
  ylab(expression('Support'[g])) +
  ggtitle("Within effects" , subtitle = NULL) +
  geom_point(aes(color = level),  alpha = 0.5) +
  scale_color_manual(values = c("grey3", "grey69"))+
  theme(legend.position="none")
   
indirect_conflict_sg <- ggplot(data = confl_med_DF , aes(x = v_c, y =  sG_given_vc )) +
 xlab(expression('Valuation'[c]))  +
 theme(axis.title.y=element_blank(),
 axis.text.y=element_blank(),
 axis.ticks.y=element_blank()) +
 ggtitle("Across effects", subtitle = NULL) +
 geom_point(aes(color = level),  alpha = 0.5)+
 scale_color_manual(values = c("grey3", "grey69"))   +
 theme(legend.position="none")

direct_conflict_sc <- ggplot(data = confl_med_DF , aes(x = v_c, y =  sC_given_vc )) +
  xlab(expression('Valuation'[c])) +
  ylab((expression('Support'[c])) ) +
  geom_point(aes(color = level),  alpha = 0.5) +
  scale_color_manual(values = c("grey3", "grey69"))+
  theme(legend.position="none")
   
indirect_conflict_sc <- ggplot(data = confl_med_DF , aes(x = v_g, y =  sC_given_vg )) +
  xlab((expression('Valuation'[g])) )  +
  theme(axis.title.y=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks.y=element_blank()) +
  ylim(0.2, 0.8) +
  geom_point(aes(color = level),  alpha = 0.5)+
  scale_color_manual(values = c("grey3", "grey69"))   +
  theme(legend.position="none")

# Change the line type
f2 <- cowplot::plot_grid(direct_conflict_sg, indirect_conflict_sg, direct_conflict_sc, indirect_conflict_sc ,rel_widths=c(1,0.9) )

f2
dev.off()
```

```
## png 
##   2
```

```r
f2
```

![](Replication_files/figure-html/confl_fig-1.png)<!-- -->

```r
  png("output/FigA2_Conflict.png")
  f2
  dev.off()
```

```
## png 
##   2
```

## Table A15. The Impact of Complementarity: Within and Across Effects (Conflict)


```r
# Confidence Intervals Conflict

l <- confl_med_DF[confl_med_DF$level=="low",  c("v_c", "v_g", "sG_given_vg", "sG_given_vc", "sC_given_vc", "sC_given_vg" )]   
h <- confl_med_DF[confl_med_DF$level=="high", c("v_c", "v_g", "sG_given_vg", "sG_given_vc", "sC_given_vc", "sC_given_vg")]

# High
round(apply(h, 2, quantile, probs = c(0.025, 0.5, 0.975)), 4)
```

```
##             v_c    v_g sG_given_vg sG_given_vc sC_given_vc sC_given_vg
## 2.5%     0.9820 0.1863      0.1790      0.2015      0.4480      0.3452
## 50%     77.2742 0.2106      0.2015      0.2323      0.5118      0.4008
## 97.5% 1600.1261 0.3796      0.2224      0.2642      0.5739      0.4695
```

```r
# Low
round(apply(l, 2, quantile, probs = c(0.025, 0.5, 0.975)), 4)
```

```
##            v_c    v_g sG_given_vg sG_given_vc sC_given_vc sC_given_vg
## 2.5%    0.9013 0.2052      0.2002      0.1998      0.3905      0.3900
## 50%    28.5535 0.2394      0.2287      0.2284      0.4429      0.4438
## 97.5% 500.9617 0.4292      0.2557      0.2560      0.4959      0.4965
```

```r
kable((apply(h - l, 2, quantile, probs = c(0.025, 0.5, 0.975))), digits = 4)
```

               v_c       v_g   sG_given_vg   sG_given_vc   sC_given_vc   sC_given_vg
------  ----------  --------  ------------  ------------  ------------  ------------
2.5%        0.0228   -0.0717       -0.0507       -0.0062        0.0064       -0.0802
50%        43.6402   -0.0299       -0.0272        0.0033        0.0699       -0.0430
97.5%    1040.8681   -0.0079       -0.0051        0.0176        0.1274        0.0043



# Table A1. Additional Summary Information


```r
# Chief activities

chief_activities <-  c("cq057_a_managed_conflict", 
                       "cq058_a_conflict_same_locality", 
                       "cq059_a_conflict_different_local", 
                       "cq060_a_stealing", 
                       "cq061_a_legal_problems", 
                       "cq062_a_between_villagers", 
                       "cq063_a_social", 
                       "cq073_a_marriage", 
                       "cq074_a_other_activities")

gvars <- data.frame(family  = c("Chief Activities", rep("",length(chief_activities)-1)),
                    dep_var = c("Manage local conflict", 
                                "Manage conflict between villages", 
                                "Manage external conflict",
                                "Managed stealing problems", 
                                "Managed marriage problems", 
                                "Managed local violence",
                                "Managed social violence",
                                "Conduct wedding/ baptism/ etc",
                                "Other" ))

 gsumm_vill <- cbind(gvars, t(sapply(chief_activities, summ, my_data = hh )))

# Individual Characteristics
 
 
 gvars <- data.frame(family  = c("Individual characteristics", rep("",2)),
                    dep_var = c("Age (x10)", 
                                "Literacy", 
                                "Male" ))

 gsumm_ind <- cbind(gvars, t(sapply(c("age", "lit", "male"), summ )))

kable( rbind(gsumm_vill, gsumm_ind ), 
       row.names = FALSE, 
       col.names = c("Level", "Description", "Obs", "Mean", "St. Dev", "Min", "Max"),
       digits = 3)
```



Level                        Description                          Obs    Mean   St. Dev   Min    Max
---------------------------  ---------------------------------  -----  ------  --------  ----  -----
Chief Activities             Manage local conflict               3535   0.440     0.496   0.0    1.0
                             Manage conflict between villages    3550   0.218     0.413   0.0    1.0
                             Manage external conflict            3545   0.157     0.363   0.0    1.0
                             Managed stealing problems           3580   0.458     0.498   0.0    1.0
                             Managed marriage problems           3575   0.540     0.498   0.0    1.0
                             Managed local violence              3535   0.400     0.490   0.0    1.0
                             Managed social violence             3575   0.575     0.494   0.0    1.0
                             Conduct wedding/ baptism/ etc       3500   0.076     0.265   0.0    1.0
                             Other                               3480   0.353     0.478   0.0    1.0
Individual characteristics   Age (x10)                           3639   4.118     1.462   1.8   10.4
                             Literacy                            3713   0.575     0.494   0.0    1.0
                             Male                                3713   0.512     0.500   0.0    1.0

# Appendix C. Attrition and Missing Responses

We presents the number of targeted and actually visited villages. We consider a village as visited if at least one household survey was collected in that village or a chief survey.


```r
# Number of villages visited
# Use the variable answered for all households where data was collected (q11)
# Same for chief survey (cq7)

q11 <- aggregate(q011_sex ~ IDV, data = hh, function(x) {sum(!is.na(x))}, na.action = NULL)
colnames(q11) <- c("IDV","q11")
vill_data <- merge(vill_data, q11, by="IDV", all=TRUE)  
vill_data$StepD  <- (!(vill_data$cq007_date=="") | vill_data$q11>0)
villages_visited <- length(vill_data$StepD[vill_data$StepD==1])

vill_data$Province[vill_data$IDV < 50000] <-"SK"
vill_data$Province[vill_data$IDV < 40000] <-"TG"
vill_data$Province[vill_data$IDV < 30000] <-"MN"
vill_data$Province[vill_data$IDV < 20000] <-"HK"

villages_visited
```

```
## [1] 815
```

```r
table(vill_data$StepD, vill_data$Province)
```

```
##        
##          HK  MN  SK  TG
##   FALSE  10 259  11  25
##   TRUE  286  35 287 207
```

Number of households for which we collected data.


```r
# Number of households visited

length(hh$q011_sex[!is.na(hh$q011_sex) & hh$IDS_TYPES=="DML"])    
```

```
## [1] 0
```

Next, we regress village attrition on our rainfall and conflict shock variables. 


```r
# Create missing village variable
vill_data$vill_notvisited <- 1-vill_data$StepD
table(vill_data$vill_notvisited, vill_data$StepD)
```

```
##    
##     FALSE TRUE
##   0     0  815
##   1   305    0
```

Results for rainfall shock:

```r
res_rain <- estimatr::lm_robust(vill_notvisited ~ avg_rain_dev + as.factor(IDS_CHEF), data = vill_data)
summary(res_rain)$coefficients[2,1:3]
```

```
##    Estimate  Std. Error     t value 
## 0.005847093 0.008327279 0.702161257
```


Result for conflict shock:

```r
res_conflict <- estimatr::lm_robust(vill_notvisited ~ Confl_Shock + as.factor(IDS_CHEF), data = vill_data )
summary(res_conflict)$coefficients[2,1:3]
```

```
##    Estimate  Std. Error     t value 
## 0.011180280 0.006316709 1.769953447
```


This concludes the replication file.
