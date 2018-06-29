######################################################################################################
# This script takes the original dataset "cleveland.data" 
# from http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/cleveland.data, 
# then cleans up missing data and is used as input to synthpop syn to generate 60K 
# rows of anonymized synthetic data. This dataset is meant for use in Artificial Neural Network
# Classification Models for Heart Disease Prediction.
#
# By Al Sabay, 2018, SMU MS Datascience Graduate Research Studies
######################################################################################################

library(synthpop)

df_c <- read.csv("data/cleveland.csv", header = TRUE)

# exclude some columns with missing data (-9), dates, unused or not relevant
xclude_vars <- names(df_c) %in% c("id", "ccf", "painloc", "painexer", "relrest", "pncaden", 
                                  "smoke", "dm", "ekgmo", "ekgday", "ekgyr", "thaltime",
                                  "dummy", "rldv5", "restckm", "exerckm", "restef",
                                  "restwm", "exeref", "exerwm", "thalsev", "thalpul",
                                  "earlobe", "cmo", "cday", "cyr", "diag", "ramus", "om2", "lvx1", "lvx2",
                                  "lvx3", "lvx4", "lvf", "cathef", "junk", "name")

df_new <- df_c[!xclude_vars]

# recode -9 to NA so that R data management functions can be used
df_new[df_new == -9] <- NA

# which columns have NA?
apply(is.na(df_new), 2, sum)

# impute NA columns with median value
NA2median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
df_new[] <- lapply(df_new, NA2median)

# check again for NA
apply(is.na(df_new), 2, sum)

# recode diagnosis (diag) to 1 (from 1,2,3,4) if heart disease exists, or 0 if no heart disease exists.
# this is the dependent variable
df_new$num[df_new$num > 0] <- 1 


# synthesisze a data set of 60,000 samples from cleveland dataset.
# drop.not.used = True (means don't use any original data in the synthesized df)
syn_cleve <- syn(df_new, seed = 306, k = 60000, drop.not.used = TRUE)

# write synthesized data to csv file
write.table(syn_cleve$syn, file = "data/syn_cleveland.csv", sep = ",", row.names = FALSE, col.names = TRUE)

# compare characteristics to original dataset
compare(syn_cleve, df_new, vars = "num")
compare(syn_cleve, df_new, vars = "age")
compare(syn_cleve, df_new, vars = "sex")
compare(syn_cleve, df_new, vars = "cp")
compare(syn_cleve, df_new, vars = "trestbps")
compare(syn_cleve, df_new, vars = "htn")
compare(syn_cleve, df_new, vars = "chol")
compare(syn_cleve, df_new, vars = "cigs")
compare(syn_cleve, df_new, vars = "years")
compare(syn_cleve, df_new, vars = "fbs")
compare(syn_cleve, df_new, vars = "famhist")
compare(syn_cleve, df_new, vars = "restecg")
compare(syn_cleve, df_new, vars = "dig")
compare(syn_cleve, df_new, vars = "prop")
compare(syn_cleve, df_new, vars = "nitr")
compare(syn_cleve, df_new, vars = "pro")
compare(syn_cleve, df_new, vars = "diuretic")
compare(syn_cleve, df_new, vars = "proto")
compare(syn_cleve, df_new, vars = "thaldur")
compare(syn_cleve, df_new, vars = "met")
compare(syn_cleve, df_new, vars = "thalach")
compare(syn_cleve, df_new, vars = "thalrest")
compare(syn_cleve, df_new, vars = "tpeakbps")
compare(syn_cleve, df_new, vars = "tpeakbpd")
compare(syn_cleve, df_new, vars = "trestbpd")
compare(syn_cleve, df_new, vars = "exang")
compare(syn_cleve, df_new, vars = "xhypo")
compare(syn_cleve, df_new, vars = "oldpeak")
compare(syn_cleve, df_new, vars = "slope")
compare(syn_cleve, df_new, vars = "rldv5e")
compare(syn_cleve, df_new, vars = "ca")
compare(syn_cleve, df_new, vars = "thal")
compare(syn_cleve, df_new, vars = "lmt")
compare(syn_cleve, df_new, vars = "ladprox")
compare(syn_cleve, df_new, vars = "ladhist")
compare(syn_cleve, df_new, vars = "cxmain")
compare(syn_cleve, df_new, vars = "om1")
compare(syn_cleve, df_new, vars = "rcaprox")
compare(syn_cleve, df_new, vars = "rcadist")