#########################################################################################################
# Heart Disease Data synthesis from the Cleveland 14 dataset. 
# Purpose: To produced synthesized data that matches original sample characteristics in order to
#          anonymize sensitive medical data, and to increase the data volume for use in machine learning.
# By: Alfeo Sabay, SMU Masters in Data Science
#########################################################################################################
library(synthpop)

# read in original sample data (cleveland dataset from uci)
cols <- c("age", "sex", "chest_pain", "resting_bp", "cholesterol", "fast_sugar",
          "resting_ecg", "max_hrate", "exer_angina", "oldpeak", "slope", "ca_mavesel",
          "heart_def_status", "diag")

df_data <- read.csv("data/cleveland14_capstone.csv", header = FALSE)
colnames(df_data) <- cols
df_data$diag[df_data$diag > 0] <- 1 # recode diagnosis (diag) to 1 (from 1,2,3,4) if heart disease exists, or 0 if no heart disease exists


# synthesisze a data set of 50,000 samples from cleveland dataset.
# drop.not.used = True (means don't use any original data in the synthesized df)
syn_cleveland <- syn(df_data, seed = 306, k = 50000, drop.not.used = TRUE)

# write the surrogate data file
write.table(syn_cleveland$syn, file = "data/syn_cleveland14.csv", sep = ",", row.names = FALSE, col.names = FALSE)

# data frame with the synthetic data set
df_syn <- data.frame(syn_cleveland$syn)


# logistic regression summary
summary(glm(diag~age+sex+chest_pain+resting_bp+cholesterol+fast_sugar+
              resting_ecg+max_hrate+exer_angina+oldpeak+slope+ca_mavesel+heart_def_status, 
            data = df_syn, family = "binomial"))

# use synthetic data to fit logistic regression and compare to original logistic regression fit
summary(glm.synds(diag~age+sex+chest_pain+resting_bp+cholesterol+fast_sugar+
              resting_ecg+max_hrate+exer_angina+oldpeak+slope+ca_mavesel+heart_def_status, 
            data = syn_cleveland, family = "binomial"))

# comparison of percentages of observed vs synthetic data
s1 <- syn(df_data, seed = 306, k = 50000, drop.not.used = TRUE)

# logistic regression fit of synthetic data
s2 <- glm.synds(diag~age+sex+chest_pain+resting_bp+cholesterol+fast_sugar+
                  resting_ecg+max_hrate+exer_angina+oldpeak+slope+ca_mavesel+heart_def_status, 
                data = syn_cleveland , family = "binomial")

compare(s1, df_data, vars = "age")
compare(s1, df_data, vars = "sex")
compare(s1, df_data, vars = "chest_pain")
compare(s1, df_data, vars = "resting_bp")
compare(s1, df_data, vars = "cholesterol")
compare(s1, df_data, vars = "fast_sugar")
compare(s1, df_data, vars = "resting_ecg")
compare(s1, df_data, vars = "max_hrate")
compare(s1, df_data, vars = "exer_angina")
compare(s1, df_data, vars = "oldpeak")
compare(s1, df_data, vars = "slope")
compare(s1, df_data, vars = "ca_mavesel")
compare(s1, df_data, vars = "heart_def_status")
compare(s1, df_data, vars = "diag")

# Comparison of Model Estimates based on Synthesized and Observed Data
compare(s2, df_data, plot = "Z", print.coef = TRUE, return.plot = TRUE, 
        plot.intercept = FALSE, lwd = 1, lty = 1, lcol = c("#1A3C5A","#4187BF"), 
        dodge.height = .5, point.size = 2.5, incomplete = FALSE, population.inference = FALSE, ci.level = 0.95)
