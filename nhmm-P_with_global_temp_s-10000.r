library(NHMM)
library(readxl)
library(ggplot2)
library(patchwork)
library(readr)

# getwd()
setwd("~/WorkSpace/yanl/nhmm-world/P~temp_s")

DATA <- read.table("SSTA_SCALE_Pacific2_2023.1.csv",header = FALSE, sep = "," , stringsAsFactors = FALSE)
EXO <- read.table("EXO.csv",header = FALSE, sep = "," , stringsAsFactors = FALSE)

#smoothed monthly global temp  from 1880-1 to 2022-12
temp_s <- read_csv("temp_smoothed.csv")

EXO_3 <- cbind(EXO[301:2004,1:2], temp_s[1:1704,2])

# modify the time period of SSTA data, we just use the 1881.1-2022.12 SSTA data
SSTA = DATA[301:2004, ]

xsel1 <- data.matrix(EXO_3)
ysel1 <- data.matrix(SSTA)

# a sample of the NHMM model, run several times to find the best model
my.nhmm=NHMM(y=ysel1, X=t(xsel1), K=5, iters=10000, burnin=2000, emdist="gamma", 
             nmix=1, delta=TRUE)



zz=Oz(my.nhmm)
Trans_prob=OQQ(my.nhmm,FALSE)     #transition probability
pp=OWcoef(my.nhmm,FALSE)
tt=Oemparams(my.nhmm,FALSE)
EXO_coe = OXcoef(my.nhmm, plots = FALSE, outfile = NULL)
# [1] "The Markov property of the model is significant with 95% probability."
# [1] "The Markov property of the model is significant with 90% probability."
# [1] "Input1: X is significant with 95% probability"
# [1] "Input2: X is significant with 95% probability"
# [1] "Input3: X is significant with 95% probability"
# [1] "Input1: X is significant with 90% probability"
# [1] "Input2: X is significant with 90% probability"
# [1] "Input3: X is significant with 90% probability"

ind = c(2, 1, 3, 5, 4)
EXO_coe = OXcoef(my.nhmm, plots = TRUE, outfile = NULL)
EXO_coe2 = EXO_coe
EXO_coe2[1,,]=EXO_coe[2,,]
EXO_coe2[2,,]=EXO_coe[1,,]
EXO_coe2[3,,]=EXO_coe[3,,]
EXO_coe2[4,,]=EXO_coe[4,,]  # Strong El Nino
EXO_coe2_temp1 = EXO_coe2[,1,]
EXO_coe2_temp2 = EXO_coe2[,4,]
EXO_coe2[,1,]=EXO_coe2[,2,]
EXO_coe2[,2,]=EXO_coe2_temp1
EXO_coe2[,3,]=EXO_coe2[,3,]
EXO_coe2[,4,]=EXO_coe2[,5,]
EXO_coe2[,5,]=EXO_coe2_temp2
write.csv(EXO_coe2, file = "Oxcoef_P_temp_s_k5-10000.csv")

# AD test
OBIC(my.nhmm)
logl = my.nhmm$loglik
density_estimate <- density(logl[2001:10000])
plot(density_estimate, main="Probability Density Function", xlab="Value", ylab="Density")
qqnorm(logl[2001:10000])
qqline(logl[2001:10000])
ks.test(logl[2001:10000], "pnorm", mean(logl[2001:10000]), sd(logl[2001:10000]))
library(nortest)
ad.test(logl[2001:10000])



write.csv(zz, file = "zz_PAC_temp_s_k5-10000.csv")
write.csv(logl, file = "ll_PAC_temp_s_k5-10000.csv")
#write.csv(pp, file = "Owcoef_PAC_temp_k5-10000.csv")
#save.image(file = "PAC_temp_s_k5-10000-2.RData")

#######transition probabilities###########
#mean transition probabilities
ind = c(2,1,3,5,4)
VIT<-Trans_prob[,,1]
for (i in 1:5) {
  for (j in 1:5){
    x=Trans_prob[ind[i],ind[j],] 
    VIT[i,j]<-mean(x)
  }    
}
write.csv(VIT, file = "Transprob_PAC_k5.csv")


#######beta values: transformations###########

library(ggplot2)
library(reshape2)
library(dplyr)

load("~/Desktop/P~temp_s/PAC_temp_s_k5-10000-4-best.RData")
betavalues = my.nhmm$betasave

new_betavalues <- array(0, dim = dim(betavalues))
dimnames(new_betavalues) <- dimnames(betavalues)

ref3_draws <- betavalues[3, , ]

for (j in 1:5) {
  new_betavalues[j, , ] <- betavalues[j, , ] - ref3_draws
}

temp_row_for_swap <- new_betavalues[1,,]
temp_row_for_swap2 <- new_betavalues[4,,]
coeffs = new_betavalues
coeffs[1,,] <- new_betavalues[2,,]
coeffs[2,,] <- temp_row_for_swap
coeffs[4,,] <- new_betavalues[5,,]
coeffs[5,,] <- temp_row_for_swap2
temp_column1 = coeffs[,1,]
temp_column2 = coeffs[,4,]
coeffs[,1,]=coeffs[,2,]
coeffs[,2,]=temp_column1
coeffs[,3,]=coeffs[,3,]
coeffs[,4,]=coeffs[,5,]
coeffs[,5,]=temp_column2

plot_row_display_names <- c("K_i1", "K_i2", "K_i4", "K_i5")
plot_col_display_names <- c("K1", "K2", "K3", "K4", "K5", "sin", "cos", "Temp_s")

num_plot_rows <- length(plot_row_display_names)
num_plot_cols <- length(plot_col_display_names)
num_iterations <- dim(coeffs)[3]

data_to_plot <- coeffs[c(1,2,4,5), 1:num_plot_cols, , drop = FALSE]

dimnames(data_to_plot) <- list(
  Row = plot_row_display_names,
  Col = plot_col_display_names,
  Iteration = 1:num_iterations
)
plot_data_long <- melt(data_to_plot, value.name = "Value")
plot_data_long$Iteration <- as.numeric(as.character(plot_data_long$Iteration))
plot_data_long <- plot_data_long %>%
  mutate(
    Row = factor(Row, levels = plot_row_display_names),
    Col = factor(Col, levels = plot_col_display_names)
  )
