library(prevalence)
n_values=1000
n_025=ceiling(n_values*0.025)
n_975=ceiling(n_values*0.975)

#Distribution parameters for YF CFR (Johansson)
set.seed(1)
beta1 <- betaExpert(best=0.12, lower=0.05, upper=0.26, p = 0.95, method = "mean")
p_severe_inf_values <- sort(rbeta(n_values,beta1$alpha, beta1$beta))
plot(p_severe_inf_values)
mean(p_severe_inf_values)
median(p_severe_inf_values)
p_severe_inf_values[n_025]
p_severe_inf_values[n_975]

#Distribution parameters for YF CFR (Servadio)
set.seed(1)
beta2 <- betaExpert(best=0.39, lower=0.31, upper=0.47, p = 0.95, method = "mean")
p_fatality_severe_inf_values <- sort(rbeta(n_values,beta2$alpha, beta2$beta))
plot(p_fatality_severe_inf_values)
mean(p_fatality_severe_inf_values)
median(p_fatality_severe_inf_values)
p_fatality_severe_inf_values[n_025]
p_fatality_severe_inf_values[n_975]

saveRDS(data.frame(P_severe=p_severe_inf_values,P_severeDeath=p_fatality_severe_inf_values),
        file="data/P_severe_severeDeath_1000.RDS")