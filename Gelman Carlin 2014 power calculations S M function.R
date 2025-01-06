# Define retrodesign function from Gelman and Carlin (2014) for retrospective power analysis of Bayesian model
# return metrics: power, Type S error rate, and exaggeration ratio
# helps understand how well the model can detect true effects and the potential for overestimation of effect size

retrodesign <- function(effect_size, std_deviation, alpha=.05, df=Inf, n.sims=10000){  # student t is approx normal for n->inf, so changing df to true (large) finite value not important
  critical_val <- qt(1-alpha/2, df)
  p.hi <- 1 - pt(critical_val - effect_size/std_deviation, df)
  p.lo <- pt(-critical_val - effect_size/std_deviation, df)
  power <- p.hi + p.lo
  typeS <- p.lo/power
  estimate <- effect_size + std_deviation*rt(n.sims,df)
  significant <- abs(estimate) > std_deviation*critical_val
  exaggeration <- mean(abs(estimate)[significant])/effect_size
  return(list(power=power, typeS=typeS, exaggeration=exaggeration, power_correct_sign=power*(1-typeS)))
}
