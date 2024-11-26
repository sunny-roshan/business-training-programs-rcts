# Although df has been left as infinity, changing it to the actual sample size makes virtually
# no difference since the student t is approx normal for n->inf.
retrodesign <- function(A, s, alpha=.05, df=Inf, n.sims=10000){
  z <- qt(1-alpha/2, df)
  p.hi <- 1 - pt(z-A/s, df)
  p.lo <- pt(-z-A/s, df)
  power <- p.hi + p.lo
  typeS <- p.lo/power
  estimate <- A + s*rt(n.sims,df)
  significant <- abs(estimate) > s*z
  exaggeration <- mean(abs(estimate)[significant])/A
  return(list(power=power, typeS=typeS, exaggeration=exaggeration, power_correct_sign=power*(1-typeS)))
}
retrodesign(0.1,3.28)
0.4644228*0.05010648
(1-0.4644228)*0.05010648
