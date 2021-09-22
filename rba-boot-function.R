## ADAPTED RBA BOOTSTRAP FUNCTION
RBAc <- function(n,l,m,h,vh,auc,rxl,rxm,rxh,rxvh) {
  Pl <- l/n # generates risk bin probabilities based on observed numbers: l, m, h, vh
  Pm <- m/n
  Ph <- h/n
  Pvh <- vh/n
  pot1 <- sample(1:4,size = n, prob = c(Pl,Pm,Ph,Pvh), replace = TRUE) 
  # synthetic sample: assignment prob same as sample
  pot2 <- sample(1:4,size = n, prob = c(.25,.25,.25,.25), replace = TRUE) 
  # synthetic sample: assignment prob chance effect
  classpot <- as.factor(sample(0:1, size = n,prob = c(auc, 1-auc), replace = TRUE)) 
  # selects pot1 or pot2 allocations using prob = auc
  all <- data.frame(pot1, pot2, classpot)
  all$final <- ifelse(all$classpot==1, pot2, pot1)
  out <- as.data.frame(table(all$final))
  totrx <- sum(c(round(out[1,2]*rxl,digits=0), round(out[2,2]*rxm,digits=0),
                 round(out[3,2]*rxh,digits=0), round(out[4,2]*rxvh,digits=0))) 
  # sums product of allocations * rx rates: rxl, rxm, rxh, rxvh
  return(totrx)
}

# EXAMPLE USAGE
# 4 category risk assessment tool (e.g., Risk Matrix 2000/s: (Thornton, 2007; Thornton et al., 2003))
# sample n = 200, risk assessment auc = 0.68
# risk assessment rates: low risk: .01 (1%); medium: .05 (5%); high: .15 (15%); very high: .35 (35%)
# classifcations for sample: low risk: n = 10; medium: n = 70; high: n = 80; very high: n = 40

adapted_rba <- function(n) { replicate(n, RBAc(200, 10, 70, 80, 40, .68, .01, .05, .15, .35)) } 
set.seed(12345)
adapted_rba_boot <- adapted_rba(50000) 
summary(adapted_rba_boot)
sd(adapted_rba_boot)
gmodels::ci(adapted_rba_boot)

# Thornton, D., Mann, R., Webster, S., Blud, L., Travers, R., Friendship, C., et al. (2003). 
# Distinguishing and combining risks for sexual and violent recidivism. Annals of New York Academy of Sciences, 989, 225–235.
# Thornton, D. (2007, unpublished). 
# Scoring guide for Risk Matrix 2000.9/SVA. Retrieved August 13, 2018, from https://www.birmingham.ac.uk/documents/college-les/psych/rm2000scoringinstructions.pdf 
# END
