################################################################################
## 197-03: Homework I, Reproducing "Scenario B" from Simmons et al. (2011)

require(knitr) #using to print final table
rm(list = ls())

no.of.sims <- 1500
sig.count.10 <- 0 
sig.count.05 <- 0
sig.count.01 <- 0
#using these variables to count how many simulations have results 
#that are significant (at p = 0.1, 0.05,and 0.01 respectively).

for(i in 1:no.of.sims)
{
  set.seed(i) #by setting a seed for each random variable pull
              #I am ensuring reproducibiity of results.
  liking <- rnorm(20,1,1)
  set.seed(2*i) #using the same seed for the two sets of variables will
                #generate identical vectors with Cov(x,y) = 1, making
                #the whole t-test meaningless.
   wtp <- rnorm(20,1,1)
   p.20 <- t.test(liking, y = wtp)$p.value #p-value for first twenty
   
   set.seed(3*i)
   liking <- c(liking, rnorm(10,1,1)) #adding 10 observations
   set.seed(4*i)
   wtp <- c(wtp, rnorm(10,1,1)) 
   #reminder: this is ALL noise - the names "liking" and "wtp" are
   #used because they were mentioned in passing by Simmons et al. (2011)
   #and the mean and SD of the "rnorm" fns. was also arbitrary.
   
   p.30 <- t.test(liking, y = wtp)$p.value
   sig.count.10 <- sig.count.10 + (p.20 < 0.10 | p.30 < 0.10)
   sig.count.05 <- sig.count.05 + (p.20 < 0.05 | p.30 < 0.05)
   sig.count.01 <- sig.count.01 + (p.20 < 0.01 | p.30 < 0.01)
   #using the "OR" function is equivalent to counting this as signinficant
   #if EITHER set of readings (20 or 30) have a significant relationship.
   #that is equivalent to Simmons et al.'s simulation of the author stopping
   #the experiment at 20 observations if it is significant, but otherwise
   #collecting ten more instances to see how that affects the p-value.
}

sigs <- c(sig.count.10,sig.count.05,sig.count.01)
sigs.percent <- as.data.frame(paste0(signif((
  sigs/no.of.sims)*100,digits = 2),"%"))
#converting the counts to % numbers

#next few lines are string manipulation to generate the results table
#no math to see here.
sig.levels <- as.data.frame(c("p < .1","p  < .05", "p < .01"))
result <- cbind(sig.levels,sigs.percent)
colnames(result) <- c("Signif. Levels", "% w/ Signif.")

cat("\014") #clear screen
print(kable(result)) #printing the final result table.

rm(list = ls())
