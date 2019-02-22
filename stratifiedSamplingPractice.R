# Proportionate Stratified Sampling

set.seed(9850) 
df = data.frame(gender=rep(c("F","M"),c(6000,4000)), ht=c(rnorm(6000, mean=60, sd=5),rnorm(4000, mean=90, sd=5)))

head(df)
