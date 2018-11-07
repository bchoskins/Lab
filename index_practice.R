nums = c(1,2,8,17,30,49,12,15,19)
#numbers = c(1,2,3)

df <- data.frame(nums)

library(data.table)
library(dplyr)
setDT(df)[, diff := abs(nums - lag(nums, 1L))]
df[1,2] = 0
df$index = 0
df[1,3]=1

for (i in 2:length(df$diff)) {
  print(i)
  if (df$nums[i] > df$nums[i-1]){
    df$index[i] = df$diff[i] + df$index[i-1]
  }
  else {
    df$index[i] = 1 + df$index[i-1]
  }
}