nums = c(1,2,8,17,30,49,12,15,19)
#numbers = c(1,2,3)

df <- data.frame(nums)

library(data.table)
library(dplyr)
#setDT(df)[, diff := abs(nums - lag(nums, 1L))]
#df[1,2] = 0
df$index = 0
df[1,2]=1

df <- as.data.table(df)
# for (i in 2:length(df$diff)){
#   df$index[i] <- ifelse(df$nums[i] > df$nums[i-1], df$diff[i] + df$index[i-1], df$nums[i] + df$index[i-1])
# }
    # if (df$nums[i] > df$nums[i-1]){
  #   df$index[i] = df$diff[i] + df$index[i-1]
  # }
  # else {
  #   df$index[i] = df$nums[i] + df$index[i-1]
  # }

for (i in 2:length(df$nums)) {
  print(i)
  df$index[i] <- ifelse(df$nums[i] > df$nums[i-1], df$nums[i] - df$nums[i-1] + df$index[i-1], df$nums[i] + df$index[i-1])
}


# newIndex <- function(df) {
#   for (i in 2:length(df$nums)) {
#     print(i)
#     df$index[i] <- ifelse(df$nums[i] > df$nums[i-1], df$nums[i] - df$nums[i-1] + df$index[i-1], df$nums[i] + df$index[i-1])
#   }
# }
# 
# apply(df, MARGIN = 2, function(x) newIndex(x))