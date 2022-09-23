## Investigate skewness

library(moments)

data <- read.csv("Ames_data.csv")
data = drop_irrelevant(data, c())

## Transform factors
data = convert_factors(data)

data.type = sapply(data, class)

table(data.type)
classes <- sapply(data, class)
df_numeric = data[, classes != "factor"]

sw_test = lapply(df_numeric, shapiro.test)
sk_test = lapply(df_numeric, skewness)
alpha = 0.01

for(name in names(df_numeric)) {
  p_val = sw_test[[name]]$p.value
  if(p_val >= alpha || any(df_numeric[name] < 0)) 
    next
  
  df_numeric[which(df_numeric[name] == 0), name] = 1
  
  df_numeric[, name] = log(df_numeric[, name])
}



report_nulls_zeros(df_numeric)

cond_log = function(x) {
  if(x<0) {
    return(x)
  }
  if(x==0) {
    x = 1
  }
  
  return(log(x))
}
df_logged = lapply(df_numeric, cond_log)

which(colSums(df_numeric==0, na.rm = FALSE) > 0)


df_numeric[which(df_numeric['Longitude'] < 0), 'Longitude'] = 1

any(df_numeric[name] < 0)


head(df_numeric)

head(data)

data[names(df_numeric)] = df_numeric

