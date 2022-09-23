
data = mtcars[, c('mpg','hp','am')]

data


when <- data.frame(time = c("afternoon", "night", "afternoon",
                            "morning", "morning", "morning",
                            "morning", "afternoon", "afternoon"),
                   day = c("Mon", "Mon", "Mon",
                           "Wed", "Wed", "Fri",
                           "Sat", "Sat", "Fri"),
                   y = sample(1:40, 9),
                   stringsAsFactors = TRUE)

levels(when$time) <- list(morning="morning",
                          afternoon="afternoon",
                          night="night")
levels(when$day) <- list(Mon="Mon", Tue="Tue", Wed="Wed", Thu="Thu",
                         Fri="Fri", Sat="Sat", Sun="Sun")

## Default behavior:
model.matrix(~.^2, when)

dummyVars(~ day + time, data = when)


tm = train[sample(1:300, 20), ]
ttm = test[sample(1:300, 20), ]

unique(tm$Condition_1)[1]


exp_tm = model.matrix(~., tm)
exp_ttm = model.matrix(~., ttm)

exp_both = model.matrix(~., rbind(tm[, -72], ttm))

dim(exp_tm)
dim(exp_ttm)
dim(exp_both)


dftr = train[, -72]
dfte = test

exp_dftr = as.data.frame(model.matrix(~., data = dftr))
colnames(exp_dftr)[1] = 'Reference_Level'
dftrnames = colnames(exp_dftr)[order(colnames(exp_dftr))]

exp_dfte = as.data.frame(model.matrix(~., data = dfte))
colnames(exp_dfte)[1] = 'Reference_Level'
dftenames = colnames(exp_dfte)[order(colnames(exp_dfte))]

dim(dftr)
dim(exp_dftr)
dim(dfte)
dim(exp_dfte)


(missing_lvls = setdiff(dftrnames, dftenames))

(new_lvls = setdiff(dftenames, dftrnames))


## Merge
exp_dfte[missing_lvls] = 0
exp_dfte = exp_dfte[, -which(colnames(exp_dfte) %in% new_lvls)]

dfnames = colnames(exp_dftr)[order(colnames(exp_dftr))]
df2names = colnames(exp_dfte)[order(colnames(exp_dfte))]

setdiff(dfnames, df2names)
setdiff(df2names, dfnames)

dfnames == df2names

### Code from Prof.

expand_factors = function(df) {
  #df # train data without "PID" and "Sale_Price"
  
  categorical.vars = colnames(df)[
    which(sapply(df,
                 function(x) class(x)=="factor"))]
  
  # Keep only no categoricals
  df_numerical = df[, !colnames(df) %in% categorical.vars, 
                    drop=FALSE]
  
  n.train <- nrow(df_numerical)
  for(var in categorical.vars){
    mylevels <- sort(unique(df[, var]))
    m <- length(mylevels)
    m <- ifelse(m>2, m, 1)
    new_columns <- matrix(0, n.train, m)
    col.names <- NULL
    for(j in 1:m){
      new_columns[df[, var]==mylevels[j], j] <- 1
      col.names <- c(col.names, paste(var, '_', mylevels[j], sep=''))
    }
    colnames(new_columns) <- col.names
    df_numerical <- cbind(df_numerical, new_columns)
  }
  
  return(df_numerical)
  
}

newdf = expand_factors(train)
newdft = expand_factors(test)

train.x = train[, -y_index]

categorical.vars = colnames(train.x)[
  which(sapply(train.x,
               function(x) class(x)=="factor"))]

# Keep only no categorical
train.matrix = train.x[, !colnames(train.x) %in% categorical.vars, 
                        drop=FALSE]

n.train <- nrow(train.matrix)
for(var in categorical.vars){
  mylevels <- sort(unique(train.x[, var]))
  m <- length(mylevels)
  m <- ifelse(m>2, m, 1)
  tmp.train <- matrix(0, n.train, m)
  col.names <- NULL
  for(j in 1:m){
    tmp.train[train.x[, var]==mylevels[j], j] <- 1
    col.names <- c(col.names, paste(var, '_', mylevels[j], sep=''))
  }
  colnames(tmp.train) <- col.names
  train.matrix <- cbind(train.matrix, tmp.train)
}













