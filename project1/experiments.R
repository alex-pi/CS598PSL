
library(ggcorrplot)
library(dplyr)

df <- read.csv("Ames_data.csv", stringsAsFactors = FALSE)

#df <- df[, colSums(is.na(df)) == 0]
df <- df[, -1]
df$Sale_Price = log(df$Sale_Price)


sum(is.na(df$Garage_Yr_Blt))

df = convert_factors(df)
str(df)

classes <- sapply(df, class)
df_num <- df[, classes == "numeric" | classes == "integer"]
fact_names <- names(df[, classes == "factor"])
df_facts <- df[, classes == "factor"]
cor <- cor(df_num)
ggcorrplot(cor, hc.order = TRUE, insig = "blank",
           type = "lower", ggtheme = theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           tl.cex = 10)

corr_names = names(cor['Sale_Price', which(cor['Sale_Price', ] > 0.3)])

pairs(df[, corr_names])

df[df$Total_Bsmt_SF == 0, ]$Total_Bsmt_SF = 1
df$Total_Bsmt_SF = log(df$Total_Bsmt_SF)
df$First_Flr_SF = log(df$First_Flr_SF)
df$Gr_Liv_Area = log(df$Gr_Liv_Area)
df[df$Garage_Area == 0, ]$Garage_Area = 1
df$Garage_Area = log(df$Garage_Area)
df[df$Open_Porch_SF == 0, ]$Open_Porch_SF = 1
df$Open_Porch_SF = log(df$Open_Porch_SF)
df[df$Wood_Deck_SF == 0, ]$Wood_Deck_SF = 1
df$Wood_Deck_SF = log(df$Wood_Deck_SF)


df_facs <- df[, sapply(df, class) == "factor"]
corr_names = corr_names[-15]

for (crname in corr_names) {

  for (i in 1:length(df_facs)) {
    fname = names(df_facs)[i]
    new_name = paste(crname, 'x', fname, sep='')
    
    df[new_name] = df[, crname] * as.numeric(df[, fname])
  }
}




classes <- sapply(train, class)
df_int <- train[, classes == "integer"]

str(df_int)

sapply(df_int, function(x) n_distinct(x))

ridxs = sample(1:dim(df_int)[1], 30)
df_int[ridxs, ]$BsmtFin_SF_1

df = drop_irrelevant(df, irrelevant_cols)
df2 = winsorization(df)

## Looking for columns with many repeated values.
for(cn in colnames(df)) {
  counts = as.data.frame(table(df[, cn]))
  counts$percentage = counts$Freq / 2930 *100
  if(any(counts$percentage > 95.0)) {
    print(paste("###", cn, "###"))
    print(counts) 
  }
}

## Enconding factors
df2 = expand_factors(df, use_ref_lvl = FALSE)
