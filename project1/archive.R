add_product_interactions = function(df, num_names) {
  
  #classes <- sapply(df, class)
  #df_num <- df[, classes == "numeric" | classes == "integer"]
  #num_names = select_correlated_only(df, corr_min = 0.4)$corr_names
  print(num_names)
  
  for (cname in num_names) {
    if(cname == 'Sale_Price') {
      next
    }
    
    for (cnamet in num_names) {
      if(cnamet == 'Sale_Price') {
        next
      }      
      new_name = paste(cname, 'x', cnamet, sep='')
      print(new_name)
      df[new_name] = as.numeric(df[, cname]) * as.numeric(df[, cnamet])
    }
  }
  
  return(df)
  
}

select_correlated_only = function(df, corr_min = 0.3) {
  
  classes <- sapply(df, class)
  df_num <- df[, classes == "numeric" | classes == "integer"]
  fact_names <- names(df[, classes == "factor" | classes == "character"])
  cor <- cor(df_num)
  
  corr_names = names(cor['Sale_Price', which(cor['Sale_Price', ] > corr_min)])
  
  return(list(
    corr_names = corr_names,
    df = df[, c(fact_names, corr_names)]
  ))
}

add_factor_interactions = function(df, cnames) {
  
  df_facs <- df[, sapply(df, class) == "factor"]
  
  for (crname in cnames) {
    if(crname == 'Sale_Price') {
      next
    }
    
    for (i in 1:length(df_facs)) {
      fname = names(df_facs)[i]
      if(fname == 'Sale_Price') {
        next
      }
      new_name = paste(crname, 'x', fname, sep='')
      
      df[new_name] = df[, crname] * as.numeric(df[, fname])
    }
  }
  
  return(df)
}