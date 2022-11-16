
dept_sales_graph = function(data, title) {

  xl = c(1, dim(data)[2])
  yl = c(min(data), max(data))
  
  plot(NULL, NULL, 
       xlab = "Weeks",
       ylab = "Sales",
       xlim = xl, 
       ylim = yl,
       main = title)
  
  cols_ = hcl.colors(dim(data)[1], palette = "Teal")
  for(s in 1:dim(data)[1]) {
    #print(xl[2])
    #print(dim(data[s,]))
    lines(x = 1:xl[2], y = c(data[s,]), 
          col = cols_[s])
  }

}

dept_train = train_split[[1]][,1:4]

train_svd_ = dept_train %>% 
  select(Store, Dept, Date, Weekly_Sales) %>%
  spread(Date, Weekly_Sales)

no_svd = train_svd_[,-c(1,2)]

dept_train = train_split[[3]][,1:4]

train_svd_ = dept_train %>% 
  select(Store, Dept, Date, Weekly_Sales) %>%
  spread(Date, Weekly_Sales)

no_svd2 = train_svd_[,-c(1,2)]

par(mfrow = c(2, 2))

dept_sales_graph(no_svd, "Sales for Department 1")
dept_sales_graph(no_svd2, "Sales for Department 3")








with_svd = train_svd_[,-c(1,2)]

with_svd2 = train_svd_[,-c(1,2)]

dept_sales_graph(with_svd, "Sales for Department 1 (SVD)")
dept_sales_graph(with_svd2, "Sales for Department 3 (SVD)")

