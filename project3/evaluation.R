library(pROC)

min_auc = 1

top_dir = getwd( )
mymain = paste(top_dir, "/mymain.R", sep = '')

for (j in 1:5)
{
  
  print(paste("J:", j))
  
  # Get the current split's working directory path 
  wd = paste( getwd( ), "/split_", j, sep = '')
  
  # Set directory to the current split's folder
  setwd(wd)
  
  file.copy(file.path(top_dir,"myvocab.txt"), wd)
  
  
  source(mymain)
  
  
  # move "test_y.tsv" to this directory
  test.y <- read.table("test_y.tsv", header = TRUE)
  pred <- read.table("mysubmission.txt", header = TRUE)
  pred <- merge(pred, test.y, by="id")
  roc_obj <- roc(pred$sentiment, pred$prob)
  this_split_auc = pROC::auc(roc_obj)
  print(paste("This split AUC:", this_split_auc))
  
  min_auc = min(min_auc, this_split_auc)
  setwd('..')
}
print(paste("Minimum AUC: ", min_auc))

