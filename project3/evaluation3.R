library( pROC )

set.seed( 867 )

# global variables
results        = matrix( 0, 5, 2 )
regexp         = "" # [^a-zA-Z0-9]"
loss_type      = 0    # 0=ridge, 1=lasso, 2=XGBoost
sample_rate    = 0.55
n_gram         = c(1L, 4L )
nb_folds       = 4
threshold      = 1e-3
max_iterations = 1e3

min_auc = 1

top_dir = getwd( )
mymain  = paste( top_dir, "/mymain.R", sep = '' )

tt = Sys.time()
str = NULL
for ( j in 1:5 )
{
  cat( str, "--- split ", j, " ---\n", sep="" )
  #print( paste( "J:", j ) )
  
  # Get the current split's working directory path 
  wd = paste( getwd( ), "/split_", j, sep = '' )
  
  # Set directory to the current split's folder
  setwd( wd )
  
  file.copy( file.path( top_dir, "myvocab3.txt" ), wd, overwrite = TRUE )
  
  source( mymain )
  
  # move "test_y.tsv" to this directory
  test.y  <- read.table( "test_y.tsv",       header = TRUE )
  pred    <- read.table( "mysubmission.txt", header = TRUE )
  pred    <- merge( pred, test.y, by="id" )
  roc_obj <- roc( pred$sentiment, pred$prob )
  this_split_auc = pROC::auc( roc_obj )
  #print( paste( "split AUC:", this_split_auc ) )
  cat( str, "AUC: ", this_split_auc, "\n", sep="" )
  results[j,2] = this_split_auc
  min_auc = min( min_auc, this_split_auc )
  setwd( '..' )
}
#print( paste( "Minimum AUC: ", min_auc ) )
d_time = difftime( Sys.time(), tt, units = 'sec' )

#------------------------------------------------
cat( str, "--- results ---",
     "\n Loss Type: ", loss_type, 
     "\n    N-gram: ", n_gram[1], " ", n_gram[2], 
     "\n   NbFolds: ", nb_folds,
     "\n threshold: ", threshold, 
     "\niterations: ", max_iterations, 
     "\n    regexp: ", regexp, 
     "\n",
     "\n", results[1,1], " sec   AUC: ", results[1,2],
     "\n", results[2,1], " sec   AUC: ", results[2,2],
     "\n", results[3,1], " sec   AUC: ", results[3,2],
     "\n", results[4,1], " sec   AUC: ", results[4,2],
     "\n", results[5,1], " sec   AUC: ", results[5,2],
     "\n\n", d_time, " sec   min AUC: ", min_auc, 
     "\n", 
     sep=""
)
