list.of.packages <- c("slam", "renv", "rsconnect")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(rsconnect)

rsconnect::setAccountInfo(name='team598psl',
                          token='244FBFE4920DAEA5B60BADF39700B6F9',
                          secret='uBUI0Yf7Xuc87crALV33JwKZiUfM2rmeUdComQJU')


rsconnect::deployApp('movies_app')
