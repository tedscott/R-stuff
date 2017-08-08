
# download the csv


# use CosmosToR package to download from cosmos streams directly to DF
library(CosmosToR)
vc <- vc_connection('https://cosmos14.osdinfra.net/cosmos/ACE.proc/')
filepath <- "/users/tedscott/DerivativeRetention500000.ss" 