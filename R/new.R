

numCores = detectCores()

cl = makeCluster(numCores)

clusterExport(cl, c("clean_dataset", "station_parameterization", "Remove_na_cols") , envir = environment())
clusterEvalQ(cl, {
  library(tidyverse)
  library(extRemes)
  library(heatwaveR)
  library(threshr)


})

df_station_parameters_duration2 = rbindlist(parLapply(cl, clean_dataset, station_parameterization, duration = 2))
stopCluster(cl)

df_station_parameters_duration2 = do.call(rbind, parLapply(cl, clean_dataset, FUN = station_parameterization, duration = 2))
