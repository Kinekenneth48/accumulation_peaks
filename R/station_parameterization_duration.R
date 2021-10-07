
#compute station parameters using duration
station_parameterization <- function(station_data, duration) {

  exceedance_info <- station_data %>%
    exceedance(x = DATE, y = wesd, threshold = 0, minDuration = duration, maxGap = 1)

  summary_of_acc_peak <- exceedance_info$exceedance %>%
    ungroup() %>%
    dplyr::select(exceedance_no, duration, date_start, date_peak,date_end ,intensity_max_abs) %>%
    rename(peak = intensity_max_abs)  %>%
    mutate(year = lubridate::year(date_peak), month = lubridate::month(date_peak), snow_yr = ifelse(month >= 10, year+1, year))


  summary_of_acc_peak <- Remove_na_cols(summary_of_acc_peak, "peak")


  peaks_per_year <- summary_of_acc_peak %>%
    group_by(snow_yr) %>%
    summarise(n = n())

  no_unique_yrs <- peaks_per_year %>%
    count(snow_yr) %>%
    tally(n)

  total_no_storm <- peaks_per_year %>%
    tally(n)




  # No fitted distribution when unique years is less than 15
  if (total_no_storm < 70){
    rm(station_data)
  }
  else {

    fit_poi <- extRemes::fpois(peaks_per_year$n)
    mean_p <- fit_poi[["parameter"]][1]


    #set a vector of training threshold
    vec <- stats::quantile(summary_of_acc_peak$peak, probs = seq(0.1, (1-(70/total_no_storm$n)), length.out = 20), na.rm=TRUE)


    #compare the predictive performances of the training threshold and select the best
    p <- threshr::ithresh(data = summary_of_acc_peak$peak, u_vec = vec, prior = "mdi", h_prior = list(a = 0.2))
    sum_mu <- summary(p)
    mu <- sum_mu[1,3]



    #fit the GP distribution to the storm peak and extract the necessary parameters
    y <- fevd(x = summary_of_acc_peak$peak, threshold = mu, type = c("GP"),  method = c("Lmoments"))



    xi <- y[["results"]][["shape"]]
    sigma <- y[["results"]][["scale"]]




    ID <-  unique(station_data$ID)
    NAME <- unique(station_data$NAME)
    STATE <- unique(station_data$STATE)
    LONGITUDE <- unique(station_data$LONGITUDE)
    LATITUDE <- unique(station_data$LATITUDE)

    df_parameters <- data.frame(ID = ID,
                                Station = NAME,
                                State = STATE,
                                num_obs = total_no_storm$n,
                                Longitude = LONGITUDE,
                                Latitude = LATITUDE,
                                location = mu,
                                scale = sigma,
                                shape = xi,
                                lambda = mean_p[["mean"]])


    return(df_parameters)

  }
}
