
#Compute RTLS using the two-part simulation
two_part_sim_RTLs <- function(station_data){



  lambda = station_data$lambda
  location = station_data$location
  scale = station_data$scale
  shape = station_data$shape
  ID = unfactor(station_data$ID)

  julia_assign("lambda", lambda)
  julia_assign("location", location)
  julia_assign("scale", scale)
  julia_assign("shape", shape)


  julia_eval("x = rand(Truncated(Poisson(lambda), 0.0, 10), 50000000)")
  julia_eval("snowload = Array{Float64}(undef, 50000000)")
  julia_eval("
 for i in 1:50000000
 snowload[i] = max(rand(GeneralizedPareto(location, scale, shape)), x[i])
 end
 ")



  load = julia_eval("snowload")

  rl = gr_model(load * 0.00980665)*(load * 0.00980665)

  rtl =  rtsnow::get_rtl(tL = rl,
                         index = c(2.5, 3.0, 3.25, 3.5),
                         # Dead Load statistical Parameters (Normal)
                         nDL = 15*0.04788, #psf to kPa
                         biasDL = 1.05,
                         covDL = 0.1,
                         # Resistance Statistical Parameters (Normal)
                         # Assuming Flexural Beam Bartlette Parameters
                         biasR = 1.049,
                         covR = 0.09,
                         # Roof adjustment from the code.
                         roof_adjust = 0.7,
                         # Safety Factor
                         gl_safety = 1.0,
                         # Don't use the minimum load requirements.
                         #(Assume all roofs subject to 0.7 multiplier.)
                         minLoad = 0,
                         # Steel phi Factor
                         phi = 0.9,
                         # Number of years represented in each simulation
                         years = 1)

  return(c(ID, rtl))
}
