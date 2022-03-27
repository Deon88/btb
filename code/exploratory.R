
library(tidyverse)

# scotland

scot <- read.csv("D:\\btb\\data\\tidied data\\scotland.csv")
scot <- gather(scot, county, btb, Aberdeenshire:Wigtown)
scot$Time.Period <- gsub(" ", "-", scot$Time.Period, fixed = TRUE)
scot$Time.Period <- paste0("01-", scot$Time.Period)
scot$Time.Period <- as.Date(scot$Time.Period, format = "%d-%B-%Y")
colnames(scot)[1] <- "date"
scot$region <- "Scotland"

# wales

wales <- read.csv("D:\\btb\\data\\tidied data\\wales.csv")
wales <- gather(wales, county, btb, Anglesey:West.Glamorgan)
wales$Time.Period <- gsub(" ", "-", wales$Time.Period, fixed = TRUE)
wales$Time.Period <- paste0("01-", wales$Time.Period)
wales$Time.Period <- as.Date(wales$Time.Period, format = "%d-%B-%Y")
colnames(wales)[1] <- "date"
wales$region <- "Wales"

# england high

eng_h <- read.csv("D:\\btb\\data\\tidied data\\england_high.csv")
eng_h <- gather(eng_h, county, btb, Avon:Wiltshire)
eng_h$Time.Period <- gsub(" ", "-", eng_h$Time.Period, fixed = TRUE)
eng_h$Time.Period <- paste0("01-", eng_h$Time.Period)
eng_h$Time.Period <- as.Date(eng_h$Time.Period, format = "%d-%b-%y")
colnames(eng_h)[1] <- "date"
eng_h$region <- "England_high"

# england edge

eng_e <- read.csv("D:\\btb\\data\\tidied data\\england_edge.csv")
eng_e <- gather(eng_e, county, btb, Berkshire:Warwickshire)
eng_e$Time.Period <- gsub(" ", "-", eng_e$Time.Period, fixed = TRUE)
eng_e$Time.Period <- paste0("01-", eng_e$Time.Period)
eng_e$Time.Period <- as.Date(eng_e$Time.Period, format = "%d-%b-%y")
colnames(eng_e)[1] <- "date"
eng_e$region <- "England_edge"

# england low

eng_l <- read.csv("D:\\btb\\data\\tidied data\\england_low.csv")
eng_l <- gather(eng_l, county, btb, Bedfordshire:West.Yorkshire)
eng_l$Time.Period <- gsub(" ", "-", eng_l$Time.Period, fixed = TRUE)
eng_l$Time.Period <- paste0("01-", eng_l$Time.Period)
eng_l$Time.Period <- as.Date(eng_l$Time.Period, format = "%d-%B-%Y")
colnames(eng_l)[1] <- "date"
eng_l$region <- "England_low"

df <- rbind(scot, wales, eng_h, eng_l, eng_e)

ggplot(df) +
  geom_point(aes(x = date, y = btb, colour = region),
             alpha = 0.05) +
  #facet_wrap(~ region) +
  geom_smooth(aes(x = date, y = btb, colour = region, fill = region), method = "gam", formula = y ~ s(x, k = 10)) +
  theme_minimal()



coords <- data.frame(
  county = unique(df$county),
  lat = c(
    57.257310, 56.727029, 56.250000, 55.544457, 57.500000, 55.750000, 55.822670, 58.417000, 56.109141, 55.960000, 55.069020, 55.955134, 55.944538, 56.291451, 57.083330, 56.917000, 56.214194, 54.837384, 55.865680, 57.556091, 57.548639, 58.984970, 55.653281, 56.396355, 55.877661, 57.667000, 55.596500, 55.547049, 60.332788, 56.117254, 58.091120, 55.889179, 54.867757, 53.286850, 51.904149, 52.201701, 53.052276, 51.568965, 52.942005, 51.816000, 52.362142, 52.746228, 51.862518, 51.481310, 51.947609, 51.695399, 51.537271, 50.422459, 50.784937, 50.965621, 51.835564, 52.104900, 52.652085, 51.090617, 52.866525, 52.470000, 51.302582, 52.061137, 52.351185, 54.525000, 54.483973, 54.772875, 53.897254, 51.779105, 51.508698, 51.567186, 53.509211, 51.794696, 50.676651, 49.920761, 51.219917, 53.889327, 53.117603, 53.407671, 52.694735, 54.262343, 55.255691, 53.443276, 52.195887, 51.257911, 54.904450, 50.952663, 53.767234, 51.453077, 51.774285, 53.216055, 53.142210, 50.854783, 51.013370, 52.637752, 52.297033, 53.140291, 51.764110, 52.290341 ),
  lon = c(
    -2.582569, -2.895406, -5.250000, -4.575509, -3.083330, -2.500000, -5.088143, -3.500000, -3.754089, -4.530000, -3.607726, -2.723493, -3.252208, -3.166740, -4.666667, -2.500000, -3.426940, -4.049668, -4.257138, -3.260489, -3.880854, -2.958252, -3.194548, -3.437749, -4.393195, -5.000000, -2.456200, -2.846338, -1.248914, -3.943125, -4.530788, -3.584723, -4.448027, -4.376142, -4.176073, -4.255353, -2.953221, -3.289481, -4.021545, -3.368000, -3.435665, -3.424678, -4.906826, -3.180498, -3.402706, -3.528531, -3.533991, -4.844708, -3.820975, -2.490125, -2.120949, -2.746337, -2.761619, -2.999076, -2.027307, -2.290000, -1.948593, -0.447403, 0.033918, -1.189000, -3.062691, -1.575347, -0.541199, 0.520274, -0.124863, 0.146116, -2.295323, -0.085920, -1.288707, -6.298471, 0.826559, -2.660970, -0.188466, -2.829014, 1.035677, -1.756932, -2.052170, -1.457559, 1.095247, -0.497507, -1.381453, -0.532384, -1.690552, -1.134992, -0.834180, -2.716960, -1.615621, -0.036713, -1.096191, -1.126000, -0.902817, -1.000922, -1.346373, -1.543272
  )
)

unique_county <- unique(coords$county)
df$lat <- NA
df$lon <- NA
for(i in 1:length(unique_county)) {
  df$lat[df$county == unique_county[i]] <- coords$lat[coords$county == unique_county[i]]
  df$lon[df$county == unique_county[i]] <- coords$lon[coords$county == unique_county[i]]
}

df$month <- as.numeric(format(df$date,"%m"))
df$year <- as.numeric(format(df$date,"%Y"))
df$date2 <- as.numeric(format(df$date, "%Y.%m"))

# growth rate of btb

uniq_date <- unique(df$date2)
uniq_count <- unique(df$county)

df$growth <- NA
df$btb_adj <- df$btb + 1

for(i in 1:length(uniq_count)){
  for(t in 2:length(uniq_date)){
    df$growth[df$county == uniq_count[i] & df$date2 == uniq_date[t]] <- 
      log(df$btb_adj[df$county == uniq_count[i] & df$date2 == uniq_date[t]]) - 
      log(df$btb_adj[df$county == uniq_count[i] & df$date2 == uniq_date[t-1]])
    
  }
}


UK <- map_data(map = "world", region = "UK") # changed map to "world"

ggplot(data = df[df$year == 2021,]) +
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_point(aes(x = lon, y = lat, fill = growth),
             pch = 21, colour = "grey", alpha = 0.8, size = 2) +
  facet_wrap(year ~ month) +
  theme_classic() +
  scale_fill_viridis_c(option = "C") +
  coord_map()


library(gganimate)

a <- ggplot(data = df) +
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_point(aes(x = lon, y = lat, fill = growth),
             pch = 21, colour = "grey", alpha = 0.8, size = 5) +
  theme_classic() +
  scale_fill_viridis_c(option = "C") +
  coord_map() +
  labs(title = "Date: {closest_state}",
       y = "Latitude",
       x = "Longitude",
       fill = "bTB growth rate",
       size = "bTB growth rate") +
  enter_fade() +
  exit_disappear() +
  transition_states(date2, transition_length = 1, state_length = 2) 

animate(a, nframes = 624)

ggplot(df) +
  # geom_jitter(aes(x = month, y = growth, colour = region),
  #             alpha = 0.05, height = 0.1, width = 0.25) +
  geom_smooth(aes(x = month, y = growth, colour = region, fill = region),
              method = "gam", formula = y ~ s(x, k = 10, bs = "cc")) +
  facet_wrap(. ~ region, scales = "free_y") +
  theme_bw()

ggplot(df) +
  geom_point(aes(x = date2, y = growth, colour = region),
             alpha = 0.2) +
  geom_smooth(aes(x = date2, y = growth, colour = region),
              method = "gam", formula = y ~ s(x, k = 50)) +
  theme_bw()



# lat lon to utm ----------------------------------------------------------

library(sp)
library(rgdal)

coordinates(df) <- c("lon", "lat")
proj4string(df) <- CRS("+proj=longlat +datum=WGS84")  ## for example

df_xy <- spTransform(df, CRS("+proj=utm +zone=30 ellps=WGS84"))
df <- as.data.frame(df_xy)

ggplot(data = df[df$year == 2021,]) +
  geom_point(aes(x = lon, y = lat, fill = growth),
             pch = 21, colour = "grey", alpha = 0.8, size = 2) +
  facet_wrap(year ~ month) +
  theme_classic() +
  scale_fill_viridis_c(option = "C")



# Time --------------------------------------------------------------------

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

df$t <- elapsed_months(start_date = min(df$date), end_date = df$date)


ggplot(df[df$region == "England_high",]) +
  geom_point(aes(x = t, y = btb, fill = growth),
             alpha = 0.5, pch = 21, colour = "white") +
  geom_smooth(aes(x = t, y = btb),
              method = "gam", formula = y ~ s(x, bs = "cr", k = 15)) +
  scale_fill_viridis_c(option = "C", limits = c(-1, 1), oob = scales::squish) +
  facet_wrap(. ~ county, scales = "free_y")

ggplot(df[df$region == "England_high",]) +
  geom_point(aes(x = t, y = growth, fill = btb),
             alpha = 0.5, pch = 21, colour = "white") +
  geom_smooth(aes(x = t, y = growth),
              method = "gam", formula = y ~ s(x, bs = "cr", k = 15)) +
  scale_fill_viridis_c(option = "C") +
  facet_wrap(. ~ county, scales = "free_y")

ggplot(df[df$region == "England_edge",]) +
  geom_point(aes(x = t, y = btb, fill = growth),
             alpha = 0.5, pch = 21, colour = "white") +
  geom_smooth(aes(x = t, y = btb),
              method = "gam", formula = y ~ s(x, bs = "cr", k = 15)) +
  scale_fill_viridis_c(option = "C", limits = c(-1, 1), oob = scales::squish) +
  facet_wrap(. ~ county, scales = "free_y")

ggplot(df[df$region == "England_edge",]) +
  geom_point(aes(x = t, y = growth, fill = btb),
             alpha = 0.5, pch = 21, colour = "white") +
  geom_smooth(aes(x = t, y = growth),
              method = "gam", formula = y ~ s(x, bs = "cr", k = 15)) +
  scale_fill_viridis_c(option = "C") +
  facet_wrap(. ~ county, scales = "free_y")

ggplot(df[df$region == "Wales",]) +
  geom_point(aes(x = t, y = btb, fill = growth),
             alpha = 0.5, pch = 21, colour = "white") +
  geom_smooth(aes(x = t, y = btb),
              method = "gam", formula = y ~ s(x, bs = "cr", k = 15)) +
  scale_fill_viridis_c(option = "C", limits = c(-1, 1), oob = scales::squish) +
  facet_wrap(. ~ county, scales = "free_y")

ggplot(df[df$region == "Wales",]) +
  geom_point(aes(x = t, y = growth, fill = btb),
             alpha = 0.5, pch = 21, colour = "white") +
  geom_smooth(aes(x = t, y = growth),
              method = "gam", formula = y ~ s(x, bs = "cr", k = 15)) +
  scale_fill_viridis_c(option = "C") +
  facet_wrap(. ~ county, scales = "free_y")

ggplot(df[df$region == "England_low",]) +
  geom_point(aes(x = t, y = btb, fill = growth),
             alpha = 0.5, pch = 21, colour = "white") +
  geom_smooth(aes(x = t, y = btb),
              method = "gam", formula = y ~ s(x, bs = "cr", k = 15)) +
  scale_fill_viridis_c(option = "C", limits = c(-1, 1), oob = scales::squish) +
  facet_wrap(. ~ county, scales = "free_y")

ggplot(df[df$region == "England_low",]) +
  geom_point(aes(x = t, y = growth, fill = btb),
             alpha = 0.5, pch = 21, colour = "white") +
  geom_smooth(aes(x = t, y = growth),
              method = "gam", formula = y ~ s(x, bs = "cr", k = 15)) +
  scale_fill_viridis_c(option = "C") +
  facet_wrap(. ~ county, scales = "free_y")

ggplot(df[df$region == "Scotland",]) +
  geom_point(aes(x = t, y = btb, fill = growth),
             alpha = 0.5, pch = 21, colour = "white") +
  geom_smooth(aes(x = t, y = btb),
              method = "gam", formula = y ~ s(x, bs = "cr", k = 15)) +
  scale_fill_viridis_c(option = "C", limits = c(-1, 1), oob = scales::squish) +
  facet_wrap(. ~ county, scales = "free_y")

ggplot(df[df$region == "Scotland",]) +
  geom_point(aes(x = t, y = growth, fill = btb),
             alpha = 0.5, pch = 21, colour = "white") +
  geom_smooth(aes(x = t, y = growth),
              method = "gam", formula = y ~ s(x, bs = "cr", k = 15)) +
  scale_fill_viridis_c(option = "C") +
  facet_wrap(. ~ county, scales = "free_y")

# Test model --------------------------------------------------------------

library(mgcv)
library(emdbook)
library(scales)

niter <- 1000

set.seed(1234)
# Function to run the model
RE_fun_sim <- function(par, data = df) {
  df$D_est <- -sqrt((par[1] - df$lon)^2 + (par[2] - df$lat)^2)
  df$rho_est <- df$t + (1 / par[3]) * df$D_est
  
  -logLik.gam(gam(btb ~ s(rho_est, k = 12, bs = "tp"),
                  method = "ML",
                  data = df,
                  family = "poisson")
  )[1]
}

# Specifying the initial values
par_list_RE_sim <- c(400000, 5750000, 500)

# Recording start time of model run
start_time <- Sys.time()

# Running the model as in formal analysis
RE_sim_out <- metropSB(fn = RE_fun_sim, 
                       start = par_list_RE_sim, 
                       nmax = niter, 
                       retvals = TRUE,
                       retfreq = 1,
                       verbose = FALSE,
                       rptfreq = -1)

# Report total run time
Sys.time() - start_time

# Extract parameter space from SANN
RE_sim_out_df <- data.frame(RE_sim_out$retvals[,c(1:3, 10)])

# Convert negative log likelihood to -2lnL
RE_sim_out_df$val <- 2*RE_sim_out_df$val

# Create empty dataframe
RE_sim_profile <- data.frame(
  p1 = NA,
  p2 = NA,
  p3 = NA,
  val = NA
)

# For each unique speed considered by SANN
for(i in 1:length(unique(RE_sim_out_df$p3))) {
  
  # Store all unique values of speed
  match <- unique(RE_sim_out_df$p3)
  
  # Store all parameter combinations for a given speed considered
  temp <- subset(RE_sim_out_df, p3 == match[i])
  
  # Extract all parameter combinations where L is maximised
  temp1 <- temp[temp$val == max(temp$val),]
  
  # combine with previously created RE_sim_profile
  RE_sim_profile <- rbind(RE_sim_profile, temp1)
}

# Remove first NA entry when creating dataframe
RE_sim_profile <- RE_sim_profile[-1,]

# Find where -2lnL is best
lnLmax <- RE_sim_profile$val[RE_sim_profile$val == min(RE_sim_profile$val)]

# Calculate difference for each lnL from best
RE_sim_profile$lnL_diff <- RE_sim_profile$val - lnLmax

# For all parameter combinations, extract those that fall within chisq 95% CI
p3ci_profile_df <- RE_sim_profile[RE_sim_profile$lnL_diff < qchisq(p = 0.95, df = 1),]
p3min <- min(p3ci_profile_df$p3)
p3max <- max(p3ci_profile_df$p3)


# Repeat above, but for centroid coordinates
RE_sim_out_df$centroid <- paste(RE_sim_out_df$p1, RE_sim_out_df$p2, sep = " ")
RE_sim_profile <- data.frame(
  p1= NA,
  p2 = NA,
  p3 = NA,
  val = NA,
  centroid = NA
)
for(i in 1:length(unique(RE_sim_out_df$centroid))) { 
  match <- unique(RE_sim_out_df$centroid)
  temp <- subset(RE_sim_out_df, centroid == match[i])
  temp1 <- temp[temp$val == max(temp$val),]
  RE_sim_profile <- rbind(RE_sim_profile, temp1)
}
RE_sim_profile <- RE_sim_profile[-1,]
lnLmax <- RE_sim_profile$val[RE_sim_profile$val == min(RE_sim_profile$val)]
RE_sim_profile$lnL_diff <- RE_sim_profile$val - lnLmax

# Note that CI is based on 2 degrees of freedom from chisq
ci_profile_df <- RE_sim_profile[RE_sim_profile$lnL_diff < qchisq(p = 0.95, df = 2),]
p1min <- min(ci_profile_df$p1)
p1max <- max(ci_profile_df$p1)
p2min <- min(ci_profile_df$p2)
p2max <- max(ci_profile_df$p2)
RE_sim_est <- data.frame(
  Parameter = c("Epicentre X", "Epicentre Y", "Speed"),
  Units = c("mean centred UTM", "mean centred UTM", "m per day"),
  Estimate = comma(RE_sim_out$estimate),
  Lower95CI = comma(c(p1min, p2min, p3min)),
  Upper95CI = comma(c(p1max, p2max, p3max)),
  Initial = comma(par_list_RE_sim)
)
RE_sim_est


# Opt fig -----------------------------------------------------------------

RE_sim_out_df$iter <- as.numeric(row.names(RE_sim_out_df))
RE_sim_out_df$L <- RE_sim_out_df$val - min(RE_sim_out_df$val)

ggplot(RE_sim_out_df) +
  geom_path(aes(x = p1, y = p2),
            linetype = 2,
            colour = "grey") +
  geom_point(aes(x = p1, y = p2, group = iter),
             size = 2, colour = "grey") +
  geom_point(data = RE_sim_out_df[RE_sim_out_df$L <= qchisq(0.95, 2),],
             aes(x = p1, y = p2, colour = val, group = iter),
             size = 1.5) +
  scale_colour_viridis_c(option = "C") +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  labs(x = "X UTM (centered)",
       y = "Y UTM (centered)",
       colour = expression(paste(Delta, "-2lnL <= ", chi^{2}, "(0.95, 2)")))

ggplot(RE_sim_out_df) +
  geom_point(aes(x = p3, y = val, group = iter),
             size = 2, colour = "grey") +
  geom_point(data = RE_sim_out_df[RE_sim_out_df$L <= qchisq(0.95, 1),],
             aes(x = p3, y = val, colour = val, group = iter),
             size = 1.5) +
  scale_colour_viridis_c(option = "C") +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  labs(x = "Speed (m per day)",
       y = "-2lnL",
       title = "RE Speed",
       colour = expression(paste(Delta, "-2lnL <= ", chi^{2}, "(0.95, 1)")))


# Prediction --------------------------------------------------------------

df$D <- -sqrt((RE_sim_out$estimate[1] - df$lon)^2 + (RE_sim_out$estimate[2] - df$lat)^2)
df$rho <- df$t + (1 / RE_sim_out$estimate[3]) * df$D

RE_sim <- gam(btb ~ s(rho, k = 12, bs = "tp"),
              method = "ML",
              data = df,
              family = "poisson")

p_spatial_RE_sim <- expand.grid(x = seq(min(df$lon), 
                                        max(df$lon),
                                        length = 50),
                                y = seq(min(df$lat), 
                                        max(df$lat),
                                        length = 50),
                                t = seq(min(df$t), 
                                        max(df$t),
                                        length = 15)
)

p_spatial_RE_sim$D <- -sqrt((RE_sim_out$estimate[1] - p_spatial_RE_sim$x)^2 + 
                              (RE_sim_out$estimate[2] - p_spatial_RE_sim$y)^2)
p_spatial_RE_sim$rho <- p_spatial_RE_sim$t + 
  (1 / RE_sim_out$estimate[3]) * p_spatial_RE_sim$D

RE_fit <- exp(predict(RE_sim, newdata = p_spatial_RE_sim))

ind <- exclude.too.far(p_spatial_RE_sim$x, p_spatial_RE_sim$y,
                       df$lon, df$lat, dist = 0.1)

RE_fit[ind] <- NA

# RE_fit <- transform(RE_fit, 
#                     upper = fit + (2 * se.fit),
#                     lower = fit - (2 * se.fit))

pred_RE <- cbind(p_spatial_RE_sim, RE_fit)

ggplot() +
  geom_tile(data = pred_RE, aes(x = x, y = y, fill = RE_fit)) +
  scale_fill_viridis_c(option = "C", na.value = "transparent") +
  scale_x_continuous(label = scales::comma) +
  scale_y_continuous(label = scales::comma) +
  labs(x = "X",
       y = "Y",
       fill= expression(r[t]),
       subtitle = "Predicted spatial pattern over time") +
  theme_bw() +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
  facet_wrap(~ round(t))

plot(RE_sim)
