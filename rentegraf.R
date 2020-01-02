library(tidyverse)
library(gganimate)
library(lubridate)

# Collect information from Norges Bank.There are a number of issues with the file, for example;
# There are 4 types of NAs: <blank>, NA, ND, NC.
# Dates are written as 2-Jan-86.
# There are a lot of blanks in the beginning, thus readr must receive help with column specifications

renter <- readr::read_csv2("https://www.norges-bank.no/globalassets/marketdata/stat/no/renter/v2/renter_dag.sdv", 
                   col_types = cols(
                      DATES = col_character(),
                      FOLIO.NOM = col_double(),
                      RESERVE.NOM = col_double(),
                      DLAAN.NOM = col_double(),
                      STATSVKL.3M.EFF = col_double(),
                      STATSVKL.6M.EFF = col_double(),
                      STATSVKL.9M.EFF = col_double(),
                      STATSVKL.12M.EFF = col_double(),
                      STATSOBL.3Y.EFF = col_double(),
                      STATSOBL.5Y.EFF = col_double(),
                      STATSOBL.10Y.EFF = col_double(),
                      NOWA.RATE = col_double(),
                      NOWA.VOLUME = col_double(),
                      NOWA.QUALIFIER = col_character()
                    ),
                    na = c("", "NA", "ND", "NC"))

# Fix dates using lubridate

renter$DATES <- lubridate::dmy(renter$DATES)

# We could have skipped importing some of the colums, but let's focus on govmt yields
# Rearrange to tidy frame (long)

rent_stat <- renter %>%
  select(DATES, 
         STATSVKL.3M.EFF: STATSOBL.10Y.EFF) %>%
  gather(key = "Period", value = "Yield", -DATES)

# create matching table to have periods in number of months

matching <- data.frame(Period = unique(rent_stat$Period),
                       Period_month = as.integer(c("3", "6", "9", "12", "36", "60", "120")))

# Match with rent_stat, calculate some stats, remove blank values only keeping those with values across curve

rent_stat <- rent_stat %>% 
  left_join(matching, by = "Period") %>%
  group_by(DATES) 
  

# plot yields since the beginning

rent_stat %>%
  ggplot(aes(x = DATES, y = Yield, color = Period)) + 
  geom_line() + 
  theme_bw()

ggsave("yield.png")

# gganimate 


anim <- rent_stat %>% 
  ggplot(aes(x = Period_month, y = Yield)) +
  geom_point(size = 5, color = "#728FB2", alpha = 0.6) + 
  theme_bw() + 
  transition_time(DATES) +
  ggtitle("NO Yield curve {frame_time}")



animate(anim, duration = 10, fps = 20, width = 500, height = 500, renderer = gifski_renderer())

anim_save("yield.gif")

