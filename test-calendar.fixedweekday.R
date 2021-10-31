# Testing calendar.fixedweekday()

# Package install from the Github (the latest available version)
# remotes::install_github("palatej/rjd3toolkit",
#                  INSTALL_opts = "--no-multiarch", quiet = T)
# remotes::install_github("palatej/rjd3modelling",
#                  INSTALL_opts = "--no-multiarch", quiet = T)
# remotes::install_github("palatej/rjd3sa",
#                  INSTALL_opts = "--no-multiarch", quiet = T)

library(rjd3modelling)

cal_test <- calendar.new()
calendar.fixedday(calendar = cal_test, month = 1, day = 1)
calendar.fixedweekday(calendar = cal_test, month = 2, week = 2, dayofweek = 2)

hol_test <- holidays(calendar = cal_test, start = "2017-01-01",
                     length = as.Date("2020-01-01") - as.Date("2017-01-01"),
                     type = "All")

hol_test[hol_test == 1, , drop = F]
head(hol_test)
# There should be 2 columns for each holiday

wd_test <- htd(calendar = cal_test, frequency = 12,
               start = c(2017, 1), length = 3 * 12,
               groups = c(rep(1, 5), rep(0, 2)), contrasts = F)

window(wd_test, start = c(2017, 2), deltat = 1)
# There should be 9 holidays and 19 working days in February
