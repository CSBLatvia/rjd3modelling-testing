# Package install from the Github (the latest available version)
remotes::install_github("palatej/rjd3toolkit",
                 INSTALL_opts = "--no-multiarch", quiet = T)
remotes::install_github("palatej/rjd3modelling",
                 INSTALL_opts = "--no-multiarch", quiet = T)
remotes::install_github("palatej/rjd3sa",
                 INSTALL_opts = "--no-multiarch", quiet = T)

library(rjd3modelling)
library(data.table)

cal_test <- calendar.new()

calendar.fixedday(calendar = cal_test, month = 1, day = 1)
calendar.fixedday(calendar = cal_test, month = 1, day = 2,
                  start = "2018-01-01")
calendar.fixedday(calendar = cal_test, month = 1, day = 3,
                  start = "2018-01-01", end = "2018-12-31")

hol_test <- holidays(calendar = cal_test, start = "2017-01-01",
                     length = as.Date("2020-01-01") - as.Date("2017-01-01"),
                     type = "All") |> data.table(keep.rownames = T)

hol_test
hol_test[, holiday := max(.SD), .SDcols = patterns("V"), by = "rn"]
hol_test[holiday > 0]

if (hol_test[holiday > 0, .N] != 6L) stop("Should be 6 holidays")

