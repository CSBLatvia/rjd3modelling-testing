# Testing the generation of calendar regressors by the rjd3modelling package
# https://github.com/palatej/rjd3modelling

# Tutorial how to generate calendar regressors is available at:
# https://aqlt-formation-rte.netlify.app/tp/enonces/r-2-cjo_solution

# This is a test of calendar regressors for Latvia
# The calendar regressors used in Latvia are available:
# https://github.com/CSBLatvia/SA-cal-reg-LV

# Reset
rm(list = ls())
gc()

# Options
options(width = 80)
options(max.print = 10e3)
options(encoding = "UTF-8")
options(stringsAsFactors = FALSE)
options(datatable.integer64 = "character")
options(datatable.keepLeadingZeros = TRUE)
options(datatable.logical01 = TRUE)

# Package install from the Github (the latest available version)
remotes::install_github("palatej/rjd3toolkit",
                 INSTALL_opts = "--no-multiarch", quiet = T)
remotes::install_github("palatej/rjd3modelling",
                 INSTALL_opts = "--no-multiarch", quiet = T)
remotes::install_github("palatej/rjd3sa",
                 INSTALL_opts = "--no-multiarch", quiet = T)

# Packages
library(data.table)
library(ggplot2)
library(rjd3modelling)


# Load the current number of workingdays
uri <- "https://raw.githubusercontent.com/CSBLatvia/SA-cal-reg-LV/master/data"


# Workdays
wd_count_curr <- fread(file.path(uri, "count_WD_long.csv"))
wd_count_curr[, .N, keyby = .(Period)]


# Load the current Latvian calendar regressors
reg_names <- c("TD_M", "TD_Q", "WD_M", "WD_Q")
file_uris <- file.path(uri, paste0(reg_names, ".csv"))
names(file_uris) <- reg_names
file_uris

cal_reg_curr <- lapply(file_uris, fread) |>
  lapply(melt.data.table, id.vars = "date", variable.factor = F) |>
  rbindlist(idcol = "specification")

cal_reg_curr[, date := as.IDate(date)]

weekday_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                   "Saturday", "Sunday")

cal_reg_curr[, variable := factor(variable, c(weekday_names[1:6],
                                              "WorkingDays", "LeapYear"))]

dcast.data.table(data = cal_reg_curr, formula = variable ~ specification,
                 fun.aggregate = length)



# Generate calendar regressors with the rjd3modelling
calendar_lv <- calendar.new()

# 1) New Years day
calendar.fixedday(calendar = calendar_lv, month = 1, day = 1)

# 2) Labour Day
calendar.fixedday(calendar = calendar_lv, month = 5, day = 1)

# 3) Restauration of Independance
calendar.fixedday(calendar = calendar_lv, month = 5, day = 4,
                  start = "2002-01-01")
# This is compensable holiday since 2008
# If May 4 is Saturday or Sunday then the next Monday is a holiday
# in other words extra holidays are
# May 5, if it is a Monday (May 4 is a Sunday)
# May 6, if it is a Monday (May 4 is a Saturday)

# 2008-05-05
# 2013-05-06
# 2014-05-05
# 2019-05-06

# calendar.fixedday(calendar = calendar_lv, month = 5, day = 5,
#                   start = "2008-01-01", end = "2008-12-31") # 4
# calendar.fixedday(calendar = calendar_lv, month = 5, day = 6,
#                   start = "2013-01-01", end = "2013-12-31") # 5
# calendar.fixedday(calendar = calendar_lv, month = 5, day = 5,
#                   start = "2014-01-01", end = "2014-12-31") # 6
# calendar.fixedday(calendar = calendar_lv, month = 5, day = 6,
#                   start = "2019-01-01", end = "2019-12-31") # 7



# 8) Mothers day (always Sunday)
calendar.fixedweekday(calendar = calendar_lv, month = 5, week = 2,
                      dayofweek = 7)

# 9 & 10) Midsummer eve & day
calendar.fixedday(calendar = calendar_lv, month = 6, day = 23)
calendar.fixedday(calendar = calendar_lv, month = 6, day = 24)

# # 11) Closing day of the General Latvian Song and Dance Celebration
# calendar.fixedweekday(calendar = calendar_lv, month = 7, week = 1,
#                       dayofweek = 7,
#                       start = "2018-01-01", end = "2018-12-31") # 2018-07-08
# # 12) Compensation because the closing day was on Sunday
# calendar.fixedweekday(calendar = calendar_lv, month = 7, week = 2,
#                       dayofweek = 1,
#                       start = "2018-01-01", end = "2018-12-31") # 2018-07-09

# It looks calendar.fixedweekday does not work
# calendar.fixedday(calendar = calendar_lv, month = 7, day = 8,
#                   start = "2018-01-01", end = "2018-12-31")
# calendar.fixedday(calendar = calendar_lv, month = 7, day = 9,
#                   start = "2018-01-01", end = "2018-12-31")

# This is a holiday since 2014 with compensation
# General Latvian Song and Dance Celebration happens once in 5 years.
# There is not a known scheme how the date of the closing day is chosen
# Hower in most cases it is on the Sunday of the first week of July
# Since 1993 the only exception was in 2008 when the closing day was 2008-07-12
# which was a Saturday of the 2nd week of July
# However it is not fixed and can be changed according to the situation


# Visit by Pope Francis (one time event)
# calendar.fixedday(calendar = calendar_lv, month = 9, day = 24,
#                   start = "2018-09-01", end = "2018-12-31")


# Proclamation Day of the Republic of Latvia
calendar.fixedday(calendar = calendar_lv, month = 11, day = 18)
# This is compensable holiday since 2007
# If November 18 is Saturday or Sunday then the next Monday is a holiday
# in other words extra holidays are
# November 19, if it is a Monday (November 18 is a Sunday)
# November 20, if it is a Monday (November 18 is a Saturday)

# 2007-11-19
# 2012-11-19
# 2017-11-20
# 2018-11-19

# calendar.fixedday(calendar = calendar_lv, month = 11, day = 19,
#                   start = "2007-01-01", end = "2007-12-31")
# calendar.fixedday(calendar = calendar_lv, month = 11, day = 19,
#                   start = "2012-01-01", end = "2012-12-31")
# calendar.fixedday(calendar = calendar_lv, month = 11, day = 20,
#                   start = "2017-01-01", end = "2017-12-31")
# calendar.fixedday(calendar = calendar_lv, month = 11, day = 19,
#                   start = "2018-01-01", end = "2018-12-31")


# Christmas & New Years eve
calendar.fixedday(calendar = calendar_lv, month = 12, day = 24,
                  start = "2007-01-01") # Christmas eve
calendar.fixedday(calendar = calendar_lv, month = 12, day = 25) # Christmas
calendar.fixedday(calendar = calendar_lv, month = 12, day = 26) # Boxing Day
calendar.fixedday(calendar = calendar_lv, month = 12, day = 31) # New Years eve


# Easter related holidays
calendar.easter(calendar = calendar_lv, offset = -2) # Good Friday
calendar.easter(calendar = calendar_lv, offset =  0) # Easter (Sunday)
calendar.easter(calendar = calendar_lv, offset =  1,
                start = "1997-01-01") # Easter Monday
calendar.easter(calendar = calendar_lv, offset = 49,
                start = "1996-01-01") # Pentacost (Sunday)



# Holidays

cal_reg_curr[, range(date)]

start_date <- cal_reg_curr[, min(date)]
length_reg <- cal_reg_curr[, max(date) - min(date) + 31]

dat_cal <- holidays(calendar = calendar_lv,
                    start = start_date, length = length_reg,
                    type = "All") |> data.table(keep.rownames = T)

setnames(dat_cal, "rn", "date")
dat_cal[, date := as.IDate(date)]

x <- grep("V", names(dat_cal), value = T)

dat_cal[, length(unique(year(date)))]
dat_cal[, lapply(.SD, sum), .SDcols = x]

dat_cal[, holiday := max(.SD), .SDcols = x, by = .(date)]
dat_cal[, .N, keyby = .(holiday)]

dcast.data.table(data = dat_cal[between(date, "2018-01-01", "2018-12-31")],
                 formula = year(date) + month(date) ~ mday(date),
                 fun.aggregate = sum, value.var = "holiday")

# dat_cal[, c(x) := NULL]

dat_cal[, Year := year(date)]
dat_cal[, Period_y := "y"]
dat_cal[, Period_q := paste0("q", (month(date) + 2) %/% 3)]
dat_cal[, Period_m := paste0("m", sprintf("%02d", month(date)))]

dat_cal[, .N, keyby = .(Period_y, Period_q, Period_m)]

# Dziesmu svētku pārbaude
melt.data.table(data = dat_cal, id.vars = "date", measure.vars = x)[value == 1]
melt.data.table(data = dat_cal[Year == 2018],
                id.vars = "date", measure.vars = x)[value == 1]
dat_cal[Year == 2018 & Period_m == "m07", sum(holiday)]

dat_y <- dat_cal[, .(days = .N, h_days = sum(holiday)),
                 keyby = .(Year, Period_y)]



# Workdays

start_date <- cal_reg_curr[, min(date)]
end_date <-   cal_reg_curr[, max(date)]

start_date_array <- c(year(start_date), 1)

length_m <- (year(end_date) - year(start_date) + 1) * 12
length_q <- length_m / 3
length_y <- length_m / 12

wd_count_curr

dat_wd_m <- htd(calendar = calendar_lv, frequency = 12,
                start = start_date_array,
                length = length_m, groups = c(rep(1, 5), rep(0, 2)),
                contrasts = F)
dat_wd_q <- htd(calendar = calendar_lv, frequency = 4,
                start = start_date_array,
                length = length_q, groups = c(rep(1, 5), rep(0, 2)),
                contrasts = F)
dat_wd_y <- htd(calendar = calendar_lv, frequency = 1,
                start = start_date_array,
                length = length_y, groups = c(rep(1, 5), rep(0, 2)),
                contrasts = F)

dat_wd_m[, 2]
dat_wd_q[, 2]
dat_wd_y[, 2]

dat_wd_m <- as.data.table(dat_wd_m)
dat_wd_q <- as.data.table(dat_wd_q)
dat_wd_y <- as.data.table(dat_wd_y)

dat_wd_m[, date := seq(from = start_date, to = end_date, by = "month")]
dat_wd_q[, date := seq(from = start_date, to = end_date, by = "quarter")]
dat_wd_y[, date := seq(from = start_date, to = end_date, by = "year")]

dat_wd <- rbindlist(list(m = dat_wd_m, q = dat_wd_q, y = dat_wd_y),
                    idcol = "per")

setnames(dat_wd, c("group-0", "group-1"), c("HD", "WD"))

dat_wd[, Year := year(date)]

dat_wd[per == "m", Period := paste0("m", sprintf("%02d", month(date)))]
dat_wd[per == "q", Period := paste0("q", quarter(date))]
dat_wd[per == "y", Period := "y"]

dat_wd[per == "m"]
dat_wd[per == "q"]
dat_wd[per == "y"]

dat_wd_test <- merge(wd_count_curr, dat_wd, by = c("Year", "Period"),
                     suffixes = c("_curr", "_rjd3"))

dat_wd_test[per == "m" & WD_curr != WD_rjd3,
            .(Year, Period, WD_curr, WD_rjd3,
              diff = WD_rjd3 - WD_curr)][order(Period, Year)]

# stop()

# Calendar regressors

cal_reg_curr

cal_reg_curr[abs(value) > 1e-10][variable == "LeapYear",
                                 .N, keyby = .(specification, value)]

cal_reg_curr[, sum(value), keyby = .(specification, date)]

reg_names



# Monthly ####

## Leap year (monthly)
lp_m <- lp.variable(frequency = 12, start = start_date_array, length = length_m)
# str(lp_m)
# plot(lp_m)

## TD_M
td_m <- htd(calendar = calendar_lv, frequency = 12,
            start = start_date_array,
            length = length_m)
# str(td_m)
# plot(td_m)

### Add Leap Year
td_m <- ts.union(td_m, lp_m) |> data.table()

setnames(td_m, c(weekday_names[1:6], "LeapYear"))

td_m[, specification := "TD_M"]
td_m[, date := seq(from = start_date, to = end_date, by = "month")]

td_m <- melt.data.table(data = td_m, id.vars = c("specification", "date"),
                        variable.factor = F)

# ggplot(data = td_m, mapping = aes(x = date, y = value)) +
#   geom_line() +
#   facet_wrap(facets = vars(variable)) +
#   theme_bw()


## WD_M
wd_m <- htd(calendar = calendar_lv, frequency = 12,
            start = start_date_array,
            length = length_m, groups = c(rep(1, 5), rep(0, 2)))
# str(wd_m)
# plot(wd_m)

### Add Leap Year
wd_m <- ts.union(wd_m, lp_m) |> data.table()

setnames(wd_m, c("WorkingDays", "LeapYear"))

wd_m[, specification := "WD_M"]
wd_m[, date := seq(from = start_date, to = end_date, by = "month")]

wd_m <- melt.data.table(data = wd_m, id.vars = c("specification", "date"),
                        variable.factor = F)

# ggplot(data = wd_m, mapping = aes(x = date, y = value)) +
#   geom_line() +
#   facet_wrap(facets = vars(variable)) +
#   theme_bw()


# Quarterly

## Leap year (quarterly)
lp_q <- lp.variable(frequency = 4, start = start_date_array, length = length_q)
# str(lp_q)
# plot(lp_q)

## TD_Q
td_q <- htd(calendar = calendar_lv, frequency = 4,
            start = start_date_array,
            length = length_q)
# str(td_q)
# plot(td_q)

### Add Leap Year
td_q <- ts.union(td_q, lp_q) |> data.table()

setnames(td_q, c(weekday_names[1:6], "LeapYear"))

td_q[, specification := "TD_Q"]
td_q[, date := seq(from = start_date, to = end_date, by = "quarter")]

td_q <- melt.data.table(data = td_q, id.vars = c("specification", "date"),
                        variable.factor = F)

# ggplot(data = td_q, mapping = aes(x = date, y = value)) +
#   geom_line() +
#   facet_wrap(facets = vars(variable)) +
#   theme_bw()


## WD_q
wd_q <- htd(calendar = calendar_lv, frequency = 4,
            start = start_date_array,
            length = length_q, groups = c(rep(1, 5), rep(0, 2)))
# str(wd_q)
# plot(wd_q)

### Add Leap Year
wd_q <- ts.union(wd_q, lp_q) |> data.table()

setnames(wd_q, c("WorkingDays", "LeapYear"))

wd_q[, specification := "WD_Q"]
wd_q[, date := seq(from = start_date, to = end_date, by = "quarter")]

wd_q <- melt.data.table(data = wd_q, id.vars = c("specification", "date"),
                        variable.factor = F)

# ggplot(data = wd_q, mapping = aes(x = date, y = value)) +
#   geom_line() +
#   facet_wrap(facets = vars(variable)) +
#   theme_bw()



cal_reg_rjd3 <- rbindlist(list(td_m, wd_m, td_q, wd_q))

cal_reg_rjd3[, variable := factor(variable, c(weekday_names[1:6],
                                              "WorkingDays", "LeapYear"))]



cal_reg <- merge(x = cal_reg_curr, y = cal_reg_rjd3,
                 by = c("specification", "date", "variable"),
                 suffixes = c("_curr", "_rjd3"))
cal_reg[, diff := value_rjd3 - value_curr]

cal_reg <- melt.data.table(data = cal_reg,
                           id.vars = c("specification", "date", "variable"),
                           variable.name = "method", variable.factor = F)


pl_cal_reg_curr <- ggplot(data = cal_reg[method == "value_curr"],
                          mapping = aes(x = date, y = value)) +
  geom_line() +
  scale_x_date(date_minor_breaks = "years") +
  facet_grid(rows = vars(specification), cols = vars(variable),
             scales = "free_y") +
  ggtitle("Calendar regressors for Latvia (current)") +
  theme_bw() +
  theme(legend.position = "top")

pl_cal_reg_rjd3 <- ggplot(data = cal_reg[method == "value_rjd3"],
                          mapping = aes(x = date, y = value)) +
  geom_line() +
  scale_x_date(date_minor_breaks = "years") +
  facet_grid(rows = vars(specification), cols = vars(variable),
             scales = "free_y") +
  ggtitle("Calendar regressors for Latvia (rjd3modelling)") +
  theme_bw() +
  theme(legend.position = "top")

pl_cal_reg_both <- ggplot(data = cal_reg[method != "diff"],
                          mapping = aes(x = date, y = value,
                                        colour = method)) +
  geom_line() +
  scale_x_date(date_minor_breaks = "years") +
  facet_grid(rows = vars(specification), cols = vars(variable),
             scales = "free_y") +
  ggtitle("Calendar regressors for Latvia (current & rjd3modelling)") +
  theme_bw() +
  theme(legend.position = "top")

pl_cal_reg_diff <- ggplot(data = cal_reg[method == "diff"],
                          mapping = aes(x = date, y = value)) +
  geom_line() +
  scale_x_date(date_minor_breaks = "years") +
  facet_grid(rows = vars(specification), cols = vars(variable),
             scales = "free_y") +
  ggtitle("Difference") +
  theme_bw() +
  theme(legend.position = "top")


pl_cal_td_m <- ggplot(data = cal_reg[specification == "TD_M"],
                      mapping = aes(x = date, y = value)) +
  geom_line() +
  scale_x_date(date_minor_breaks = "years") +
  facet_grid(rows = vars(method), cols = vars(variable)) +
  ggtitle("TD_M") +
  theme_bw() +
  theme(legend.position = "top")

pl_cal_wd_m <- ggplot(data = cal_reg[specification == "WD_M"],
                      mapping = aes(x = date, y = value)) +
  geom_line() +
  scale_x_date(date_minor_breaks = "years") +
  facet_grid(rows = vars(method), cols = vars(variable)) +
  ggtitle("WD_M") +
  theme_bw() +
  theme(legend.position = "top")

pl_cal_td_q <- ggplot(data = cal_reg[specification == "TD_Q"],
                      mapping = aes(x = date, y = value)) +
  geom_line() +
  scale_x_date(date_minor_breaks = "years") +
  facet_grid(rows = vars(method), cols = vars(variable)) +
  ggtitle("TD_Q") +
  theme_bw() +
  theme(legend.position = "top")

pl_cal_wd_q <- ggplot(data = cal_reg[specification == "WD_Q"],
                      mapping = aes(x = date, y = value)) +
  geom_line() +
  scale_x_date(date_minor_breaks = "years") +
  facet_grid(rows = vars(method), cols = vars(variable)) +
  ggtitle("WD_Q") +
  theme_bw() +
  theme(legend.position = "top")


# Zoom

dat <- cal_reg[specification == "WD_M" & variable == "WorkingDays" &
                 method == "diff"]

dat[, lab := format(date, format = "%Y %b")]
dat[value < (max(value) - min(value)) / 2, lab := paste("--", lab)]
dat[value > (max(value) - min(value)) / 2, lab := paste(lab, "--")]

pl_cal_wd_m_diff <- ggplot(data = dat,
                      mapping = aes(x = date, y = value)) +
  geom_point(mapping = aes(colour = factor(month(date)))) +
  geom_text(mapping = aes(label = lab),
            data = dat[abs(value) > 0.1],
            hjust = "inward", angle = 90, size = 3) +
  scale_y_continuous(breaks = -99:99) +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  ggtitle("WD_M_diff") +
  theme_bw() +
  theme(legend.position = "none")
pl_cal_wd_m_diff

ggsave(filename = "cal_wd_m_diff.pdf", plot = pl_cal_wd_m_diff,
       width = 16, height = 9, units = "in")


# Save

pdf(file = "cal_reg_LV.pdf", width = 16, heigh = 9)
print(pl_cal_reg_curr)
print(pl_cal_reg_rjd3)
print(pl_cal_reg_both)
print(pl_cal_reg_diff)
print(pl_cal_td_m)
print(pl_cal_wd_m)
print(pl_cal_td_q)
print(pl_cal_wd_q)
print(pl_cal_wd_m_diff)
dev.off()
