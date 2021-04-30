# ********************************************************** #
# ********** Google Trends Database Building Tool ********** #
# ********************************************************** #


source("gt_tools.r")

# This is a package for easy compilation of Google trends data.
# This package intends to do the following functions:
  # 1- Download long term daily or weekly data for a particular search quote.
  # 2- Correct long term daily or weekly data using long monthly data.
  # 3- Seasonal adjustment of gt series.

# Parameters Gtrends ----
keyword         <- "hotel"
country         <- "CO"
input.sdate     <- "2014-01-01"
input.edate     <- "2021-04-16"
input.frequency <- "w" # d:daily; w:weekly; m:monthly
input.delta     <- 3 # Num. months, (depend on freq selected) each window
input.ol.win    <- 1 # Num. months of overlapped periods
input.type      <- "web" # Web, news, images, youtube, froogle
input.categ     <- "0"

ini <- Sys.time()
gt2 <- long_gt(keyword = "hotel",country="CO",
                     input.sdate = as.Date("2010-01-01"),
                     input.edate = as.Date("2021-04-16"),
                     input.frequency = "d",input.delta = 6,
                     input.ol.win = 1)
fin <- Sys.time()

gt2 <- long_gt_ltc(keyword = "hotel",country="CO",
                      input.sdate = as.Date("2014-01-01"),
                      input.edate = as.Date("2021-04-16"),
                      input.frequency = "w",input.delta = 6,
                      input.ol.win = 1)








xser <- gt2$final
x = daily_sim(n=4)$original # series with length 4 years
xser.xts <- xts::xts(xser$gt_full,order.by = xser$date)
sa.day <- dsa(xser.xts, cval=7,automodel = "reduced")
plot(get_sa(sa.day))
plot(get_original(sa.day))
