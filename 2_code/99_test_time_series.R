data_chron <- data_clean[
  twitter_created_at >= "2018-01-01"
  ][ , .(day_created = data.table::yday(twitter_created_at),
         year_created = data.table::year(twitter_created_at),
         weekday = as.factor(data.table::wday(twitter_created_at)))
     ][, n_tweets := .N, by = list(year_created, day_created)]

data_chron[, mav := data.table::frollmean(n_tweets, 30L), by = year_created]

quant <- quantile(data_chron$mav, 0.9, na.rm = TRUE)

data_chron[
  , upper_quartile := n_tweets - quant
  ][, is_peak := upper_quartile > 0L]

data.table::setcolorder(data_chron, c("year_created", "day_created"))

data_chron[year_created == 2019] %>%
  ggplot(aes(x = day_created, y = n_tweets)) +
  geom_line() +
  geom_line(mapping = aes(x = day_created, y = mav), col = 2L) +
  facet_wrap(~ year_created)

