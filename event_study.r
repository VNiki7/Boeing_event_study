library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(broom)



library(readxl)
library(dplyr)

raw <- read_excel("adatok_1970_tõl.xlsx", sheet = "airlines+s&p500")
names(raw)

prices <- raw %>%
  transmute(
    date = as.Date(date, origin = "1899-12-30"),
    BA   = `BOEING close`,
    LUV  = `SOUTHWEST AIRLINES`,
    DAL  = `DELTA AIR LINES`,
    SP   = SP500
  )

date = as.Date(date, format = "%Y.%m.%d")

str(raw$`date...1`)
head(raw$`date...1`)

prices <- raw %>%
  transmute(
    date = as.Date(`date...1`, origin = "1899-12-30"),
    BA   = `BOEING close`,
    LUV  = `SOUTHWEST AIRLINES`,
    DAL  = `DELTA AIR LINES`,
    SP   = `S&P 500 COMPOSITE - PRICE INDEX`
  )
str(prices)
head(prices, 3)
summary(prices$date)

library(dplyr)
library(tidyr)

prices <- prices %>%
  arrange(date) %>%
  mutate(
    R_BA  = log(BA / lag(BA)),
    R_LUV = log(LUV / lag(LUV)),
    R_DAL = log(DAL / lag(DAL)),
    R_SP  = log(SP / lag(SP))
  ) %>%
  drop_na()
str(prices)

prices <- prices %>%
  mutate(
    BA  = as.numeric(BA),
    LUV = as.numeric(LUV),
    DAL = as.numeric(DAL),
    SP  = as.numeric(SP)
  )

prices <- prices %>%
  arrange(date) %>%
  mutate(
    R_BA  = log(BA / lag(BA)),
    R_LUV = log(LUV / lag(LUV)),
    R_DAL = log(DAL / lag(DAL)),
    R_SP  = log(SP / lag(SP))
  ) %>%
  drop_na()

summary(prices$R_BA)
summary(prices$R_SP)

events <- tibble::tibble(
  event = c(
    "Lion Air 610",
    "Ethiopian 302",
    "737 MAX grounding",
    "787 Dreamliner quality",
    "Alaska MAX 9 door plug"
  ),
  t0 = as.Date(c(
    "2018-10-29",
    "2019-03-10",
    "2019-03-13",
    "2021-05-06",
    "2024-01-05"
  )),
  type = c(
    "type_failure",
    "type_failure",
    "type_failure",
    "type_failure",
    "type_failure"
  )
)

event_window <- function(df, event_date, window = 10) {
  df %>%
    mutate(tau = as.numeric(date - event_date)) %>%
    filter(tau >= -window, tau <= window)
}

estimation <- prices %>%
  filter(date >= "2017-01-01", date <= "2018-09-30")

mkt_model <- lm(R_BA ~ R_SP, data = estimation)
summary(mkt_model)

prices <- prices %>%
  mutate(
    R_hat = predict(mkt_model, newdata = prices),
    AR = R_BA - R_hat
  )

library(dplyr)

window <- 10

event_results <- events %>%
  rowwise() %>%
  mutate(
    ev_data = list(event_window(prices, t0, window = window)),
    
    CAR_m1_p1 = sum(ev_data$AR[ev_data$tau >= -1 & ev_data$tau <= 1], na.rm = TRUE),
    CAR_m5_p5 = sum(ev_data$AR[ev_data$tau >= -5 & ev_data$tau <= 5], na.rm = TRUE),
    CAR_m10_p10 = sum(ev_data$AR, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(event, t0, CAR_m1_p1, CAR_m5_p5, CAR_m10_p10)

event_results

library(dplyr)
library(tidyr)

window <- 10

event_curves <- events %>%
  rowwise() %>%
  mutate(
    ev_data = list(
      event_window(prices, t0, window = window) %>%
        arrange(tau) %>%
        mutate(CAR = cumsum(AR))
    )
  ) %>%
  ungroup() %>%
  select(event, ev_data) %>%
  unnest(ev_data)


library(ggplot2)

ggplot(event_curves, aes(x = tau, y = CAR, color = event)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Nap az eseményhez képest",
    y = "Kumulált abnormális hozam (CAR)",
    title = "Boeing incidensek: CAR görbék (-10;+10 nap)",
    color = "Esemény"
  ) +
  theme_minimal()


library(dplyr)
library(tidyr)
library(ggplot2)

# 1) Estimation window 
estimation <- prices %>%
  filter(date >= as.Date("2017-01-01"), date <= as.Date("2018-09-30"))

# 2) Market model becslések 
mkt_BA  <- lm(R_BA  ~ R_SP, data = estimation)
mkt_LUV <- lm(R_LUV ~ R_SP, data = estimation)
mkt_DAL <- lm(R_DAL ~ R_SP, data = estimation)

# 3) AR számítás mindháromra
prices2 <- prices %>%
  mutate(
    Rhat_BA  = predict(mkt_BA,  newdata = prices),
    Rhat_LUV = predict(mkt_LUV, newdata = prices),
    Rhat_DAL = predict(mkt_DAL, newdata = prices),
    
    AR_BA  = R_BA  - Rhat_BA,
    AR_LUV = R_LUV - Rhat_LUV,
    AR_DAL = R_DAL - Rhat_DAL
  )

window <- 10

spill_tbl <- events %>%
  rowwise() %>%
  mutate(
    ev = list(event_window(prices2, t0, window = window)),
    
    # Boeing CAR
    BA_CAR_m1_p1   = sum(ev$AR_BA[ev$tau >= -1  & ev$tau <= 1],  na.rm = TRUE),
    BA_CAR_m5_p5   = sum(ev$AR_BA[ev$tau >= -5  & ev$tau <= 5],  na.rm = TRUE),
    BA_CAR_m10_p10 = sum(ev$AR_BA, na.rm = TRUE),
    
    # Southwest (LUV) CAR
    LUV_CAR_m1_p1   = sum(ev$AR_LUV[ev$tau >= -1 & ev$tau <= 1], na.rm = TRUE),
    LUV_CAR_m5_p5   = sum(ev$AR_LUV[ev$tau >= -5 & ev$tau <= 5], na.rm = TRUE),
    LUV_CAR_m10_p10 = sum(ev$AR_LUV, na.rm = TRUE),
    
    # Delta (DAL) CAR
    DAL_CAR_m1_p1   = sum(ev$AR_DAL[ev$tau >= -1 & ev$tau <= 1], na.rm = TRUE),
    DAL_CAR_m5_p5   = sum(ev$AR_DAL[ev$tau >= -5 & ev$tau <= 5], na.rm = TRUE),
    DAL_CAR_m10_p10 = sum(ev$AR_DAL, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(event, t0,
         BA_CAR_m1_p1, BA_CAR_m5_p5, BA_CAR_m10_p10,
         LUV_CAR_m1_p1, LUV_CAR_m5_p5, LUV_CAR_m10_p10,
         DAL_CAR_m1_p1, DAL_CAR_m5_p5, DAL_CAR_m10_p10)

spill_tbl
print(spill_tbl, width = Inf)

luv_curves <- events %>%
  rowwise() %>%
  mutate(
    ev = list(
      event_window(prices2, t0, window = window) %>%
        arrange(tau) %>%
        transmute(tau, event, CAR = cumsum(AR_LUV))
    )
  ) %>%
  ungroup() %>%
  select(ev) %>%
  unnest(ev)

ggplot(luv_curves, aes(x = tau, y = CAR, color = event)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Nap az eseményhez képest",
    y = "Kumulált abnormális hozam (CAR)",
    title = "Southwest (LUV): CAR görbék (???10+10 nap)",
    color = "Esemény"
    
  ) +
  theme_minimal()


dal_curves <- events %>%
  rowwise() %>%
  mutate(
    ev = list(
      event_window(prices2, t0, window = window) %>%
        arrange(tau) %>%
        transmute(tau, event, CAR = cumsum(AR_DAL))
    )
  ) %>%
  ungroup() %>%
  select(ev) %>%
  unnest(ev)

ggplot(dal_curves, aes(x = tau, y = CAR, color = event)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Nap az eseményhez képest",
    y = "Kumulált abnormális hozam (CAR)",
    title = "Delta (DAL): CAR görbék (???10+10 nap)",
    color = "Esemény"
  ) +
  theme_minimal()


combo_curves <- events %>%
  rowwise() %>%
  mutate(
    ev = list(
      event_window(prices2, t0, window = window) %>%
        arrange(tau) %>%
        transmute(
          tau,
          event,
          BA  = cumsum(AR_BA),
          LUV = cumsum(AR_LUV),
          DAL = cumsum(AR_DAL)
        ) %>%
        pivot_longer(cols = c(BA, LUV, DAL), names_to = "company", values_to = "CAR")
    )
  ) %>%
  ungroup() %>%
  select(ev) %>%
  unnest(ev)

ggplot(combo_curves, aes(x = tau, y = CAR, color = company)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ event, ncol = 2, scales = "free_y") +
  labs(
    x = "Nap az eseményhez képest",
    y = "CAR",
    title = "Spillover: Boeing vs Southwest vs Delta (???10.+10 nap)",
    color = "vállalat"
  ) +
  theme_minimal()

library(dplyr)
library(lubridate)

prices_monthly <- prices %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    BA = last(BA),
    SP = last(SP)
  ) %>%
  ungroup() %>%
  arrange(month)

prices_monthly <- prices_monthly %>%
  mutate(
    R_BA = log(BA / lag(BA)),
    R_SP = log(SP / lag(SP))
  ) %>%
  drop_na()

mkt_model_m <- lm(R_BA ~ R_SP, data = prices_monthly)
summary(mkt_model_m)

prices_monthly <- prices_monthly %>%
  mutate(
    R_hat = predict(mkt_model_m, newdata = prices_monthly),
    AR = R_BA - R_hat
  )

events_other <- tibble::tibble(
  event = c(
    "1977 Boeing 707",
    "1982 Microburst 727",
    "1985 JAL 747SR",
    "1996 TWA 800",
    "2009 Boeing 707",
    "2013 Boeing 737"
  ),
  t0 = as.Date(c(
    "1977-05-17",
    "1982-07-09",
    "1985-08-12",
    "1996-07-17",
    "2009-10-21",
    "2013-11-17"
  )),
  type = "other"
)

event_window_m <- function(df, event_date, window = 3) {
  df %>%
    mutate(
      tau = interval(event_date, month) %/% months(1)
    ) %>%
    filter(tau >= -window, tau <= window)
}

window_m <- 3

car_other <- events_other %>%
  rowwise() %>%
  mutate(
    ev = list(event_window_m(prices_monthly, t0, window = window_m)),
    CAR_m3_p3 = sum(ev$AR, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(event, t0, CAR_m3_p3)

car_other

mean_other <- car_other %>%
  summarise(mean_CAR_other = mean(CAR_m3_p3, na.rm = TRUE))

mean_type <- event_results %>%
  summarise(mean_CAR_type = mean(CAR_m10_p10, na.rm = TRUE))

comparison_tbl <- tibble::tibble(
  Event_Type = c("Típushiba (2018-2024, napi)", "Balszerencse (1977-2017, havi)"),
  Average_CAR = c(mean_type$mean_CAR_type, mean_other$mean_CAR_other)
)

comparison_tbl

summary_other <- car_other %>%
  summarise(
    mean_CAR   = mean(CAR_m3_p3, na.rm = TRUE),
    median_CAR = median(CAR_m3_p3, na.rm = TRUE)
  )

summary_other


summary_other <- car_other %>%
  summarise(
    mean_CAR   = mean(CAR_m3_p3, na.rm = TRUE),
    median_CAR = median(CAR_m3_p3, na.rm = TRUE),
    sd_CAR     = sd(CAR_m3_p3, na.rm = TRUE)
  )

event_pres <- event_results %>%
  mutate(
    presidency = case_when(
      t0 >= as.Date("2009-01-20") & t0 < as.Date("2017-01-20") ~ "Obama",
      t0 >= as.Date("2017-01-20") & t0 < as.Date("2021-01-20") ~ "Trump",
      t0 >= as.Date("2021-01-20")                              ~ "Biden"
    )
  )

event_pres

pres_summary <- event_pres %>%
  group_by(presidency) %>%
  summarise(
    n_events   = n(),
    mean_CAR   = mean(CAR_m10_p10, na.rm = TRUE),
    median_CAR = median(CAR_m10_p10, na.rm = TRUE),
    sd_CAR     = sd(CAR_m10_p10, na.rm = TRUE)
  )

pres_summary


