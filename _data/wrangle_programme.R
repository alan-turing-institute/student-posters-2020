library(tidyverse)
library(hms)

zoom <- tribble(
  ~room, ~poster_id, ~zoom_link,
  # room 1
  1, 7, "93411880880",
  1, 6, "98997146140",
  1, 18,"93941631356",
  1, 5, "95880984986",
  1, 17,"99916467421",
  1, 3, "91621172306",
  1, 13,"94564694583",
  1, 14,"94365127915",
  # 1, 01,"94990403140",
  # room 2
  2, 8, "93009754487",
  2, 10, "99777001117",
  2, 9, "92927994244",
  2, 15, "94514516679",
  2, 4, "98703863600",
  2, 11, "92929385919",
  2, 2, "98190487545",
  2, 12, "94586862227",
  # 2, NA, NA, NA, NA
)

turing_zoom_url <- "https://turing-uk.zoom.us/j/"

showcase <- lubridate::ymd("2020-09-24") %>% lubridate::as_datetime()

start_hour <- 10
time_slots <- zoom %>%
  filter(room == 1) %>% summarise(total = n()) %>% pull(total)

programme <-
  zoom %>%
  group_by(room) %>%
  mutate(datetime = seq(
    from = showcase + hms(hours = start_hour),
    to = showcase + hms(hours = start_hour + time_slots - 1),
    by = "1 hour")
  ) %>%
  mutate(time = as_hms(datetime), .keep = "unused") %>%
  mutate(zoom_link = str_c(turing_zoom_url, zoom_link)) %>%
  mutate(poster_id = str_pad(as.character(poster_id), 2, "left", "0")) %>%
  pivot_wider(names_from = room,
              values_from = c(poster_id, zoom_link)) %>%
  mutate(time = format(as.POSIXct(time), "%H:%M"))

write_csv(programme, "programme.csv")
