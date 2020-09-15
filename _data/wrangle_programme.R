library(tidyverse)
library(hms)

zoom <- tribble(
  ~room, ~poster_id, ~zoom_link, ~expected,
  # room 1
  1, 7, "", "katriona",
  1, 6, "", "katriona",
  1, 18,"", "tugce",
  1, 5, "", "ioana",
  1, 17,"", "solon",
  1, 3, "", "akira",
  1, 13,"", "prateek",
  1, 14,"", "risa",
  1, 01,"", "anujan",
  # room 2
  2, 8, "", "keri",
  2, 10, "", "nikolas",
  2, 9, "", "lizhi",
  2, 15, "", "ryan",
  2, 4, "", "feargus",
  2, 11, "", "obi",
  2, 2, "", "daniele",
  2, 12, "", "pedro",
  2, NA, NA, NA
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
  mutate(time = as_hms(datetime)) %>%
  mutate(zoom_link = str_c(turing_zoom_url, zoom_link)) %>%
  mutate(poster_id = str_pad(as.character(poster_id), 2, "left", "0")) %>%
  pivot_wider(names_from = room, values_from = c(poster_id, zoom_link, expected)) %>%
  mutate(time = format(as.POSIXct(time), "%H:%M"))

write_csv(programme, "programme.csv")
