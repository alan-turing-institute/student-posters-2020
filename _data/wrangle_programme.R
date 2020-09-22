library(tidyverse)
library(hms)

zoom <- tribble(
  ~room, ~poster_id, ~zoom_link, ~video_link, ~expected,
  # room 1
  1, 7, "93411880880", NA, "katriona",
  1, 6, "98997146140", NA, "katriona",
  1, 18,"93941631356", NA, "tugce",
  1, 5, "95880984986", NA, "ioana",
  1, 17,"99916467421", NA, "solon",
  1, 3, "91621172306", NA, "akira",
  1, 13,"94564694583", NA, "prateek",
  1, 14,"94365127915", NA, "risa",
  # 1, 01,"94990403140", NA, "anujan",
  # room 2
  2, 8, "93009754487", NA, "keri",
  2, 10, "99777001117", NA, "nikolas",
  2, 9, "92927994244", NA, "lizhi",
  2, 15, "94514516679", NA, "ryan",
  2, 4, "98703863600", NA, "feargus",
  2, 11, "92929385919", NA, "obi",
  2, 2, "98190487545", NA, "daniele",
  2, 12, "94586862227", NA, "pedro",
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
  mutate(has_video = !is.na(video_link)) %>%
  pivot_wider(names_from = room,
              values_from = c(poster_id, zoom_link, expected, video_link, has_video)) %>%
  mutate(time = format(as.POSIXct(time), "%H:%M"))

write_csv(programme, "programme.csv")
