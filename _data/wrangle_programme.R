library(tidyverse)
library(hms)

zoom <- tribble(
  ~showcase, ~room, ~time, ~poster_id, ~zoom_link, ~expected,
  1, 1, hms(hours = 10), 7, "https://turing-uk.zoom.us/j/98997146140", "katriona",
  1, 1, hms(hours = 11), 6, "https://turing-uk.zoom.us/j/93411880880", "katriona",
  1, 1, hms(hours = 14), 1, "https://turing-uk.zoom.us/j/94990403140", "anujan",
  1, 1, hms(hours = 15), 18, "https://turing-uk.zoom.us/j/93941631356", "tugce",
  1, 2, hms(hours = 10), 5, "https://turing-uk.zoom.us/j/95880984986", "ioana",
  1, 2, hms(hours = 11), 17, "https://turing-uk.zoom.us/j/99916467421", "solon",
  1, 2, hms(hours = 14), 3, "https://turing-uk.zoom.us/j/91621172306", "akira",
  1, 2, hms(hours = 15), 13, "https://turing-uk.zoom.us/j/94564694583", "prateek",
  2, 1, hms(hours = 10), 14, "https://turing-uk.zoom.us/j/94365127915", "risa",
  2, 1, hms(hours = 11), 8, "https://turing-uk.zoom.us/j/94365127915", "keri",
  2, 1, hms(hours = 14), 10, "https://turing-uk.zoom.us/j/99777001117", "nikolas",
  2, 1, hms(hours = 15), 9, "https://turing-uk.zoom.us/j/92927994244", "lizhi",
  2, 1, hms(hours = 16), 12, "https://turing-uk.zoom.us/j/94586862227", "pedro",
  2, 2, hms(hours = 10), 4, "https://turing-uk.zoom.us/j/98703863600", "feargus",
  2, 2, hms(hours = 11), 11, "https://turing-uk.zoom.us/j/92929385919", "obi",
  2, 2, hms(hours = 14), 2, "https://turing-uk.zoom.us/j/98190487545", "daniele",
  2, 2, hms(hours = 15), 15, "https://turing-uk.zoom.us/j/94514516679", "ryan"
)

showcase1 <- lubridate::ymd("2020-09-17") %>% lubridate::as_datetime()
showcase2 <- lubridate::ymd("2020-09-24") %>% lubridate::as_datetime()

start_hour <- hms::hms(hours = 10)
end_hour <- hms::hms(hours = 16)

showcase1_slots <- 
  seq(from = showcase1 + start_hour, to = showcase1 + end_hour, by = "1 hour") %>%
  expand_grid(datetime = ., showcase = 1L, room = c(1L,2L))

showcase2_slots <- 
  seq(from = showcase2 + start_hour, to = showcase2 + end_hour, by = "1 hour") %>%
  expand_grid(datetime = ., showcase = 2L, room = c(1L,2L))

programme <-
  bind_rows(
    showcase1_slots,
    showcase2_slots
  ) %>%
  mutate(time = as_hms(datetime)) %>%
  left_join(zoom, by = c("showcase", "time", "room")) %>%
  mutate(poster_id = str_pad(as.character(poster_id), 2, "left", "0")) %>%
  pivot_wider(names_from = room, values_from = c(poster_id, zoom_link, expected)) %>%
  mutate(time = format(as.POSIXct(time), "%H:%M"))

write_csv(programme, "programme.csv")
