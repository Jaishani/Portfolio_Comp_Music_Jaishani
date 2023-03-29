library(spotifyr)
library(compmus)
library(tidyverse)

wondamo <- get_tidy_audio_analysis("0xPz8qjfchKRCAmABJH65Z")
wondamo |>
  tempogram(window_size = 8, hop_size = 1, cyclic = FALSE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

 WondamoChart <- wondamo |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic() + ggtitle("Won Da Mo by Rema, Boy Spyce and Mavins" )





