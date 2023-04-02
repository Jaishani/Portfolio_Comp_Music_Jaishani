library(tidyverse)
library(spotifyr)
library(compmus)


kulosa2 <-
  get_tidy_audio_analysis("2WigMwGJysIh9fRnSJvpjn") |>
  compmus_align(bars, segments) |>
  select(bars) |>
  unnest(bars) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "acentre", norm = "manhattan"
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "mean"
      )
  )
KulosaChart2 <- bind_rows(
  kulosa2 |> 
    compmus_self_similarity(pitches, "aitchison") |> 
    mutate(d = d / max(d), type = "Chroma"),
  kulosa2 |> 
    compmus_self_similarity(timbre, "euclidean") |> 
    mutate(d = d / max(d), type = "Timbre")
) |>
  mutate() |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_classic() + 
  labs(x = "", y = "") + ggtitle("Ku Lo Sa by Oxlade")
