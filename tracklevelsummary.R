library(spotifyr)
library(compmus)
library(tidyverse)

afronew <- get_playlist_audio_features(
  playlist_uris = "4DSMGS0xzzI7lYJZ7JoO2h"
) |> 
  slice(1:30) |> 
  add_audio_analysis()

afroold <- get_playlist_audio_features(
  playlist_uris = "0nHJwDKzBrDBK46dfkfUI7"
) |> 
  slice(1:30) |> 
  add_audio_analysis()

tracksummary <- afronew |>
  mutate(genre = "AfroNew") |>
  bind_rows(afroold |> mutate(genre = "AfroOld"))

jazzChart <- tracksummary |>
  mutate(energy = as.integer(energy*10)) |>
  mutate(
    sections = map(
      sections,
      summarise_at,
      vars(tempo, loudness, energy),
      list(section_mean = mean, section_sd = sd)
    )
  ) |>
  unnest(sections) |>
  ggplot(
    aes(
      x = tempo,
      y = tempo_section_sd,
      colour = genre,
      alpha = loudness
    )
  ) +
  geom_point(aes(size = energy)) +
  geom_rug() +
  theme_minimal() +
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Genre",
    size = "Energy",
    alpha = "Volume (dBFS)"
  ) + ggtitle('Track level analysis for Afrobeat and Afrofusion playlists')
