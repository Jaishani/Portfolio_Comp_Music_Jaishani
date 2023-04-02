
library(tidymodels)
library(ggdendro)

Afrobeat <- get_playlist_audio_features("31s4gl3lbbpum3bhi4dq3w53zlwe", "2EHPmAGzuUxKZ5lhB3XyIZ")
Afrofusion <- get_playlist_audio_features("31s4gl3lbbpum3bhi4dq3w53zlwe", "4DSMGS0xzzI7lYJZ7JoO2h")

afromusic <-
  bind_rows(
    Afrobeat |> mutate(playlist = "Afrobeat Music") |> slice_head(n = 20),
    Afrofusion |> mutate(playlist = "Afrofusion Music") |> slice_head(n = 20)
  ) |> 
  add_audio_analysis()


# Note that you are not allowed to have duplicate songs in the dataset! 
afromusic |>
  count(track.name) %>%
  arrange(desc(n))

tracks_to_remove <- afromusic |>
  count(track.name) |>
  filter(n >= 2) %>%
  select(track.name)

afromusic <- afromusic %>%
  anti_join(tracks_to_remove)

# Similar to code from class, still trying to predict track.name but later we add on playlist name!
afromusic_juice <-
  recipe(
    track.name ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo,
    data = afromusic
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |> 
  # step_range(all_predictors()) |> 
  prep(afromusic |> mutate(track.name = str_trunc(track.name, 20))) |>
  juice() |>
  column_to_rownames("track.name")

afromusic_dist <- dist(afromusic_juice, method = "euclidean")

data_for_afromusic_clustering <- afromusic_dist |> 
  hclust(method = "average") |> # average for a balanced tree!
  dendro_data() 

playlist_data_for_join <- afromusic %>%
  select(track.name, playlist_name) %>%
  mutate(label = str_trunc(track.name, 20))

data_for_afromusic_clustering$labels <- data_for_afromusic_clustering$labels %>%
  left_join(playlist_data_for_join)

# Add factor so can use colouring! 
data_for_afromusic_clustering$labels$label <- factor(data_for_afromusic_clustering$labels$label)

Dendrogram <- data_for_afromusic_clustering |>
  ggdendrogram() +
  geom_text(data = label(data_for_afromusic_clustering), aes(x, y, 
                                                         label=label, 
                                                         hjust=0, 
                                                         colour=playlist_name), size=3) +
  coord_flip() + 
  scale_y_reverse(expand=c(0.2, 0)) +
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank()) +
  labs(title = "Playlist Clustering") +
  guides(
    colour = guide_legend(
      title = "Playlist"
    )
  )