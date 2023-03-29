install.packages("tidymodels")
install.packages("ggdendro")
install.packages("heatmaply")

library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(spotifyr)
library(compmus)

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |> 
    collect_predictions() |> 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit |> 
    conf_mat_resampled() |> 
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |> 
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |> 
    ungroup() |> filter(Prediction == Truth) |> 
    select(class = Prediction, precision, recall)
  
  afrobeatplaylist <-
    get_playlist_audio_features("31s4gl3lbbpum3bhi4dq3w53zlwe", "0nHJwDKzBrDBK46dfkfUI7") |>
    add_audio_analysis() |>
    mutate(
      segments = map2(segments, key, compmus_c_transpose),
      pitches =
        map(segments,
            compmus_summarise, pitches,
            method = "mean", norm = "manhattan"
        ),
      timbre =
        map(
          segments,
          compmus_summarise, timbre,
          method = "mean"
        )
    ) |>
    mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
    mutate_at(vars(pitches, timbre), map, bind_rows) |>
    unnest(cols = c(pitches, timbre))
  
  afrobeat_juice <-
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
        tempo +
        duration +
        C + `C#|Db` + D + `D#|Eb` +
        E + `F` + `F#|Gb` + G +
        `G#|Ab` + A + `A#|Bb` + B +
        c01 + c02 + c03 + c04 + c05 + c06 +
        c07 + c08 + c09 + c10 + c11 + c12,
      data = afrobeatplaylist
    ) |>
    step_center(all_predictors()) |>
    step_scale(all_predictors()) |> 
    # step_range(all_predictors()) |> 
    prep(afrobeatplaylist |> mutate(track.name = str_trunc(track.name, 20))) |>
    juice() |>
    column_to_rownames("track.name")
}  

afrobeatplaylist_dist <- dist(afrobeat_juice, method = "euclidean")
afrobeatplaylist_dist |> 
  hclust(method = "single") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram()

