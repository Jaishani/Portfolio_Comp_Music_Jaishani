library(tidyverse)
library(spotifyr)
library(compmus)

 expensiveshit2 <-
   get_tidy_audio_analysis("6sNNtFKdCz0bnjx7IEXyl2") |>
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
 ESChart2 <- bind_rows(
   expensiveshit2 |> 
     compmus_self_similarity(pitches, "aitchison") |> 
     mutate(d = d / max(d), type = "Chroma"),
   expensiveshit2 |> 
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
   labs(x = "", y = "") + ggtitle("Expensive Shit by Fela Kuti")
 
 
 
 

 