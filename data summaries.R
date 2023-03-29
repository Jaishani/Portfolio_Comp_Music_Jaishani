library(spotifyr)
library(tidyverse)
A1 <-get_playlist_audio_features("", "0nHJwDKzBrDBK46dfkfUI7?")
A2 <-get_playlist_audio_features("", "4DSMGS0xzzI7lYJZ7JoO2h?")
HH1 <-get_playlist_audio_features("", "7GwLszxCBQ34088qNret9i?")
HH2 <-get_playlist_audio_features("", "2HZwwiPVfJNAYfi3DilCrV?")
A1|> summarise(mean_speechiness = mean(speechiness),
               mean_acousticness = mean(acousticness),
               mean_danceability = mean(danceability),
               mean_valence = mean(valence))
)
A2|> summarise(mean_speechiness = mean(speechiness),
               mean_acousticness = mean(acousticness),
               mean_danceability = mean(danceability),
               mean_valence = mean(valence))
)
HH1|> summarise(mean_speechiness = mean(speechiness),
               mean_acousticness = mean(acousticness),
               mean_danceability = mean(danceability),
               mean_valence = mean(valence))
)
HH2|> summarise(mean_speechiness = mean(speechiness),
               mean_acousticness = mean(acousticness),
               mean_danceability = mean(danceability),
               mean_valence = mean(valence))
)

Afro <- get_track_audio_features(c("2WigMwGJysIh9fRnSJvpjn"))
Hiphop <- get_track_audio_features(c("0fX4oNGBWO3dSGUZcVdVV2"))

ggplot(HH2, aes (speechiness,valence, colour=energy))+ geom_point()+ geom_smooth()+expand_limits(y=0)+scale_x_log10()+ geom_jitter()+ggtitle("Speechiness, Valence and Energy (Contemporary Hip Hop)")

install.packages('flexdashboard') 
library(plotly)
library(Cairo)
