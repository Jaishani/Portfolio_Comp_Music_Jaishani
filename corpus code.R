library(plotly)
library(ggplot2)
library(dplyr)
library(wesanderson)
install.packages("wesanderson")

chartHH2 <-
ggplot(HH2, aes (speechiness,valence, colour=energy)) + 
  geom_point() + 
  geom_smooth() +
  expand_limits(y=0) +
  scale_x_log10() + 
  geom_jitter()+ggtitle("Speechiness, Valence and Energy (Contemporary Hip Hop)")


afro_old <- A1 %>% select(danceability, energy, loudness, mode, speechiness, acousticness,instrumentalness, valence, track.album.release_date)


hiphop_old %>%
rename(song_release_date = track.album.release_date)
hiphop_old %>% 
rename(song = row.names)

average_valence_afro_new <-afro_new %>%
summarize(average_valence = mean(valence))

average_valence_afro_old <-afro_old %>%
  summarize(average_valence = mean(valence))

average_valence_hiphop_new <-hiphop_new %>%
  summarize(average_valence = mean(valence))

afro_hiphop_valence <- rbind(average_valence_afro_new, average_valence_afro_old, average_valence_hiphop_new, average_valence_hiphop_old) %>% 

--------------------
afro_hiphop_valence <- data.frame(average_valence = c(0.6291016, 0.7349648, 0.4630257, 0.6349505))

print ("Original DataFrame : ")
print (afro_hiphop_valence)


rownames <- rownames(afro_hiphop_valence)
print ("Original row names ")
print (rownames)


rownames(afro_hiphop_valence) <- c("AfroFusion","Afrobeat","Contemporary HipHop","90s HipHop")

print ("Modified DataFrame : ")
print (afro_hiphop_valence)

colours <- c ("blue", "blue", "red", "red")

chartAfroHipHopValence <- ggplot(afro_hiphop_valence, aes(x=rownames(afro_hiphop_valence), y= average_valence)) + 
  #geom_col(fill= "#cc0000") +
  geom_bar(stat="identity", fill = colours) +
  xlab("Genre") +
  ylab("Average Valence") +
  theme_bw()  + ggtitle("Average Valence of Afrobeat, AfroFusion, \n90s and Contemporary HipHop")


afro_valence <- rbind(average_valence_afro_new, average_valence_afro_old) 
hiphop_valence <- rbind(average_valence_hiphop_new, average_valence_hiphop_old)

afro_valence <- data.frame(average_valence = c(0.6291016, 0.7349648))

print ("Original DataFrame : ")
print (afro_valence)


rownames <- rownames(afro_valence)
print ("Original row names ")
print (rownames)



hiphop_valence <- data.frame(average_valence = c(0.4630257, 0.6349505))

print ("Original DataFrame : ")
print (hiphop_valence)


rownames <- rownames(hiphop_valence)
print ("Original row names ")
print (rownames)


rownames(hiphop_valence) <- c("Contemporary HipHop","90s HipHop")

print ("Modified DataFrame : ")
print (hiphop_valence)


ChartAV <-ggplot(afro_valence, aes(x=rownames(afro_valence), y= average_valence)) + 
  geom_col(fill= "#581845") +
  xlab("Genre") +
  ylab("Average Valence") +
  theme_bw() + ggtitle("Average Valence of \nAfrobeat vs AfroFusion")

ChartHHV <- ggplot(hiphop_valence, aes(x=rownames(hiphop_valence), y= average_valence)) + 
  geom_col(fill= "#900C3F") +
  xlab("Genre") +
  ylab("Average Valence") +
  theme_bw() +
  ggtitle("Average Valence 90s HipHop \nvs Contemporary HipHop")


chartSSM_famous<- 
  ggplot(chartSSM_famous, aes(x = xstart + xduration / 2,
                              width = xduration,y = ystart + yduration / 2,
                              height = yduration,fill = d)) + 
  geom_tile() + 
  coord_fixed() + 
  scale_fill_viridis_c(guide = "none") + 
  theme_classic() + labs(x = "", y = "")  

file.exists("/fullpath/to/portfolio.Rmd")

