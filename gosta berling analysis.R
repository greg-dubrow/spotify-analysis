## uses spotifyr package to look at stats for my band gosta berling

library(tidyverse)
library(httr)
library(stringr)
library(lubridate)
library(ggrepel)
library(spotifyr)
library(janitor)
library(GGally)
library(PerformanceAnalytics)
library(corrr)


# as of 061019 issues w./ CRAN verson, override with dev
#devtools::install_github('charlie86/spotifyr', force = TRUE)

# if you need to add spotify API keys
# usethis::edit_r_environ()
# as SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx'
# as SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx'

# or do
# Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')

# sets access token for session
access_token <- get_spotify_access_token()

# if you run into redirect issues, see this stackoverflow thread
# https://stackoverflow.com/questions/32956443/invalid-redirect-uri-on-spotify-auth
# specifically this comment https://stackoverflow.com/a/51967789/10226848

# gets full range of information for tracks from artist

# note: this only gets Gosta Berling's Saga. Need to use version w/ artist ID
      #gosta_audio1 <- get_artist_audio_features(artist = 'Gosta Berling')
# include single as Travel and Sweetheart listed as singles
gosta_audio1 <- get_artist_audio_features(artist = "4Vb2yqJJthJTAZxKz4Aryn",
                                           include_groups = c("album", "single"))
glimpse(gosta_audio1)

unnest(gosta_audio1, cols = c(album_images, artists, available_markets))
gosta_audio1 %>%
  count(track_name, key, key_name, mode_name, key_mode)


# for some reason only returning data for last album, so I need to get audio info song by song
# First I looked up album IDs 
# gosta id = 4Vb2yqJJthJTAZxKz4Aryn
# travel album id 0vBs7ZtBj3ROrRyac3M47q
# sweetheart album id 0dJBaJ3VFxOtdG5L9yzALJ
# winterland album id 6CMekiY6lCIuBZpzFDInpf

# get album tracks, add album name could merge on other df, easier to quick fix this way

travel <- get_album_tracks(id = "0vBs7ZtBj3ROrRyac3M47q")
travel$album <- "Travel"
sweetheart <- get_album_tracks(id = "0dJBaJ3VFxOtdG5L9yzALJ")
sweetheart$album <- "Everybody's Sweetheart"
winterland  <- get_album_tracks(id = "6CMekiY6lCIuBZpzFDInpf")
winterland$album <- "Winterland"

# merge album files, output track ids to use for audio features
gbtracks <- data.table::rbindlist(list(sweetheart, travel, winterland))
gbtrackids <- dput(as.character(gbtracks$id)) # copy result from console

gosta_audio2 <- get_track_audio_features(c("2SotrXjkvjTZf05XSMKGyp", "07cTJ65GZ4Lvr6b1CtgPll", "4ooz79IN3la97See8IMNRL", 
                                       "7pgCh68iFO0LNUNKWTFFIP", "4ZCesDRgGWKEXwq8iKw5FB", "4ZdH5B3tijHjWiwyOErgtf", 
                                       "5GWKeBYgOsv3PKutDIQoet", "0XXWRsY6URe2Vx7Bxs6k06", "0t3AGVXHyF3dEYuhvAYuNz", 
                                       "4ObsuwrVLKUq5aF8whrFqk", "0PnjWfIPwsqBtllMILjzxB", "7uQtlGsKxXOzsSapKTZRFU", 
                                       "3kQuG44stzA3pQf7g61Ipt", "0YH9wkimhRhCmstNZyxPgO", "7rEbjyNO0dTEK6x8HkLqAz", 
                                       "4VgEAtVQtkwIHzKMOROk6X", "5R9M4s6QZljNPVVzxoy98h", "1FNtHQ0juoKg2yCf9u4VSg", 
                                       "5NWmfmupE7FEJ9O1e9vizu"),
                                     authorization = get_spotify_access_token())
glimpse(gosta_audio2)


# get track number and name, merge from gbtracks - need b/b not returned from get_track_audio_features()
gbtrack2 <- gbtracks %>%
  select(id, name, album, track_number) %>%
  rename(track_name = name)

# merge to complete df. add names for key and mode
gosta_audio <- left_join(gosta_audio2, gbtrack2) %>%
  mutate(key_name = case_when(key == 0 ~ "C", key == 2 ~ "D", key == 4 ~ "E", key == 5 ~ "F",
                              key == 7 ~ "G", key == 9 ~ "A", key == 11 ~ "B")) %>%
  mutate(mode_name = case_when(mode == 0 ~ "Minor", mode == 1 ~ "Major")) %>%
  mutate(key_mode = paste(key_name, mode_name, sep = " ")) %>%
  rename(track_id = id) %>%
  select(album, track_name, track_number, key_mode, time_signature, duration_ms, 
         danceability, energy, loudness, tempo, valence, 
         acousticness, instrumentalness, liveness, speechiness,
         key_name, mode_name, key, mode)
  
glimpse(gosta_audio)

saveRDS(gosta_audio, file = "data/gosta_audio.rds")


## visualize the data!
  # some notes from spotify here about elements 
  # https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/
  # most of the aduio features are 0-1, 1 being highest. e.g. higher speechiness = more words::music
  # loundess in dB, valence is "happiness", w/ higher = happier, tempo is BPM
  # md file to copme with images

# First, let's get the average values for the song features
gosta_audio %>%
  count(track_name, loudness)
gb_song_means <- gosta_audio %>%
  select(duration_ms, tempo, danceability, energy, valence, 
         acousticness, instrumentalness, liveness, speechiness) %>%
  summarise_all(list(mean), na.rm = TRUE) 
#  summarise_all(list(mean)) 
glimpse(gb_song_means)

gb_song_meansl <- gb_song_means %>% 
  gather(song_measure, mean, 1:9) %>%
  mutate
glimpse(gb_song_meansl)

## correlation among all the values
# subset the data
gbcorr <- gosta_audio[, c(6:15)]

# base r
cor(gbcorr)
# w/ ouput to an object
gbcorrs1 <- as.data.frame(cor(gbcorr))
view(gbcorrs1)

# ggally
ggcorr(gbcorr, label = TRUE)

# corrr package 
gbcorr %>%
  correlate(use = "pairwise.complete.obs", method = "spearman")
gbcorrs2 <- correlate(gbcorr)
rplot(gbcorrs2)
   # network graph
correlate(gbcorr) %>% 
  network_plot(min_cor=0.5)

# performance analytics package
#chart.Correlation(gbcorr, histogram=TRUE, pch="+")
chart.Correlation(gbcorr, histogram = FALSE, method = c("pearson", "kendall", "spearman"))



# get colors from https://www.ginifab.com/feeds/pms/color_picker_from_image.php
# winterland color #8989a2
#travel #615a54
# sweethear #707070


## energy & happiness
gosta_audio %>%
  ggplot(aes(energy, valence, color = album)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_text_repel(aes(label = track_name), size = 3) +
  scale_color_manual(values = c("#707070", "brown", "dark blue")) +
  ylim(0, 1) +
  xlim(0, 1) +
  theme_minimal() +
  labs(x = "energy", y = "valence (happiness)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# tempo & happiness?
gosta_audio %>%
  ggplot(aes(tempo, valence, color = album)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_text_repel(aes(label = track_name), size = 3) +
  scale_color_manual(values = c("#707070", "brown", "dark blue")) +
  ylim(0, 1) +
  theme_minimal() +
  labs(x = "tempo (bpm)", y = "valence (happiness)") +
  theme(legend.position = "bottom", legend.title = element_blank())

## tempo & energy
gosta_audio %>%
  ggplot(aes(tempo, energy, color = album)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_text_repel(aes(label = track_name), size = 3) +
  scale_color_manual(values = c("#707070", "brown", "dark blue")) +
  ylim(0, 1) +
  theme_minimal() +
  labs(x = "tempo (bpm)", y = "energy") +
  theme(legend.position = "bottom", legend.title = element_blank())

# speechiness & happiness?
gosta_audio %>%
  ggplot(aes(speechiness, valence, color = album)) +
  geom_point() +
  geom_text_repel(aes(label = track_name), size = 3) +
  scale_color_manual(values = c("#707070", "brown", "dark blue")) +
  ylim(0, 1) +
  xlim(0,.5) +
  theme_minimal() +
  labs(x = "speechiness", y = "valence (happiness)") +
  theme(legend.position = "bottom", legend.title = element_blank())

## add comment that spotify has graphic showing speech distros are skewed low



## gets detailed audio attributes for a specific song. returns as large list
gbaudio2 <- get_track_audio_analysis("4ZdH5B3tijHjWiwyOErgtf", authorization = get_spotify_access_token())

## get playlist data
my_id <- 'dannebrog13'
my_plists <- get_user_playlists(my_id)

# tracks for playlist w/all GB songs
gbtracks <- get_playlist_tracks('0DoMumzDaWeAXUEtZ4m4qN')

gbfeatures <- get_track_audio_features(audio_travel1) 


### random code bits
#beatles <- get_artist_audio_features(artist = 'the beatles')
