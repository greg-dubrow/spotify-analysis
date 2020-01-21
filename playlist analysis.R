# Compare week-by-week audio feature stats for discovery archive against likes 
# to see if there's been any change in what spoitfy thinks i like to the thinks i tag
# playlist access using this example https://msmith7161.github.io/what-is-speechiness/

library(spotifyr)
library(tidyverse)
library(tidylog)
library(visdat)
library(janitor)


# sets access token for session - keys in renviron
access_token <- get_spotify_access_token()

get_spotify_authorization_code(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                               client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                               scope = spotifyr::scopes)

my_id <- 'dannebrog13'
my_plists <-  get_user_playlists(my_id, limit = 50, offset = 0,
                                  authorization = get_spotify_authorization_code(),
                                  include_meta_info = FALSE)
my_plists <- my_plists %>%
  filter(owner.display_name == "dannebrog13")

my_plists %>%
  count(name)

# id for discover weekly archive
dwaid <- "5bnY1Oc9Lb1cWHFTA5uahV"

# get tracks for discover weekly archive - this will only get 100, per API limit
# dwa_tracks <- get_playlist_tracks(dwaid, fields = NULL, limit = 100,
#                     offset = 0, market = NULL,
#                     authorization = get_spotify_access_token(),
#                     include_meta_info = FALSE)

# gets all audio features of all tracks in discover weekly playlist
dwa_feat <- 
get_playlist_audio_features(my_id, dwaid,
                            authorization = get_spotify_access_token())
glimpse(dwa_feat)
write_rds(dwa_feat, 'all_dwa_feat.rds')

# script for this from https://www.rpubs.com/womeimingzi11/how_my_spotify_looks_like
all_dwa_tracks <-
  ceiling(get_playlist_tracks(dwaid, include_meta_info = TRUE)[['total']] / 100) %>%
  seq() %>%
  map(function(x) {
    get_playlist_tracks(dwaid, fields = NULL, limit = 100, offset = (x - 1) * 100,
                        market = NULL,
                        authorization = get_spotify_access_token(),
                        include_meta_info = FALSE)
  }) %>% reduce(rbind) 

write_rds(all_dwa_tracks, 'all_dwa_tracks.rds')
glimpse(all_dwa_tracks)


# extracts artist id and name 
artist_from_dwa <-
  dwa_feat %>%
  select(track.artists) %>%
  reduce(rbind) %>%
  reduce(rbind) %>%
  select(id, name)

artist_from_dwa <-
  dwa_feat %>%
  select(track.id, track.artists) %>%
  unnest(track.artists) %>%
  select(track.id, id, name) %>%
  group_by(track.id, name) %>%
  mutate(artist_num = row_number()) %>%
  ungroup()

glimpse(artist_from_dwa)

