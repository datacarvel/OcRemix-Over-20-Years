### OK BABY

library(tidyverse)
library(rvest)
library(rlist)
library(httr)
library(lubridate)
library(patchwork)
library(ggbeeswarm)
library(extrafont)
library(Cairo)
library(plotly)
library(scales)
library(png)
library(grid)
library(ggtext)
library(shiny)
library(stringi)
library(stringr)
library(shinythemes)
library(htmlwidgets)
CairoWin()

# I'll spare you the awful, simply horrible details motivating me to warn you about this, but if you plan to use particular fonts and you happen to change devices (project is in a cloud folder), I suggest you put the .ttf files in a folder in your R project folder.

font_import(path = "fonts-oc") # My folder that is inside my R project folder
loadfonts(device = "win") # Some necessary other step
fonts() # Just a way of seeing them all

set_config(add_headers(`User-Agent` = "Hello, I'm Steve ! I can be reached at steevecarvel@gmail.com")) # To be honest I think it only works with the GET() function but... just in case. 

# GENRE TAGS Grabber

GenreTags <- read_html("https://ocremix.org/remix/OCR04000") %>%
  html_nodes(xpath = '//span[@class = "badge badge-secondary badge-tag-genre"]/a') %>%
  html_text()

# INSTRUMENT TAGS Grabber

InstrumentTags <- read_html("https://ocremix.org/remix/OCR04000") %>%
  html_nodes(xpath = '//span[@class = "badge badge-secondary badge-tag-instrument"]/a') %>%
  html_text()

# DATE POSTED Grabber

DatePosted <- read_html("https://ocremix.org/remix/OCR04000") %>%
  html_nodes(xpath = '//p[@class = "color-secondary"]') %>%
  html_text() %>%
  str_extract("[0-9]{4}.[0-9]{2}.[0-9]{2}") %>%
  as.Date()

# Ok, time to put that all into a SINGLE-PAGE SCRAPER

DataScraper <- function(x) {
  
  closeAllConnections() # Just to minimize the number of times R says it closed a given unused connection (Still got a few, but much less). I believe you want your script to generate as less error messages or warnings as possible
  
  Sys.sleep(5) # Because you don't want to look like you're shooting DDoS artillery
  
  read_x <- try(read_html(x), silent = TRUE)
  
  if (length(read_x) == 1) { # First tried with "if (class(read_x) == "try-error")" but the former is more efficient, less error-generating
      
    DataFrame <- tibble(SongTitle = NA, Artist = NA, Game = NA, Date = NA, Genres = NA, Instruments = NA, URL = x, URLfound = FALSE)
    
  } else {
  
  SongTitle <- read_x %>%
    html_nodes(xpath = '//div[@class = "col-md-12"]/h1') %>%
    html_text() %>%
    str_extract('(?<=\\s\\").*(?=\\")')

  Artist <- read_x %>%
    html_node(xpath = '//div[@class = "col-md-7 col-lg-8"]//div[@class = "col-md-12"]/h2[text() = "By "]') %>%
    html_text() %>%
    str_extract("(?<=By\\s).*(?=\\n)")
    
  Game <-read_x %>%
    html_node(xpath = '//div[@class = "col-md-7 col-lg-8"]/section[@class = "row" and position() = 4]') %>% 
    html_text() %>%
    str_extract("(?<=Game:\\s).*(?=\\,\\smusic)")
  
  # After a few hours, thanks to this page for being the one tutorial I needed to finally isolate what I needed for the Game information : https://towardsdatascience.com/using-stringr-and-regex-to-extract-features-from-textual-alphanumeric-and-punctuation-data-in-r-2565256c0a77
  
  Genres <- read_x %>%
    html_nodes(xpath = '//span[@class = "badge badge-secondary badge-tag-genre"]/a') %>%
    html_text()
  
  Instruments <- read_x %>%
    html_nodes(xpath = '//span[@class = "badge badge-secondary badge-tag-instrument"]/a') %>%
    html_text()
  
  Date <- read_x %>%
    html_nodes(xpath = '//p[@class = "color-secondary"]') %>%
    html_text() %>%
    str_extract("[0-9]{4}.[0-9]{2}.[0-9]{2}") %>%
    as.Date()
  
  DataFrame <- tibble(SongTitle, Artist, Game, Date, Genres = list(Genres), Instruments = list(Instruments), URL = x, URLfound = TRUE)
  
  }
  # Thanks to the unnest_longer() function from tidyr, which I believe is a relatively recent feature, whether I'm interested in analyzing the genres or instruments, I'll just be able to expand the list items into individual dataframe rows for vizzing them
  # That is why I insert them into a list, but another option would have been to directly put either Genres or Instruments into those additional rows by removing the list() around Genres OR Instruments just above (although one issue was that doing so would leave out tracks that had no genre etiquette from the dataset altogether)
  
  print(DataFrame)
  
}

# Amazing, now let us test it on a few pages

ThreeURLs <- c("https://ocremix.org/remix/OCR04000", "https://ocremix.org/remix/OCR02111", "https://ocremix.org/remix/OCR02114")
ThreeAndEmpty <- c("https://ocremix.org/remix/OCR04000", "https://ocremix.org/remix/OCR02111", "https://ocremix.org/remix/OCR02114", "https://ocremix.org/remix/OCR02244")
ThreeAndError <- c("https://ocremix.org/remix/OCR04000", "https://ocremix.org/remix/OCR02111", "https://ocremix.org/remix/OCR02114", "https://ocremix.org/remix/OCR05000")
AllCases <- c("https://ocremix.org/remix/OCR04000", "https://ocremix.org/remix/OCR02111", "https://ocremix.org/remix/OCR02114", "https://ocremix.org/remix/OCR02244", "https://ocremix.org/remix/OCR05000")

VerySoftLaunch <- map_dfr(ThreeURLs, DataScraper)
ErrorLaunch <- map_dfr(ThreeAndError, DataScraper) # The last URL of this short vector does not exist
EmptyLaunch <- map_dfr(ThreeAndEmpty, DataScraper) # The last URL of this short vector exists, but the remix has no genre tag
AllCasesLaunch <- map_dfr(AllCases, DataScraper)

# Next step : generate the list of URLS.
# At the time of writing this, the latest single remix was this one : https://ocremix.org/remix/OCR04213
# In the case that a few others add themselves up between now and my scraping operations, I'll add a few with a higher number, since now our scraper can handle URL errors.

# So here we are generating a sequence from 00001 to 04213. The extra zeroes are needed for the URLs. 
# Problem : if I tell R to generate such a sequence, he won't keep the zeroes.
# Quickest workaround I have found is to add an extra 1 on the left so it's an actual number. We'll remove the extra zero when it becomes a string value (character).

Numbers <- as.character(c(100001:104260)) %>%
  str_trunc(width = 5, side = "left", ellipsis = "")

BaseURL <- "https://ocremix.org/remix/OCR"

URLs <- str_c(BaseURL, Numbers, sep = "")

# Since I have like over 4 000 pages to scrape, I'll be nice and split the scraping operations into a few parts.
# So this below takes some time. Like 1 hour per part, as in (700 * 5) / 60 = 58.33

## SCRAPER IS AWAITING ORDERS

Launch1 <- map_dfr(URLs[1:700], DataScraper) 
Launch2 <- map_dfr(URLs[701:1400], DataScraper) 
Launch3 <- map_dfr(URLs[1401:2100], DataScraper) 
Launch4 <- map_dfr(URLs[2101:2800], DataScraper)
Launch5 <- map_dfr(URLs[2801:4260], DataScraper) 

# It seems it went smoothly, so we can now combine them all

OC_data <- rbind(Launch1, Launch2, Launch3, Launch4, Launch5)

# One thing I did notice though is that there seems to be a lot of tracks with no genre tag given, which is a bit disappointing but it seems to be the case with the earliest tracks mostly
# So I probably won't do a 22-year analysis of all that... for the music genres at least.
# Because while we're at it, we can conduct an analysis of the most frequent instruments, the most remixed games, the most important artists, etc. 

# Anyway, for now let's stick to the music genres and remove the non-existent URLs and the tracks with no genre tags

OC_musicgenres_withoutNA <- OC_data %>% 
  dplyr::filter(URLfound == TRUE) %>% # Non-existent URLs were given the FALSE value
  unnest_longer(Genres) %>% # Amazing function from {tidyr} that expands each list item within the column genre into a new row while duplicating the values of the other columns for that entry
  drop_na(Genres) # Tracks that had no genre tag attached to them were given a NA by the previous function, so let's get rid of them

# Now, each track can have more than one tag, making a given song appear more than once in the dataset
# So let's see how many unique remixes we have left - the best shortcut we have for this is simply by checking the URL

length(unique(OC_musicgenres_withoutNA$URL)) # 2766

# So out of 4260 tracks as of July 24th 2021, only 2766 of them were given at least one genre tag.
# That's a lot of dropped tracks... meh. Almost half!
# So when I post about this project, I'll make sure to mention that...
  # (1) Not every remix has a unique page (as OC remix also released albums and not all tracks within an album was the object of a distinct URL page showing off the track)
  # (2) Only a little bit more than half of all those tracks that were given a distinct presentation 
  # (3) So... I'm not sure how representative this resulting sample is... The instruments data may be more interesting as it seems to have fewer empty entries

# Anyway, let's move on regardless with the music genres

# I would like to see first how many entries are left for each year

GenresPerYear <- OC_musicgenres %>%
  group_by(Year = format(Date, format = "%Y")) %>%
  summarise(PerYear = n())

A <- ggplot(GenresPerYear, aes(x = Year, y = PerYear)) +
  geom_col() +
  theme_minimal() + 
  ggtitle("All genre tags per year - a single track can be represented more than once here") +
  ylim(0, 400)

# It's a bit inconsistent. Genre-tagging was very much a thing in 2000 and 2001 but much less in the few following years. It started to increase for good in 2006.
# At this point it's hard to tell for sure whether it's really about missing tags or whether it simply reflects the amount of tracks published regardless of the tag.
# But this is a one observation per genre tag view of things. Maybe more recently multi-tagging has been more of a thing than in the post
# Let's go back to a basic level and look at all tracks published through the years

OC_alltracks <- OC_data %>% 
  dplyr::filter(URLfound == TRUE)

AllTracksPerYear <- OC_tracks %>%
  group_by(Year = format(Date, format = "%Y")) %>%
  summarise(PerYear = n())

B <- ggplot(AllTracksPerYear, aes(x = Year, y = PerYear)) +
  geom_col() +
  theme_minimal() + 
  ggtitle("All tracks per year") +
  ylim(0, 400)

# Apparently 2002 was a very productive year - at least with regards to tracks that were given their own individual track page (remember!)
# Aside from that peak and some downtime from 2005 to 2008 inclusively (still all above 100 remixes per year almost), the community's productivity was fairly stable, with peaks at and around 2014. 

# So, if you're still here, there is one last thing I'd like to check, to compare all remixes vs all genre-tagged remixes.
# The point is to see whether the number of tracks, of genre-tagged tracks and the tags themselves follow the same productivity trend more or less.

OC_alltracks_least1 <- OC_alltracks %>%
  unnest_longer(Genres) %>%
  drop_na(Genres) %>%
  group_by(Year = format(Date, format = "%Y")) %>%
  summarise(PerYear = unique(URL)) %>%
  summarise(PerYear = n())

C <- ggplot(OC_alltracks_least1, aes(x = Year, y = PerYear)) +
  geom_col() +
  theme_minimal() + 
  ggtitle("Tracks with at least one genre tag per year") +
  ylim(0, 400)

B + C + A # You need the {patchwork} package for this

# So it seems genre-tagging has been quite deficient from 2001 to 2006, but after which it has been somewhat deficient
# Let's just calculate the share of remixes that haven't got a single genre tag along the years

Missing <- tibble(Year = OC_alltracks_least1$Year, MissingShare = 1 - (OC_alltracks_least1$PerYear / AllTracksPerYear$PerYear))

D <- ggplot(Missing, aes(x = Year, y = MissingShare)) +
  geom_col() + 
  theme_minimal() +
  ggtitle("The share of remixes that have no music genre tag at all")

(B + C + A ) / D 

# 2002 and 2003 had more than 75 % or their tracks without any music genre tag ! And 2004 had more than 50 %.
# From 2005 to 2014, between 25 % and 50 % of all remixes had no genre tag, except 2012 below 25 %. Starting in 2015 though, it's all well under 10 %.

# So I will do a beeswarm plot thanks to {ggbeeswarm}, where each genre is given a place on the Y axis. This way it will even be mobile-friendly.

# In case I want to see all the data even the untagged tracks

OC_alltracks_even_untagged <- OC_alltracks %>%
  unnest_longer(Genres) 

# Extract color used by plot for each genre directly from the ggplot object n
# I gotta say this part annoyed me a bit and for quite some time, hence the low-imagination variable names

# Background panel image, and a few invited guests

imagee <- readPNG("wl44.png") 
gg <- rasterGrob(imagee, interpolate = FALSE, width = unit(1, "npc"), height = unit(1, "npc"))

# The weird-looking value of the text argument in the ggplot call at the top is for Plotly purposes, specifically the tooltip when you drag over a data point. 

# This genre name was too long

OC_musicgenres_withoutNA$Genres <- str_replace_all(OC_musicgenres_withoutNA$Genres, "spaghetti-western", "spag.-western")

# OK Let's go

shang1 <- readPNG("shang.png")
shang2 <- rasterGrob(shang1, interpolate = FALSE)

BeeVizzz <- ggplot(OC_musicgenres_withoutNA, aes(x = Date, y = fct_rev(fct_infreq(Genres)), color = Genres, text = paste("Game:", Game, "<br>", "Track:", '"', SongTitle, '"', "<br>", "Artist:", Artist))) + 
  annotation_custom(gg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  annotation_custom(shang2, xmin = as.Date("2013-11-05"), xmax = as.Date("2019-03-15"), ymin = 61.7, ymax = 65.7) +
  annotation_custom(shang2, xmin = as.Date("2016-12-05"), xmax = as.Date("2022-03-15"), ymin = 61.7, ymax = 65.7) +
  geom_beeswarm(groupOnX = FALSE, priority = "density", cex = 0.30, size = 0.25) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    axis.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 10, 0, -5),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "Pixel Emulator", size = 10, color = "#f8c828", margin = unit(c(8, 0, 0, 0), "pt")),
    plot.caption = element_text(family = "Impact", color = "#12b380", hjust = 1),
  ) +
  labs(
    caption = "data : ocremix.org"
  ) +
  annotate(geom = "text", hjust = 0, size = 2.3, x = as.Date("1990-12-15"), y = -1.5, color = "white", fontface = "italic", label = "As of July 31st, 2021") +
  annotate(geom = "text", hjust = 0, size = 2.3, x = as.Date("1990-12-15"), y = -2.5, color = "white", label = "This chart presents all OverClocked Remix tracks that 1) have an individual web post and 2) were given at") +
  annotate(geom = "text", hjust = 0, size = 2.3, x = as.Date("1990-12-15"), y = -3.3, color = "white", label = "least one music genre tag. Some individually published remixes/rearrangements have no genre tags, while ") +
  annotate(geom = "text", hjust = 0, size = 2.3, x = as.Date("1990-12-15"), y = -4.1, color = "white", label = "many, many other tracks are part of an album but were not the object of an individual web post. Check out") +
  annotate(geom = "text", hjust = 0, size = 2.3, x = as.Date("1990-12-15"), y = -4.9, color = "white", label = "their entire discography of 140+ albums here: https://ocremix.org/albums/") +
  annotate(geom = "text", hjust = 1, size = 2.3, x = as.Date("2021-09-15"), y = -4.8, color = "white", family = "Impact", label = "made by @stevecarufel_ / scarufel.com") +
  coord_cartesian(clip = "off", expand = FALSE, ylim = c(0.5, 61.6), xlim = c(as.Date("1999-09-15"), as.Date("2021-09-15")))

GGG <- ggplot_build(BeeVizzz) ### I HAVE TO CREATE THE BeeVizzz GGPLOT OBJECT BELOW BEFORE RUNNING THIS
JJJ <- GGG$data[[2]]["colour"]$colour
III <- GGG$plot[[1]]["Genres"]$Genres
JIJI <- tibble(Genre = III, Color = JJJ)
JIJI2 <- unique(JIJI[order(fct_infreq(JIJI$Genre)),])
JIJI3 <- JIJI2$Color

GenreCount <- JIJI %>%
  group_by(Genre) %>%
  dplyr::summarise(total = length(Genre)) %>%
  arrange(desc(total))

# I have been spending a shameful number of hours to find a way to display the Track Count per Music Genre, with geom_text/label and stat_summary but really couldn't
# So I come up with this, thanks to {patchwork}, even though I'm pretty sure there's a more efficient and less typing error-prone way of doing it.
# BeeVizzzCount2 only is for the numbers on the right

BeeVizzzCount2 <- ggplot(GenreCount, aes(y = fct_rev(factor(Genre, levels = fct_infreq(Genre))), x = 1, label = total)) +
  geom_text(aes(color = Genre, family = "Pixel Emulator"), size = 3) +
  scale_x_continuous(limits = c(1, 1), breaks = 1:1, expand = c(1, 1)) + 
  theme_minimal() +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "black"),
        axis.text.y = element_text(family = "Super Mario 256", size = 10, color = rev(JIJI3)),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(0, 0, 0, -10)
  )

# Full Static Viz Saving

FullStaticViz <- BeeVizzzCount2 + BeeVizzz + plot_layout(widths = c(0.47, 7.2)) +
  plot_annotation(
    title = "   OCREMIX.ORG, OVER 20 YEARS OF REMIXED VIDEO GAMES MUSIC",
    subtitle = "ALL THEIR REMIXES ARE BELONG TO EVERYONE, Each dot is a remix or a rearrangement!"
  ) & 
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    plot.title = element_text(family = "SNES", face = "italic", color = "#fc0c18", size = 22, margin = unit(c(5,0,1,50), "pt")),
    plot.subtitle = element_textbox(family = "SNES", face = "italic", color = "black", size = 15, fill = "#fc0c18", padding = unit(c(1, 4, 1, 1), "pt"), margin = unit(c(0,0,8,10), "pt")),
    plot.margin = margin(0, 5, 10, -5, unit = "pt")
  )

ggsave(FullStaticViz, filename = 'ocremix-over-the-years-stevecarufel-3aout.png', dpi = 300, type = 'cairo', width = 16, height = 26, units = 'cm')


