library(tidyverse)
library(lubridate)
library(zoo)
# 
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)

# Quelle: shorturl.at/enFWZ
netflix_top_viewed <- function(history, top){
  serie <- history %>%
    separate(col = Title, into = c("title", "staffel", "episode"), sep = ': ')
  serie <- serie[!is.na(serie$staffel),]
  serie <- serie[!is.na(serie$episode),]
  marathon <- serie %>%
    count(title, Date)
  marathon <- marathon[marathon$n >= 6,]
  marathon <- marathon[order(marathon$Date),]
  marathon_sorted <- marathon %>% group_by(title) %>% 
    summarise(n = sum(n)) %>% arrange(desc(n))
  marathon_sorted_plot <- marathon_sorted %>% 
    top_n(top) %>%
    ggplot(aes(x = reorder(title, n), y = n)) +
    geom_col(fill = "#0097d6") +
    coord_flip() +
    ggtitle(paste("Top",top, "binged series (bingeing: view at least 6 episodes from one series per day)"), filename) +
    labs(x = "series", y = "viewed episodes") +
    theme_minimal()
  
  return(marathon_sorted_plot)
}
netflix_per_time <- function(history){
  netflix_per_time <- history %>% count(Date) %>% arrange(desc(n))
  range <- range(pretty(history$Date))
  netflix_per_time_plot <- ggplot(aes(x = Date, y = n, color = n), data = netflix_per_time) +
    geom_col(color = c("#FFB90F")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ggtitle("streams per day", filename) +
    labs(x = "time", y = "viewed episodes/movies") +
    scale_x_date(date_labels = "%m %Y", breaks = seq(as.Date(range[1]), as.Date(range[2]), by = "2 months"))
return(netflix_per_time_plot)
}
netflix_heatmap <- function(history){
  netflix_per_day <- history %>% count(Date) %>% arrange(desc(n))
  #
  netflix_per_day <- netflix_per_day[order(netflix_per_day$Date),]
  netflix_per_day$diasemana <- wday(netflix_per_day$Date)
  netflix_per_day$diasemanaF <- weekdays(netflix_per_day$Date, abbreviate = T)
  netflix_per_day$mesF <- months(netflix_per_day$Date, abbreviate = T)
  #
  netflix_per_day$diasemanaF <-factor(netflix_per_day$diasemana, levels = rev(1:7), labels = rev(c("Mo","Tu","We","Th","Fr","Sa","Su")),ordered = TRUE)
  netflix_per_day$mesF <- factor(month(netflix_per_day$Date),levels = as.character(1:12), labels = c("January","February","March","April","May","June","July","August","September","October","November","December"),ordered = TRUE)
  netflix_per_day$añomes <- factor(as.yearmon(netflix_per_day$Date)) 
  netflix_per_day$semana <- as.numeric(format(netflix_per_day$Date,"%W"))
  netflix_per_day$semanames <- ceiling(day(netflix_per_day$Date) / 7)
  netflix_per_day_calendario <- ggplot(netflix_per_day, aes(semanames, diasemanaF, fill = netflix_per_day$n)) + 
    geom_tile(colour = "white") + 
    facet_grid(year(netflix_per_day$Date) ~ mesF) + 
    scale_fill_gradient(low = "#FFD000", high = "#FF1919") + 
    ggtitle("activity per day", filename) +
    labs(x = "week oof the month", y = "weekday") +
    labs(fill = "episodecount")
  return(netflix_per_day_calendario)
}
netflix_weekdays <- function(history){
  netflix_per_day <- history %>% count(Date) %>% arrange(desc(n))
  #
  netflix_per_day <- netflix_per_day[order(netflix_per_day$Date),]
  netflix_per_day$diasemana <- wday(netflix_per_day$Date)
  netflix_per_day$diasemanaF <- weekdays(netflix_per_day$Date, abbreviate = T)
  netflix_per_day$mesF <- months(netflix_per_day$Date, abbreviate = T)
  #
  netflix_per_day$diasemanaF <-factor(netflix_per_day$diasemana, levels = rev(1:7), labels = rev(c("Mo","Tu","We","Th","Fr","Sa","Su")),ordered = TRUE)
  netflix_per_day$mesF <- factor(month(netflix_per_day$Date),levels = as.character(1:12), labels = c("January","February","March","April","May","June","July","August","September","October","November","December"),ordered = TRUE)
  netflix_per_day$añomes <- factor(as.yearmon(netflix_per_day$Date)) 
  netflix_per_day$semana <- as.numeric(format(netflix_per_day$Date,"%W"))
  netflix_per_day$semanames <- ceiling(day(netflix_per_day$Date) / 7)
  #
  vista_dia <- netflix_per_day %>% count(diasemanaF)
  vista_dia_plot <- vista_dia %>% 
    ggplot(aes(diasemanaF, n)) +
    geom_col(fill = "#5b59d6") +
    coord_polar()  +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "bold"),
          plot.title = element_text(size = 16, face = "bold")) +
    ggtitle("activity per weekday", filename)
return(vista_dia_plot)
}
netflix_months <- function(history){
  netflix_per_day <- history %>% count(Date) %>% arrange(desc(n))
  #
  netflix_per_day <- netflix_per_day[order(netflix_per_day$Date),]
  netflix_per_day$diasemana <- wday(netflix_per_day$Date)
  netflix_per_day$diasemanaF <- weekdays(netflix_per_day$Date, abbreviate = T)
  netflix_per_day$mesF <- months(netflix_per_day$Date, abbreviate = T)
  #
  netflix_per_day$diasemanaF <-factor(netflix_per_day$diasemana, levels = rev(1:7), labels = rev(c("Mo","Di","Mi","Do","Fr","Sa","So")),ordered = TRUE)
  netflix_per_day$mesF <- factor(month(netflix_per_day$Date),levels = as.character(1:12), labels = c("Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember"),ordered = TRUE)
  netflix_per_day$añomes <- factor(as.yearmon(netflix_per_day$Date)) 
  netflix_per_day$semana <- as.numeric(format(netflix_per_day$Date,"%W"))
  netflix_per_day$semanames <- ceiling(day(netflix_per_day$Date) / 7)
  #
  vista_mes <- netflix_per_day %>% count(mesF)
  vista_mes_plot <- vista_mes %>% 
    ggplot(aes(mesF, n)) +
    geom_col(fill = "#808000") +
    coord_polar()  +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "bold"),
          plot.title = element_text(size = 18, face = "bold")) +
    ggtitle("activity per month", filename) 
return(vista_mes_plot)
}

netflix_top_titles <- function(history,top){
  corp <- corpus(history$Title, docvars = history)
  tok <- tokens(corp, remove_punct=T, remove_numbers = T, remove_url = T,split_hyphens = T, remove_symbols = T)
  dfm <- dfm(tok)
  dfm <- dfm_remove(dfm, pattern = c("season", "the", "staffel"))
  tstat_freq <- textstat_frequency(dfm, n = 5)
  
return(dfm %>% textstat_frequency(n = top) %>% 
    ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
    geom_point() + coord_flip() + labs(x = NULL, y = "Frequency") +
    theme_minimal())
}
netflix_top_titles_wordcloud <- function(history,wordcount){
  corp <- corpus(history$Title, docvars = history)
  tok <- tokens(corp, remove_punct=T, remove_numbers = T, remove_url = T,split_hyphens = T, remove_symbols = T)
  dfm <- dfm(tok)
  dfm <- dfm_remove(dfm, pattern = c(stopwords("de"), stopwords("en"),"season", "the", "staffel", "folge", "teil"))
return(textplot_wordcloud(dfm, max_words = wordcount))
}
netflix_network <- function(history, minwordnumber, howmanywords){
  corp <- corpus(history$Title, docvars = history)
  tok <- tokens(corp, remove_punct=T, remove_numbers = T, remove_url = T,split_hyphens = T, remove_symbols = T)
  dfm <- dfm(tok)
  dfm <- dfm_remove(dfm, pattern = c(stopwords("de"), stopwords("en"),"season", "the", "staffel", "folge"))
  dfm <- dfm_trim(dfm, min_termfreq = minwordnumber)
  #
  fcm <- fcm(dfm)
  feat <- names(topfeatures(fcm, howmanywords))
  fcm_select <- fcm_select(fcm, pattern = feat, selection = "keep")
  dim(fcm_select)
  #
  size <- log(colSums(dfm_select(dfm, feat, selection = "keep")))
  set.seed(144)
  return(textplot_network(fcm_select, min_freq = 0.8, vertex_size = size / max(size) * 3))
}
top_gesehene_episoden_echte_anzahl <- function(history){
  serie <- history %>%
    separate(col = Title, into = c("title", "staffel", "episode"), sep = ': ')
  serie <- serie[!is.na(serie$staffel),]
  serie <- serie[!is.na(serie$episode),]
  marathon <- serie %>%
    count(title, Date)
  marathon <- marathon[order(marathon$Date),]
  marathon_sorted <- marathon %>% group_by(title) %>% 
    summarise(n = sum(n)) %>% arrange(desc(n))
  marathon_sorted_plot <- marathon_sorted %>% 
    top_n(20) %>%
    ggplot(aes(x = reorder(title, n), y = n)) +
    geom_col(fill = "#0097d6") +
    coord_flip() +
    ggtitle(paste("top 20 viewed titles"), filename) +
    labs(x = "Serien", y = "geschaute episodes/movies") +
    theme_minimal()
  return(marathon_sorted_plot)
}

# Reading and cleaning data ----

# Input filename
cat("enter name of the history.csv file (without the .csv): ")
filename <- readLines(con = "stdin", n = 1)

# reading history
netflix_history <- read.csv(paste(getwd(),"/",filename,".csv",sep = "")) 
netflix_history$Date <- dmy(netflix_history$Date)

# create folder for saving plots ----
dir.create(paste(getwd(),"/",filename,"_Plots",sep = ""))


# Plots ----

netflix_per_time(netflix_history)
ggsave(
  paste(filename,"_per_time",".png",sep = ""),
  plot = last_plot(),
  device = png,
  path = paste(getwd(),"/",filename,"_Plots",sep = ""),
  scale = 0.5,
  width = 15000,
  height = 7500,
  units = "px",
  dpi = 600,
  limitsize = TRUE,
  bg = "white"
)



netflix_heatmap(netflix_history)
ggsave(
  paste(filename,"_heatmap",".png",sep = ""),
  plot = last_plot(),
  device = png,
  path = paste(getwd(),"/",filename,"_Plots",sep = ""),
  scale = 0.5,
  width = 19000,
  height = 8000,
  units = "px",
  dpi = 600,
  limitsize = TRUE,
  bg = "white"
)


netflix_months(netflix_history)
ggsave(
  paste(filename,"_months",".png",sep = ""),
  plot = last_plot(),
  device = png,
  path = paste(getwd(),"/",filename,"_Plots",sep = ""),
  scale = 0.5,
  width = 8000,
  height = 8000,
  units = "px",
  dpi = 600,
  limitsize = TRUE,
  bg = "white"
)


netflix_weekdays(netflix_history)
ggsave(
  paste(filename,"_weekdays",".png",sep = ""),
  plot = last_plot(),
  device = png,
  path = paste(getwd(),"/",filename,"_Plots",sep = ""),
  scale = 0.5,
  width = 8000,
  height = 8000,
  units = "px",
  dpi = 600,
  limitsize = TRUE,
  bg = "white"
)


netflix_top_viewed(netflix_history, 10)
ggsave(
  paste(filename,"_top_10_viewed_series",".png",sep = ""),
  plot = last_plot(),
  device = png,
  path = paste(getwd(),"/",filename,"_Plots",sep = ""),
  scale = 0.5,
  width = 15000,
  height = 7500,
  units = "px",
  dpi = 600,
  limitsize = TRUE,
  bg = "white"
)


#netflix_top_titles(netflix_history, 15)

#netflix_top_titles_wordcloud(netflix_history, 75)


top_gesehene_episoden_echte_anzahl(netflix_history)
ggsave(
  paste(filename,"_top_20_viewed_objects",".png",sep = ""),
  plot = last_plot(),
  device = png,
  path = paste(getwd(),"/",filename,"_Plots",sep = ""),
  scale = 0.5,
  width = 15000,
  height = 7500,
  units = "px",
  dpi = 600,
  limitsize = TRUE,
  bg = "white"
)


