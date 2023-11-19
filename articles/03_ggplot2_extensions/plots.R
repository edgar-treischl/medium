#00 SETUP#########
#library(tidyverse)
#Checked
library(showtext)
library(patchwork)
library(waffle)
library(ggplot2)
library(ggalluvial)
library(magrittr)
library(dplyr)
#library(titanic)
library(ggmosaic)
library(ggbeeswarm)

library(tidyr)
library(palmerpenguins)


#library(rgeos)
library(geojsonio)
library(broom)
library(readxl)
library(ggmap)
library(ggcorrplot)
library(treemapify)
library(ggridges)
#library(forcats)

library(tidytext)


#you need dev version of ggwordcloud
#devtools::install_github("lepennec/ggwordcloud")

library(ggwordcloud)

library(ggcharts)
library(gapminder)
library(cowplot)
library(forcats)


font_add_google("Lato", "Lato")
#font_add_google("Combo", "Combo")


## Automatically use showtext to render text for future devices
showtext_auto()


#01 Alluvial#########

school_alluvial <- function() {
  
  df <- tibble::tribble(
    ~From,                 ~To, ~Freq,  ~Schulwechsel,
    "MS",     "FS",   863,  "Abstieg",
    "MS",        "RS", 4899, "Aufstieg",
    "MS",         "Gym",   312, "Aufstieg",
    "FS",      "MS",   757, "Aufstieg",
    "FS",        "RS",    19, "Aufstieg",
    "FS",         "Gym",     5, "Aufstieg",
    "RS",     "FS",    51,  "Abstieg",
    "RS",      "MS", 5669,  "Abstieg",
    "RS",         "Gym",   311, "Aufstieg",
    "Gym",     "FS",     6,  "Abstieg",
    "Gym",      "MS",   766,  "Abstieg",
    "Gym",        "RS", 7699,  "Abstieg",
    "Gym",    "FOS", 1227,  "Abstieg")
  
  
  df$To <- factor(df$To,
                  levels = c("Gym", "FOS", "RS", "MS", "FS"))
  
  
  df$From <- factor(df$From,
                    levels = c("Gym", "FOS", "RS", "MS", "FS"))
  
  
  
  ggplot(data = df,
         aes(axis1 = From, axis2 = To, y = Freq)) +
    geom_alluvium(aes(fill = Schulwechsel), alpha = 0.9) +
    geom_stratum()+
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), 
              size = rel(4))+
    theme_minimal(base_size = 14, base_family = "Lato")+
    labs(title = "Change of school type (2018/19)",
         caption = "Data: Official data from the Bavarian State Office for Statistics.")+
    scale_x_discrete(limits = c("From", "To")) +
    scale_fill_manual(values = c("#9b2226", "#003566"),
                      name = "Change of school",
                      labels = c("Downwards", "Upwards"))+
    theme(legend.position="bottom")+
    theme(plot.caption = element_text(color = "darkgray"))
  
  
}




#02 MOSAIC#########

mosaic_plot <- function(variables) {
  df <- summarytools::tobacco
  
  txt <- data.frame(
    x = c(0.15, 0.67, 0.15, 0.67),
    y = c(0.1, 0.1, 0.9, 0.9),
    text = c("55.8%", "22.3%", "44.2%", "77.7%")
  )
  
  ggplot(df) +
    geom_mosaic(aes(x = product(smoker), fill = diseased))+
    theme_minimal(base_size = 12)+
    ylab("Diseased")+
    xlab("Smoker")+
    theme(legend.position = "bottom")+
    geom_label(data = txt, 
               aes(x = x, 
                   y = y, 
                   label = text), size = 3)+
    scale_fill_manual(name = "Smoke", 
                      values=c("#457b9d", "#D3D3D3"))+
    theme(legend.position="none")
  #theme(text=element_text(family="DGMetaScience", size=12, face="bold"))
  
}

mosaic_plot2 <- function(variables) {
  df <- titanic::titanic_train
  
  #table(df$Survived, df$Sex)
  
  df$Survived <- factor(df$Survived,
                        levels = c(0, 1),
                        labels = c("Not survived", "Survived")) 
  
  df$Sex <- as.factor(df$Sex)
  df$Sex <- factor(df$Sex,
                   labels = c("Female", "Male")) 
  
  
  mytable <- summarytools::ctable(x = df$Sex, 
                                  y = df$Survived, 
                                  prop = "r")   # Show row proportions
  
  #mytable
  #mytable$proportions
  
  textplot <- c(round(mytable$proportions[1], 3)*100,
                round(mytable$proportions[2], 3)*100,
                round(mytable$proportions[4], 3)*100,
                round(mytable$proportions[5], 3)*100)
  
  txt <- data.frame(
    x = c(.18, .68, .18, .68),
    y = c(0.1, 0.1, 0.9, 0.9),
    text = paste(as.character(textplot), "%")
  )
  
  
  ggplot(df) +
    geom_mosaic(aes(x = product(Sex), fill = Survived),
                alpha = 1,
                color = c("#3C5A8A", "black", "#3C5A8A", "black"))+
    theme_minimal(base_size = 12, base_family ="Lato")+
    ylab("Survived")+
    xlab("Sex")+
    theme(legend.position = "bottom")+
    geom_label(data = txt, 
               aes(x = x, 
                   y = y, 
                   label = text), size = 3, family = "Lato")+
    scale_fill_manual(name = "Survived", 
                      values=c("#3C5A8A", "white"))+
    theme(legend.position="none")
}



#03 HEXBIN#########


# hexbin_plot <- function(variables) {
#   spdf <- geojson_read("DATA/us_states_hexgrid.geojson",  what = "sp")
#   
#   # Bit of reformating
#   spdf@data = spdf@data %>%
#     mutate(google_name = gsub(" \\(United States\\)", "", google_name))
#   
#   # I need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)
#   spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
#   spdf_fortified <- tidy(spdf, region = "google_name")
#   
#   # Calculate the centroid of each hexagon to add the label:
#   centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
#   
#   
#   #arrest <- as.data.frame(USArrests)
#   #arrest$id <- rownames(arrest, do.NULL = TRUE, prefix = "row")
#   
#   umemployment <- read_excel("DATA/umemployment.xlsx")
#   
#   
#   
#   spdf_fortified <- full_join(spdf_fortified, umemployment, by = "id")
#   
#   # Now I can plot this shape easily as described before:
#   ggplot() +
#     geom_polygon(data = spdf_fortified, aes(x = long, y = lat,  group = group, fill=Umemployment)) +
#     geom_text(data=centers, aes(x=x, y=y, label=id), 
#               family= "Lato") +
#     scale_fill_continuous(type = "viridis", name="Unemployment (%)", 
#                           guide="colorbar", na.value="white"
#     )+
#     guides(fill = guide_colourbar(barwidth = 7, barheight = 1.5))+
#     theme_void(base_size = 12, base_family ="Lato") +
#     coord_map() +
#     theme(legend.position="bottom")
#   #theme(text=element_text(family="DGMetaScience", size=12, face="bold"))
#   
#   
# }
# 
# hexbin_plot()


#04 BEESWARM#########

# beeplot <- function(variables) {
#   titanic_df<-as.data.frame(titanic_train) 
#   
#   titanic_df$Survived <- factor(titanic_df$Survived, 
#                                 levels = c(0, 1),
#                                 labels = c("Not survived", "Survived")) 
#   
#   titanic_df$sex2 <- factor(titanic_df$Sex,
#                             levels = c("male", "female"),
#                             labels = c("Men", "Women"))
#   
#   ggplot(titanic_df, aes(Survived, Age, color = Sex)) + 
#     geom_quasirandom(size = 1, width = 0.5)+
#     scale_color_manual(values=c("#0057AD", "#C70C0B"))+
#     #scale_color_manual(values=c("#94d2bd", "#005f73"))+
#     theme_minimal(base_size = 12, base_family ="Lato")+
#     theme(legend.position="bottom")
#   #theme(text=element_text(family="DGMetaScience", size=12, face="bold"))
#   
# }

bees <- function() {
  
  penguins <- tidyr::drop_na(penguins)
  
  ggplot(penguins, aes(species, body_mass_g, color = sex)) + 
    geom_quasirandom(size = 1.3, width = 0.5)+
    scale_color_manual(values=c("#94d2bd", "#005f73"),
                       name = "Sex",
                       labels = c("Female", "Male"))+
    theme_minimal(base_size = 14, base_family ="Lato")+
    theme(legend.position="bottom")+
    labs(y = "Body mass (g)",
         x = "Species")
  
  
}


#05 GGMaps#########

map_plot <- function(variables) {
  data <- data.frame(murder = USArrests$Murder,
                     state = tolower(rownames(USArrests)))
  map <- map_data("state")
  
  ggplot(data, aes(fill = murder))+
    geom_map(aes(map_id = state), map = map)+
    expand_limits(x = map$long, y = map$lat)+
    xlab("Longitude")+
    ylab("Latitude")+
    scale_fill_continuous(type = "viridis", name="Arrest in the US", 
                          guide="colorbar",na.value="white", trans = 'reverse')+
    theme_minimal(base_size = 12, base_family ="Lato")+
    
    theme(legend.position="bottom")
}


map_plot()



#06 Treemamp#########
treemap_data <- tibble::tribble(
  ~Topic,        ~Subtopic, ~Raw, ~percentage, ~percentage_facet,
  "Democrats",         "Others",  24L,        14.04,              63.16,
  "Democrats",  "Chuck & Nancy",   6L,         3.51,              15.79,
  "Democrats",        "Hillary",   8L,         4.68,              21.05,
  "GOP",         "Corker",   4L,         2.34,              33.33,
  "GOP",         "Others",   8L,         4.68,              66.67,
  "The media",         "Others",  14L,         8.19,              15.73,
  "The media",            "NYT",  17L,         9.94,              19.10,
  "The media",            "CNN",  13L,         7.60,              14.61,
  "The media",            "NBC",  12L,         7.02,              13.48,
  "The media", "\"Fake News!\"",  33L,        19.30,              37.08,
  "Others",  "Int'l figures",   3L,         1.75,               9.38,
  "Others",      "Companies",   4L,         2.34,              12.50,
  "Others",         "Sports",   8L,         4.68,              25.00,
  "Others",    "Celebrities",   6L,         3.51,              18.75,
  "Others",          "Comey",   7L,         4.09,              21.88,
  "Others",         "Others",   4L,         2.34,              12.50
)

tree_map <- function(variables) {
  ggplot(treemap_data, aes(area = percentage, fill = Topic, label = Subtopic,
                           subgroup = Topic)) +
    geom_treemap(alpha = 0.8, colour ="gray", size=3) +
    geom_treemap_subgroup_border(alpha = 0.8, colour ="black")+
    geom_treemap_subgroup_text(place = "bottomleft", grow = FALSE, alpha = 0.25, 
                               colour ="black", 
                               fontface = "italic", min.size = 0, family = "Lato")+
    geom_treemap_text(fontface = "italic", colour = "black", alpha = 0.8, place = "center",
                      grow = FALSE, min.size = 0, family = "Lato")+
    #scale_fill_manual(values=c("#034e7b", "red", "#bdbdbd", "#807dba"))+
    scale_fill_manual(values=c("white", "white", "white", "white"))+
    theme_minimal(base_size = 12, base_family ="Lato")+
    theme(legend.position="none")  
  #theme(text=element_text(family="Times", size=16, face="bold"))
  
}

#07 Ridgeplot#########

ridge_plot <- function(variables) {
  df <- gapminder::gapminder %>% 
    filter(year == 2007 & continent != "Oceania")%>% 
    group_by(continent) %>% 
    mutate(avg_lf = mean(lifeExp))
  
  
  ggplot(df, aes(x = lifeExp, y = fct_rev(continent), fill = stat(x))) +
    geom_density_ridges_gradient(scale = 0.95, rel_min_height = 0.0001) +
    scale_fill_viridis_c(name = "Life Expectancy", option = "D") +
    xlab("Life Expectancy")+
    ylab("Continent")+
    theme_minimal(base_size = 12, base_family ="Lato")+
    theme(legend.position="right")
  
}


#08 Waffles#########


#font_add_google("Patua One", "Patua")
#showtext_auto()

waffle_plot <- function(variables) {
  students <- c(`Female`= 49, `Male`= 51)
  
  
  
  w1 <- waffle(students, rows=10, size=1, 
               colors=c("#012a4a", "#89c2d9"), 
               title="Share of Students", flip = T)+
    theme(plot.title = element_text(size=12),
          text=element_text(family="Times"))+
    theme(legend.position="bottom")
  
  
  absolventen <- c(`Women`= 52, `Men`= 48)
  
  
  w2 <- waffle(absolventen, rows=10, size=1,
               colors=c("#012a4a", "#89c2d9"), 
               title="Graduates", flip = T,
               legend_pos = "none")+
    theme(plot.title = element_text(size=12),
          text=element_text(family="Lato"))
  
  phd <- c(`Women`= 45, `Men`= 55)
  
  w3 <- waffle(phd, rows=10, size=1, 
               colors=c("#012a4a", "#89c2d9"), 
               title="Ph.D.s", flip = T, legend_pos = "none")+
    theme(plot.title = element_text(size=12),
          text=element_text(family="Lato"))
  
  
  habil <- c(`Women`= 32, `Men`= 68)
  
  w4 <- waffle(habil, rows=10, size=1, 
               colors=c("#012a4a", "#89c2d9"), 
               title="Habilitation", flip = T, legend_pos = "none")+
    theme(plot.title = element_text(size=12),
          text=element_text(family="Lato"))+
    theme(legend.position="bottom")
  
  
  prof <- c(`Women`= 12, `Men`= 88)
  
  w5 <- waffle(prof, rows=10, size=1, 
               colors=c("#012a4a", "#89c2d9"), 
               title="Professors", flip = T, legend_pos = "none")+
    theme(plot.title = element_text(size=12),
          text=element_text(family="Lato")
    )+
    labs(caption = "Source: destatis.de")+
    theme(text=element_text(family="Lato"))
  
  
  #w2 | w3 | w4 | w5
  (w2 + w3) / (w4 + w5)
  
  
  
}



#09 Wordle#########

wordle_plot <- function(variables) {
  font_add_google("Gloria Hallelujah", "Gloria+Hallelujah")
  
  ## Automatically use showtext to render text for future devices
  showtext_auto()
  
  load("DATA/deathly_hallows.rda")
  
  #length(deathly_hallows)
  
  
  text_df <- tibble(line = 1:37, text = deathly_hallows)
  
  text_df <- text_df %>%
    unnest_tokens(word, text)
  
  data(stop_words)
  
  harrys <- tibble(word = c("harry", "hermione", "ron", "harry's",
                            "ron's", "hermione's", "potter", 
                            "told", "stood", "weasley", "boy"),
                   lexicon = "SMART") 
  
  text_df <- text_df %>%
    anti_join(stop_words)
  
  text_df <- text_df %>%
    anti_join(harrys)
  
  txtvis <- text_df %>%
    count(word, sort = TRUE) 
  
  
  txtvis <- txtvis[1:50, ]
  
  txtvis <- txtvis %>%
    mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))
  
  set.seed(123)
  ggplot(txtvis, aes(label = word, size = n,
                     angle = angle)) +
    geom_text_wordcloud_area(color = "black",
                             family="Gloria+Hallelujah")+
    scale_size_area(max_size = 24, trans = power_trans(1/.7)) +
    #scale_size_area(max_size = 24) +  
    theme_minimal()+
    theme(text=element_text(family="Gloria+Hallelujah"))+
    labs(title = "Harry Potter and the Deathly Hallows")+
    theme(plot.title = element_text(size=20, face = "bold"))
  #scale_color_gradient(low = "blue", high = "red")
  
}


#10 Dumbbell#########

dumbbell_plot <- function(variables) {
  df <- gapminder::gapminder %>% 
    select(1:4) %>% 
    filter (continent == "Europe")%>% 
    filter (year == min(gapminder$year) | year == max(gapminder$year))%>% 
    pivot_wider(names_from = year, values_from = lifeExp,
                names_prefix = "lifeExp_") %>%
    mutate(diff = lifeExp_2007 - lifeExp_1952)%>%
    slice_max(diff, n = 10)%>% 
    arrange(diff)
  
  df$country <- dplyr::recode_factor(df$country, `Bosnia and Herzegovina` = "BiH")
  
  vector <- df %>% 
    pull(country)
  
  df$country <- factor(df$country, levels=vector)
  
  
  
  
  dumbbell_chart(df, x = country,
                 y1 = lifeExp_1952,
                 y2 = lifeExp_2007,
                 line_color = "#457b9d",
                 sort = FALSE,
                 line_size = 1.2,
                 point_size = 2,
                 point_colors = c("#457b9d", "#1d3557"),
                 legend_labels = c("1952", "2007")
  )+
    labs(x = NULL, y = NULL) +
    scale_y_continuous(labels = function(x) paste(x, "Years"))+
    theme_minimal(base_size = 12, base_family ="Lato")+
    theme(text=element_text(size = 12),
          axis.text = element_text(size = 12))+
    theme(legend.position="bottom")
  
  
}




#title = "Increases of Life Expectancy"

# alice_plot <- function(variables) {
#   alice2 <- read_csv("DATA/alice3.txt")
#   alice <- alice2$`Alice in Wonderland`
#   
#   text_df <- tibble(line = 1:824, text = alice)
#   
#   text_df <- text_df %>%
#     unnest_tokens(word, text)
#   
#   data(stop_words)
#   
#   
#   tidy_books <- text_df %>%
#     anti_join(stop_words)
#   
#   df <- tidy_books %>%
#     count(word, sort = TRUE) %>% 
#     filter(n > 7) 
#   
#   harrys <- tibble(word = c("alice"),
#                    lexicon = "SMART")
#   
#   df <- df %>%
#     anti_join(harrys)
#   
#   font_add_google("Delius", "Delius")
#   
#   ## Automatically use showtext to render text for future devices
#   showtext_auto()
#   
#   df <- df %>%
#     mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))
#   
#   set.seed(123)
#   ggplot(df, aes(label = word, size = n,
#                  angle = angle)) +
#     geom_text_wordcloud_area(color = "black",
#                              family="Delius")+
#     scale_size_area(max_size = 24, trans = power_trans(1/.7))+
#     #scale_size_area(max_size = 24) +  
#     theme_minimal()+
#     theme(text=element_text(family="Delius"))+
#     #labs(title = "Alice in Wonderland")+
#     theme(plot.title = element_text(size=20, face = "bold"))
#   #scale_color_gradient(low = "blue", high = "red")
#   
# }

tree2 <- function(variables) {
  df <- data.frame(
    stringsAsFactors = FALSE,
    ID = c("AL","AT","BE","BG","CH",
           "CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
           "HU","IE","IS","IT","LI","LT","LU","LV","ME",
           "MK","MT","NL","NO","PL","PT","RO","RS","SE","SI",
           "SK","TR"),
    size = c(2.85,8.9,11.52,6.95,8.61,
             0.89,10.69,83.17,5.82,1.33,10.72,47.33,5.53,67.32,
             4.06,9.77,4.96,0.36,59.64,0.04,2.79,0.63,1.91,
             0.62,2.08,0.51,17.41,5.37,37.96,10.3,19.33,6.93,
             10.33,2.1,5.46,83.15)
  )
  
  
  ggplot(df, aes(area = size , fill = size,
                 label = paste(ID, size, sep = "\n"))) +
    geom_treemap()+
    geom_treemap_text(colour = "white",
                      place = "centre",
                      size = 15) +
    scale_fill_viridis_c(direction = -1)+
    theme(legend.position = "none")
}






