#===========
#IMPORTS
#===========
library(tidyverse)
library(highcharter)
library(ggplot2)
library(igraph)
library(geosphere)
library(mstknnclust)
library(sp)

library(osmdata)
library(sf)
library(ggmap)
library(maps)
library(leaflet)
library(leaflet.extras)

#===========
#CONSTANTS
#===========
constants.since <- "2019-01-01"
constants.until <- "2021-01-01"
constants.columns <- c(
  'event_date',
  'event_type',
  'sub_event_type',
  'assoc_actor_1',
  'actor1',
  'country',
  'admin1',
  'fatalities',
  'latitude',
  'longitude'
)
constants.colors <- c(
  "#FFDB6D",
  "#C4961A",
  "#F4EDCA",
  "#D16103",
  "#C3D7A4",
  "#52854C",
  "#4E84C4",
  "#293352",
  "#14FF46",
  "#1ECBB1",
  "#BE04FF",
  "#9D16CC",
  "#340843",
  "#150728",
  "#524A53",
  "#929292",
  "#B69E9E",
  "#DDFFF7",
  "#ffd2fc",
  "#e980fc",
  "#b96ac9",
  "#231b1b",
  "#135F19",
  "#14761C",
  "#49805F",
  "#667ED0",
  "#A066D0",
  "#2A1266",
  "#7A6AA4",
  "#AF95F3",
  "#DED2FF",
  "#D3FFD2",
  "#D2FFF6",
  "#D2F4FF",
  "#D2E1FF",
  "#D7D2FF",
  "#B21BA9",
  "#B21B2B",
  "#921A26",
  "#75151E",
  "#520F15",
  "#340509",
  "#512327",
  "#1C0507",
  "#F6FF68",
  "#C0CA26",
  "#55CA26",
  "#26CA6C",
  "#26CAAF",
  "#107D6B",
  "#086253",
  "#05382F",
  "#203733",
  "#3B4B48",
  "#648F87",
  "#9EC9C1",
  "#562EFA",
  "#2F1697",
  "#35247A",
  "#413B5B",
  "#2C254B",
  "#FFB26E",
  "#BB763A",
  "#904F17"
)

#===========
#FUNCTIONS
#===========
get_df <- function(path){
  columns <- constants.columns
  df <- read.csv(path, header = TRUE)[-1, columns]
  df <- subset(df,(event_date > constants.since & event_date < constants.until))
  return(df)
}

get_cluster_distance <- function(data){
  return(mst.knn(data))
}

get_cluster_count <- function(query, group_tally){
  result <- group_tally %>% filter(cluster == query)
  return(as.numeric(result$n))
}

#If region == TRUE show south america map else show country map of data
build_highcharter <- function(data, region){
  if(region == TRUE){
    data <- data %>% group_by(country) %>% tally()
    
    hcmap(
      "custom/south-america", data = data, value = "n",
      joinBy = c("name", "country"),
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
    )
  }else {
    country <- head(data, 1)$country
    data <- data %>% group_by(admin1) %>% tally()
    
    map_data <- switch (country,
      "Ecuador" = "countries/ec/ec-all",
      "Chile" = "countries/cl/cl-all",
      "Venezuela" = "countries/ve/ve-all",
    )
    
    hcmap(
      map_data, data = data, value = "n",
      joinBy = c("name", "admin1"),
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
    )
  }
}

build_plot_cluster <- function(cluster){
  network <- cluster$network
  
  igraph::V(network)$label.cex <- seq(0.6,0.6,length.out=2)
  plot(
    network,
    vertex.size = 8,
    vertex.color= igraph::clusters(network)$membership,
    layout=igraph::layout.fruchterman.reingold(network, niter=10000),
    main=paste("MST-kNN \n Clustering solution \n Number of clusters = ", cluster$cnumber, sep = "" )
  )
  
}

build_map <- function(data){
  m <- leaflet() %>%
    setView(lng = -79.911, lat = -2.101 , zoom = 12) %>%
    addTiles() %>%
    addCircles(
      data = data,
      lng = as.numeric(data$longitude),
      lat = as.numeric(data$latitude),
      radius = 50,
      weight = 5,
      color = constants.colors[data$cluster],
    )
  m
}

#===========
#DF SETTERS
#===========
df.countries.ecuador <- get_df("data/ecuador.csv")
df.countries.chile <- get_df("data/chile.csv")
df.countries.venezuela <- get_df("data/venezuela.csv")
df.regions.south_america <- rbind(
  df.countries.ecuador,
  df.countries.chile,
  df.countries.venezuela
  )

#=============
#HIGHTCHARTER
#=============
build_highcharter(df.countries.ecuador, FALSE)
build_highcharter(df.countries.chile, FALSE)
build_highcharter(df.countries.venezuela, FALSE)
build_highcharter(df.regions.south_america, TRUE)

#==============
#GROUPS TALLYS
#==============
#************************************************************
#Countries sub events types groups
#************************************************************
df.countries.ecuador.groups.country_sub_event_type <- df.countries.ecuador %>% group_by(country,sub_event_type) %>% tally()
df.countries.chile.groups.country_sub_event_type <- df.countries.chile %>% group_by(country,sub_event_type) %>% tally()
df.countries.venezuela.groups.country_sub_event_type <- df.countries.venezuela %>% group_by(country,sub_event_type) %>% tally()

#************************************************************
#Countries events types fatalities groups
#************************************************************
df.countries.ecuador.groups.country_event_type_fatalities <- df.countries.ecuador %>% group_by(country,event_type,fatalities) %>% slice(which.max(fatalities)) %>% tally() %>% mutate(total_fatalities = n * as.numeric(fatalities))
df.countries.chile.groups.country_event_type_fatalities <- df.countries.chile %>% group_by(country,event_type,fatalities) %>% tally()  %>% mutate(total_fatalities = n * as.numeric(fatalities))
df.countries.venezuela.groups.country_event_type_fatalities <- df.countries.venezuela %>% group_by(country,event_type,fatalities) %>% tally() %>% mutate(total_fatalities = n * as.numeric(fatalities))

#************************************************************
#Countries assoc_actor_1 groups
#************************************************************
df.countries.ecuador.separate_rows.assoc_actor_1 <- subset(df.countries.ecuador, assoc_actor_1 != "") %>% separate_rows(assoc_actor_1, sep = ";")
df.countries.chile.separate_rows.assoc_actor_1 <- subset(df.countries.chile, assoc_actor_1 != "") %>% separate_rows(assoc_actor_1, sep = ";")
df.countries.venezuela.separate_rows.assoc_actor_1 <- subset(df.countries.venezuela, assoc_actor_1 != "") %>% separate_rows(assoc_actor_1, sep = ";")

df.countries.ecuador.groups.country_assoc_actor_1 <- df.countries.ecuador.separate_rows.assoc_actor_1 %>% group_by(country,assoc_actor_1) %>% tally()
df.countries.chile.groups.country_assoc_actor_1 <- df.countries.chile.separate_rows.assoc_actor_1 %>% group_by(country,assoc_actor_1) %>% tally()
df.countries.venezuela.groups.country_assoc_actor_1 <- df.countries.venezuela.separate_rows.assoc_actor_1 %>% group_by(country,assoc_actor_1) %>% tally()


#************************************************************
#Admin1 sub events types groups
#************************************************************
df.countries.ecuador.groups.admin1_sub_event_type <- df.countries.ecuador %>% group_by(admin1,sub_event_type) %>% tally()
df.countries.chile.groups.admin1_sub_event_type <- df.countries.chile %>% group_by(admin1,sub_event_type) %>% tally()
df.countries.venezuela.admin1.country_sub_event_type <- df.countries.venezuela %>% group_by(admin1,sub_event_type) %>% tally()

#===========
#SUMMARIES
#===========
#************************************************************
#Countries sub events types summaries
#************************************************************
df.countries.ecuador.summaries.country_sub_event_type <- df.countries.ecuador.groups.country_sub_event_type %>% summarise(
  min_count = min(n),
  min_sub_event_type =  sub_event_type[which(n == min(n))],
  max_count = max(n),
  max_sub_event_type = sub_event_type[which(n == max(n))], 
)

df.countries.chile.summaries.country_sub_event_type <- df.countries.chile.groups.country_sub_event_type %>% summarise(
  min_count = min(n),
  min_sub_event_type =  sub_event_type[which(n == min(n))],
  max_count = max(n),
  max_sub_event_type = sub_event_type[which(n == max(n))], 
)

df.countries.venezuela.summaries.country_sub_event_type <- df.countries.venezuela.groups.country_sub_event_type %>% summarise(
  min_count = min(n),
  min_sub_event_type = sub_event_type[which(n == min(n))],
  max_count = max(n),
  max_sub_event_type = sub_event_type[which(n == max(n))], 
)

df.regions.south_america.summaries.country_sub_event_type <- rbind(
  df.countries.ecuador.summaries.country_sub_event_type,
  df.countries.chile.summaries.country_sub_event_type,
  df.countries.venezuela.summaries.country_sub_event_type
)

#************************************************************
#Countries events types fatalities summaries
#************************************************************
df.countries.ecuador.summaries.country_event_type_fatalities <- df.countries.ecuador.groups.country_event_type_fatalities %>% summarise(
  min_count = min(as.numeric(total_fatalities)),
  max_count =  max(as.numeric(total_fatalities)),
)

df.countries.chile.summaries.country_event_type_fatalities <- df.countries.chile.groups.country_event_type_fatalities %>% summarise(
  min_count = min(as.numeric(total_fatalities)),
  max_count =  max(as.numeric(total_fatalities)),
)

df.countries.venezuela.summaries.country_event_type_fatalities <- df.countries.venezuela.groups.country_event_type_fatalities %>% summarise(
  min_count = min(as.numeric(total_fatalities)),
  max_count =  max(as.numeric(total_fatalities)),
)

df.regions.south_america.summaries.country_event_type_fatalities <- rbind(
  df.countries.ecuador.summaries.country_event_type_fatalities,
  df.countries.chile.summaries.country_event_type_fatalities,
  df.countries.venezuela.summaries.country_event_type_fatalities
)

#************************************************************
#Countries actor1 summaries
#************************************************************
df.countries.ecuador.summaries.country_assoc_actor_1 <- df.countries.ecuador.groups.country_assoc_actor_1 %>% summarise(
  min_count = min(as.numeric(n)),
  min_assoc_actor_1 = assoc_actor_1[which(n == min(n))],
  max_count = max(as.numeric(n)),
  max_assoc_actor_1 = assoc_actor_1[which(n == max(n))], 
)

df.countries.chile.summaries.country_assoc_actor_1 <- df.countries.chile.groups.country_assoc_actor_1 %>% summarise(
  min_count = min(as.numeric(n)),
  min_assoc_actor_1 = assoc_actor_1[which(n == min(n))],
  max_count = max(as.numeric(n)),
  max_assoc_actor_1 = assoc_actor_1[which(n == max(n))], 
)

df.countries.venezuela.summaries.country_assoc_actor_1 <- df.countries.venezuela.groups.country_assoc_actor_1 %>% summarise(
  min_count = min(as.numeric(n)),
  min_assoc_actor_1 = assoc_actor_1[which(n == min(n))],
  max_count = max(as.numeric(n)),
  max_assoc_actor_1 = assoc_actor_1[which(n == max(n))], 
)

df.regions.south_america.summaries.country_assoc_actor_1 <- rbind(
  df.countries.ecuador.summaries.country_assoc_actor_1,
  df.countries.chile.summaries.country_assoc_actor_1,
  df.countries.venezuela.summaries.country_assoc_actor_1
)

#************************************************************
#Admin1 sub events types
#************************************************************
df.countries.ecuador.summaries.admin1_sub_event_type <- df.countries.ecuador.groups.admin1_sub_event_type  %>% summarise(
  min_count = min(n),
  min_sub_event_type =  sub_event_type[which(n == min(n))],
  max_count = max(n),
  max_sub_event_type  = sub_event_type[which(n == max(n))], 
)

#===========
#PLOTS
#===========
#************************************************************
#Countries sub events types summaries plots
#************************************************************
ggplot(
  df.regions.south_america.summaries.country_sub_event_type, 
  aes(x = min_sub_event_type, y = min_count, fill=as.factor(country))
) + geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Que protestas se repitieron menos en los paises de Sudamerica") +
  labs(x = "Tipo de protesta", y = "Cantidad minima") + 
  theme()


ggplot(
  df.regions.south_america.summaries.country_sub_event_type, 
  aes(x = max_sub_event_type, y = max_count, fill = as.factor(country))
  ) + geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      ggtitle("Protestas pacificas en los paises de Sudamerica")

#************************************************************
#Countries events types fatalities summaries plots
#************************************************************
ggplot(
  df.regions.south_america.summaries.country_event_type_fatalities, 
  aes(x = event_type, y = min_count, fill = as.factor(country))
) + geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Numero minimo de muertes realizadas en los paises de Sudamerica") +
  labs(x = "Tipo de protesta", y = "Cantidad minima") + 
  theme()

ggplot(
  df.regions.south_america.summaries.country_event_type_fatalities, 
  aes(x = event_type, y = max_count, fill = as.factor(country))
) + geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Numero maximo de muertes realizadas en los paises de Sudamerica") +
  labs(x = "Tipo de protesta", y = "Cantidad maxima") + 
  theme()


#************************************************************
#Countries assoc_actor_1
#************************************************************
ggplot(
  df.regions.south_america.summaries.country_assoc_actor_1, 
  aes(x = max_assoc_actor_1, y = max_count, fill = as.factor(country))
) + geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Participantes en los paises de Sudamerica") +
  labs(x = "Tipo de protesta", y = "Cantidad maxima") + 
  theme()

#************************************************************
#Admin1 sub events types
#************************************************************

ggplot(
  df.countries.ecuador.summaries.admin1_sub_event_type, 
  aes(x = max_sub_event_type, y = max_count, fill = as.factor(max_sub_event_type))
) + geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Protestas que mas se realizaron en Ecuador") +
  labs(x = "Tipo de protesta", y = "Cantidad maxima") + 
  theme()

ggplot(
  df.countries.ecuador.summaries.admin1_sub_event_type, 
  aes(x = min_sub_event_type, y = min_count, fill = as.factor(max_sub_event_type))
) + geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Protestas que menos se realizaron en Ecuador") +
  labs(x = "Tipo de protesta", y = "Cantidad maxima") + 
  theme()



##documentation geosphere : https://cran.r-project.org/web/packages/geosphere/geosphere.pdf
#============================
#Distance matrix and MST-kNN
#============================
#***************
#ecuador
#***************
df.countries.ecuador.coords <- df.countries.ecuador[,c("longitude","latitude")]
df.countries.ecuador.coords <- transform(
  df.countries.ecuador.coords,
  longitude = as.numeric(longitude),
  latitude = as.numeric(latitude)
)
df.countries.ecuador.coords.spatial_points <- SpatialPoints(
  df.countries.ecuador.only_coords,
  proj4string=CRS(as.character(NA)), 
  bbox = NULL
)
df.countries.ecuador.coords.distance_harversine <- base::as.matrix(
  distm(
    df.countries.ecuador.coords, #x
    df.countries.ecuador.coords, #y
    distHaversine #function
  )
)
View(df.countries.ecuador.coords.distance_harversine)
rownames(df.countries.ecuador.coords.distance_harversine) <- rownames(df.countries.ecuador.coords)
colnames(df.countries.ecuador.coords.distance_harversine) <- rownames(df.countries.ecuador.coords.distance_harversine)
df.countries.ecuador.coords.cluster <- get_cluster_distance(df.countries.ecuador.coords.distance_harversine)
df.countries.ecuador.cluster.columns <- df.countries.ecuador[
  df.countries.ecuador.coords.cluster$partition[,"object"],
  constants.columns
]
df.countries.ecuador.cluster.table <- cbind(df.countries.ecuador.columns, cluster = df.countries.ecuador.coords.cluster$partition$cluster)
df.countries.ecuador.coords.distance_harversine.table <- as.vector(df.countries.venezuela.coords.distance_harversine)
df.countries.ecuador.coords.distance_harversine.table <- df.countries.ecuador.coords.distance_harversine.table[df.countries.ecuador.coords.distance_harversine.table > 0]

#***************
#chile
#***************
df.countries.chile.coords <- df.countries.chile[,c("longitude","latitude")]
df.countries.chile.coords <- transform(
  df.countries.chile.coords,
  longitude = as.numeric(longitude),
  latitude = as.numeric(latitude)
)
df.countries.chile.coords.spatial_points <- SpatialPoints(
  df.countries.chile.coords,
  proj4string=CRS(as.character(NA)), 
  bbox = NULL
)
df.countries.chile.coords.distance_harversine <- base::as.matrix(
  distm(
    df.countries.chile.coords, #x
    df.countries.chile.coords, #y
    distHaversine #function
  )
)
rownames(df.countries.chile.coords.distance_harversine) <- rownames(df.countries.chile.coords)
colnames(df.countries.chile.coords.distance_harversine) <- rownames(df.countries.chile.coords.distance_harversine)
df.countries.chile.coords.cluster <- get_cluster_distance(df.countries.chile.coords.distance_harversine)
df.countries.chile.cluster.columns <- df.countries.chile[
  df.countries.chile.coords.cluster$partition[,"object"],
  constants.columns
]
df.countries.chile.cluster.table <- cbind(df.countries.chile.columns, cluster = df.countries.chile.coords.cluster$partition$cluster)
df.countries.chile.coords.distance_harversine.table <- as.vector(df.countries.chile.coords.distance_harversine)
df.countries.chile.coords.distance_harversine.table <- df.countries.chile.coords.distance_harversine.table[df.countries.chile.coords.distance_harversine.table > 0]

#***************
#venezuela
#***************
df.countries.venezuela.coords <- df.countries.venezuela[,c("longitude","latitude")]
df.countries.venezuela.coords <- transform(
  df.countries.venezuela.coords,
  longitude = as.numeric(longitude),
  latitude = as.numeric(latitude)
)
df.countries.venezuela.coords.spatial_points <- SpatialPoints(
  df.countries.venezuela.coords,
  proj4string=CRS(as.character(NA)), 
  bbox = NULL
)
df.countries.venezuela.coords.distance_harversine <- base::as.matrix(
  distm(
    df.countries.venezuela.coords, #x
    df.countries.venezuela.coords, #y
    distHaversine #function
  )
)
rownames(df.countries.venezuela.coords.distance_harversine) <- rownames(df.countries.venezuela.coords)
colnames(df.countries.venezuela.coords.distance_harversine) <- rownames(df.countries.venezuela.coords.distance_harversine)
df.countries.venezuela.coords.cluster <- get_cluster_distance(df.countries.venezuela.coords.distance_harversine)
df.countries.venezuela.cluster.columns <- df.countries.venezuela[
  df.countries.venezuela.coords.cluster$partition[,"object"],
  constants.columns
]
df.countries.venezuela.cluster.table <- cbind(df.countries.venezuela.cluster.columns, cluster = df.countries.venezuela.coords.cluster$partition$cluster)
df.countries.venezuela.coords.distance_harversine.table <- as.vector(df.countries.venezuela.coords.distance_harversine)
df.countries.venezuela.coords.distance_harversine.table <- df.countries.venezuela.coords.distance_harversine.table[df.countries.venezuela.coords.distance_harversine.table > 0]


#============================
#CLUSTER ANALYSIS
#============================
build_plot_cluster(df.countries.ecuador.coords.cluster)
build_plot_cluster(df.countries.chile.coords.cluster)
build_plot_cluster(df.countries.venezuela.coords.cluster)

#============================
#CLUSTER GROUP ANALISIS
#============================
df.countries.ecuador.groups.admin1_cluster <- df.countries.ecuador.cluster.table %>% group_by(admin1,cluster) %>% count(cluster)
df.countries.chile.groups.admin1_cluster <- df.countries.chile.cluster.table %>% group_by(admin1,cluster) %>% count(cluster)
df.countries.venezuela.groups.admin1_cluster <- df.countries.venezuela.cluster.table %>% group_by(admin1,cluster) %>% count(cluster)

#============================
#MAP ANALYSIS
#============================
#https://rstudio.github.io/leaflet/shapes.html
build_map(df.countries.ecuador.cluster.table)
build_map(df.countries.chile.cluster.table)
build_map(df.countries.venezuela.cluster.table)

#============================
#DOWNLOAD SECTION
#============================
write.table(df.countries.ecuador.cluster.table, file="df.countries.ecuador.cluster.table.csv", sep=",")
write.table(df.countries.ecuador.coords.distance_harversine, file="df.countries.ecuador.coords.distance_harversine.csv", sep=",")

write.table(df.countries.chile.cluster.table, file="df.countries.chile.cluster.table.csv", sep=",")
write.table(df.countries.chile.coords.distance_harversine, file="df.countries.chile.coords.distance_harversine.csv", sep=",")

write.table(df.countries.venezuela.cluster.table, file="df.countries.venezuela.cluster.table.csv", sep=",")
write.table(df.countries.venezuela.coords.distance_harversine, file="df.countries.venezuela.coords.distance_harversine.csv", sep=",")



