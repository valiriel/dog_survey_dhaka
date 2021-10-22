#remotes::install_github("r-spatial/mapview")
library(sf); library(tidyverse); library(mapview); mapviewOptions(fgb = FALSE); library(leafem)
mapviewOptions(vector.palette = viridisLite::viridis, alpha=1, legend=F, basemaps = c("OpenStreetMap"), legend.opacity = 0)
rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  st_geometry(g)=name
  g
}
#'------------------------------------------------------------------------------------------------------

p <- 0.5 # expected survey answer probability, 50/50 as unknown atm

z <- 1.96 # standard Z-score for the desired level of confidence (1.96 for a 95% confidence interval)

# https://en.wikipedia.org/wiki/Margin_of_error
w <- 0.1 # +/- 5% intervals

# formula from = https://en.wikipedia.org/wiki/Sample_size_determination
# double checked with https://www.surveymonkey.co.uk/mp/sample-size-calculator/ which assumes 50/50

# naive sample size
n.p.w <- round((4*(z^2)*p*(1-p))/(w^2))

# We also need to account for the possibility that closest household does not answer, 
# let's account for a 20% response rate.

sample <- ceiling(n.p.w + (n.p.w/100)*80)

# 900 points, 4 hhs per point, 15 groups, 5 days is the final

dhaka_building <- read_sf("dsd_data/shps/simplified_dhaka_large_buildings.shp")
dhaka_building$ID <- 1; dhaka_building <- summarise(group_by(dhaka_building,ID))

survey_points <- st_sample(dhaka_building, size=900) %>% st_transform(crs=4326) %>% st_as_sf()
survey_points <- rename_geometry(survey_points, "geometry")
mapview(survey_points)

#'------------------------------------------------------------------------------------------------------
#' * cluster into 15 groups* 

A <- data.frame(st_coordinates(survey_points)); mean <- nrow(A)/15

b <- T;i<-1
while (b == TRUE) {
  
  clusters <- kmeans(A, centers = 15, nstart=1, iter.max = 1) # 15 groups = 15 centers
  #plot(A, col = clusters$cluster)  
  i<-i+1; print(i)
  if(sum(clusters$size<(mean-10)) == 0 & sum(clusters$size>(mean+10)) == 0)
    b <- F
}

survey_points$main_cluster <- clusters$cluster; mapview(survey_points, zcol="main_cluster")
# save(survey_points, file="dsd_data/points_main_cluster.rda")
load("dsd_data/points_main_cluster.rda")

#'------------------------------------------------------------------------------------------------------
#' * cluster main 15 groups into 5 subgroups to reflect days and avoid extra travel* 

for (k in 1:15) {
  
  sub_clusters <- st_transform(survey_points, crs=4326) %>% filter(main_cluster == k)
  
  A <- data.frame(st_coordinates(sub_clusters))
  mean <- nrow(A)/5
  
  b <- T;i<-1
  while (b == TRUE) {
    
    clusters <- kmeans(A, centers = 5, nstart=1, iter.max = 1) # 5 days, 60 points/5 = 12 per day approx.
    #plot(A, col = clusters$cluster)  
    i<-i+1; print(paste0(k, "___", i))
    if(sum(clusters$size<floor(mean-2)) == 0 & sum(clusters$size>ceiling(mean+2)) == 0)
      b <- F
  }
  
  sub_clusters$sub_cluster <- clusters$cluster; sub_clusters <- arrange(sub_clusters, sub_cluster)
  
  sub_clusters <- sub_clusters %>% 
    transmute(G_ID = paste0("G",k,"_", str_pad(1:nrow(sub_clusters), width=2, side="left", pad="0")),
               group = k, 
               clusters = sub_cluster,
               x = st_coordinates(sub_clusters)[,"X"],
               y = st_coordinates(sub_clusters)[,"Y"])
  
  sub_clusters$link <- paste0("https://www.google.com/maps/search/?api=1&query=", 
                              str_sub(sub_clusters$y,1,11), ",", str_sub(sub_clusters$x,1,11))
  
  m <- mapview(sub_clusters, zcol="clusters", cex=9, label=sub_clusters$G_ID) #%>% 
        #addStaticLabels(label=sub_clusters$G_ID, noHide = TRUE, textOnly = TRUE, 
                        #textsize = "15px", permanent=TRUE, offset=c(20,0))
  
  dir.create(paste0("dsd_output/groups_samples/group_",k))
  mapshot(m, file=paste0("dsd_output/groups_samples/group_",k,"/group_",k,".pdf"))
  mapshot(m, paste0("dsd_output/groups_samples/group_",k,"/group_",k,".html"))
  
  csv <- data.frame(ID = sub_clusters$G_ID, 
                    group = sub_clusters$group, 
                    day_cluster = sub_clusters$clusters, 
                    link = sub_clusters$link)
  write.csv(csv, file= paste0("dsd_output/groups_samples/group_",k,"/group_",k,".csv"), row.names = F)

  if (k == 1) data <- sub_clusters
  if (k > 1) data <- rbind(data, sub_clusters)
  
}

write.csv(as.data.frame(data), file = "dsd_output/clustered_points_final.csv", row.names = F)
save(data, file="dsd_output/clustered_points_final.rda")
load("dsd_output/clustered_points_final.rda")

mapview(data, zcol="group") + mapview(data, zcol="clusters")
