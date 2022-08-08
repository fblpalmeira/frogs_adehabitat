# Oregon Spotted Frog

# Read in the data manually
frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')
View(frogs)

library(dplyr)

#Create a new column with categorical value to gender
frogs <- frogs %>% 
  mutate (Gender=case_when(Female==0 ~ "Male",
                           Female==1 ~ "Female"), .after=Female)

#Rename some columns to run Adehabitat
frogs <- frogs %>%
  rename(id = "Frequency",
         x = "UTME_83",
         y = "UTMN_83") 

#Plot locations of all individuals
png(file="Fig1_Individual_Locations.png", width = 400, height = 400)
plot(frogs$y~frogs$x, 
     col = as.factor(frogs$id), 
     pch = 16, xlab="X (UTM)", ylab="Y (UTM)")
dev.off()

# Calculate the trajectory among locations for each individual
library(adehabitatLT)  
png(file="Fig2_frogs.ltraj.png", width = 400, height = 400)
frogs.ltraj <- as.ltraj(xy = frogs[,c("x", "y")], id = frogs$id, typeII=FALSE)
plot(frogs.ltraj)
frogs.ltraj 
dev.off()

t1<-frogs.ltraj[[1]]# The first six locations of the first animal
t2<-frogs.ltraj[[2]]
t3<-frogs.ltraj[[3]]
t4<-frogs.ltraj[[4]]
t5<-frogs.ltraj[[5]]
t6<-frogs.ltraj[[6]]
t7<-frogs.ltraj[[7]]
t8<-frogs.ltraj[[8]]
t9<-frogs.ltraj[[9]]
t10<-frogs.ltraj[[10]]
t11<-frogs.ltraj[[11]]
t12<-frogs.ltraj[[12]]
t13<-frogs.ltraj[[13]]
t14<-frogs.ltraj[[14]]
t15<-frogs.ltraj[[15]]
t16<-frogs.ltraj[[16]]
t17<-frogs.ltraj[[17]]
t18<-frogs.ltraj[[18]]
t19<-frogs.ltraj[[19]]
t20<-frogs.ltraj[[20]]
t21<-frogs.ltraj[[21]]
t22<-frogs.ltraj[[22]]
t23<-frogs.ltraj[[23]]
t24<-frogs.ltraj[[24]]
t25<-frogs.ltraj[[25]]
t26<-frogs.ltraj[[26]]
t27<-frogs.ltraj[[27]]
t28<-frogs.ltraj[[28]]
t29<-frogs.ltraj[[29]]
t30<-frogs.ltraj[[30]]
t31<-frogs.ltraj[[31]]
t32<-frogs.ltraj[[32]]

frogs.ltraj_all<-rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,
                       t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,
                       t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,
                       t31,t32) #combine all trajectories
summary(frogs.ltraj_all)

frogs.traj<-merge(x=frogs,y=frogs.ltraj_all,by=c("x","y"),all.x=TRUE,all.y=TRUE)
View(frogs.traj)

# Boxplot of the distances travelled 
library (ggplot2)
png(file="Fig3_Gender_Trajetory.png", width = 500, height = 400)
colnames(frogs.traj) <- make.unique(names(frogs.traj))
p <- ggplot(frogs.traj, aes(Gender, dist, color=Gender))
p + geom_violin(width=1.4, scale = "count")+ #Scale maximum width proportional to sample size.  Default is to trim violins to the range of the data.
  geom_jitter(height = 0, width = 0.1, alpha=0.5)+
  geom_boxplot(width=0.1, alpha=0.2)+
  ggtitle("")+
  xlab("")+ylab("Distance travelled by week (meters)")+
  theme_bw() + 
  theme (panel.grid.major.y = element_blank(), 
         panel.grid.minor.y = element_blank()) + 
  theme (panel.grid.major.x = element_blank(), 
         panel.grid.minor.x = element_blank())
dev.off()

# Create a dataframe to hold all of the contents of bltu.paths with a column for id. 
# Put first element into the dataframe
total.path.df <- data.frame(frogs.ltraj[[1]], id = attr(frogs.ltraj[[1]], "id"))
# Use a 'for' loop to fill the larger dataframe with the rest of the trajectories.
for(i in 2:length(frogs.ltraj)) {
  total.path.df <- rbind(total.path.df, 
                         data.frame(frogs.ltraj[[i]], id = attr(frogs.ltraj[[i]], "id")))
}

# Calculate distance travelled per interval and add it to the dataframe
total.path.df$distperweek <- total.path.df$dist / (total.path.df$dt/60/60/24)

# Aggregate to show mean distance per interval for each turtle
path.summary <- aggregate(distperweek~id, data = total.path.df, FUN = mean)
path.summary$sd <- aggregate(distperweek~id, data = total.path.df, FUN = sd)$distperweek

# Look at summmary dataframe
path.summary
summary(path.summary)
# Make a graph to visualize data using ggplot
library(ggplot2)
# Create limits used for error bars in graph
limits <- aes(ymax = path.summary$distperweek + path.summary$sd, 
              ymin = path.summary$distperweek - path.summary$sd)

# Make plot. Choose the dataframe (data) and aesthetics (aes; for the x and y)
png(file="Fig4_path.plot_Individual.png", width = 1200, height = 600)
path.plot <- ggplot(data = path.summary, aes(x = id, y = distperweek, colour = id)) + 
  geom_point(size = 3) + # add points
  geom_errorbar(limits, width = 0.2) + # adds error bars
  labs(x = "Animal number", 
       y = "Mean distance travelled per week (m)" ) + # Axis labels
  theme_classic() + # Make plot black and white with no background grid
  theme(legend.position = "none")
path.plot # call plot
dev.off()

library(janitor) #Abrir o pacote 'janitor' para limpar algumas linhas de dados. MCP roda a partir de 5 localizações (x,y).
frogs<-frogs[-c(289,290,291,292,293,294), ]
frogs <- frogs[!is.na(frogs$x) & !is.na(frogs$y),]
# Only include three columns (id, x, and y coordinates) for making MCP's
frogs.sp <- frogs[, c("id", "x", "y")] 

# Create a SpatialPointsDataFrame by defining the coordinates
library(sp)
coordinates(frogs.sp) <- c("x", "y")
# Set the coordinate reference system (CRS)
# The sample data are UTM points in NAD83 from zone 10N
proj4string(frogs.sp) <- CRS( "+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs" )

library(adehabitatHR) # Load library
frogs.mcp1 <- mcp(frogs.sp, percent = 95, unout="ha")## estimates the MCP 95%
frogs.mcp1
plot(frogs.mcp1)## Plot the home ranges  MCP 95%
frogs.mcp95<-as.data.frame(frogs.mcp1)## Store the home-range size as dataframe
frogs.mcp95<- frogs.mcp95 %>%
  rename(MCP95 = "area") 

frogs.mcp2 <- mcp(frogs.sp, percent = 100, unout="ha")## estimates the MCP 100%
frogs.mcp2
plot(frogs.mcp2)## Plot the home ranges MCP 100%
frogs.mcp100<-as.data.frame(frogs.mcp2)## Store the home-range size as dataframe
frogs.mcp100<- frogs.mcp100 %>%
  rename(MCP100 = "area") 

frogs.mcp<-merge(x=frogs.mcp95,y=frogs.mcp100,by=c("id"),all.x=TRUE,all.y=TRUE)
View(frogs.mcp)
summary(frogs.mcp)

frogs.metrics<-merge(x=path.summary,y=frogs.mcp,by=c("id"),all.x=TRUE,all.y=TRUE)
View(frogs.metrics)

library(scales) # Helps make polygons partly transparent using the alpha argument below
png(file="Fig5_MCP95_Individual.png", width = 400, height = 400)
plot(frogs.sp, col = as.factor(frogs.sp@data$id), pch = 16)
plot(frogs.mcp1, col = alpha(1:30, 0.5), add = TRUE)
dev.off()

# Boxplot of the Minimum Convex Polygon Estimator MCP 95%

frogs.all<-merge(x=frogs.traj,y=frogs.metrics,by=c("id"),all.x=TRUE,all.y=TRUE)
View(frogs.all)

png(file="Fig6_Gender_MCP95.png", width = 500, height = 400)
colnames(frogs.all) <- make.unique(names(frogs.all))
p <- ggplot(frogs.all, aes(Gender, MCP95, color=Gender))
p + geom_violin(width=1.4, scale = "count")+ #Scale maximum width proportional to sample size.  Default is to trim violins to the range of the data.
  geom_jitter(height = 0, width = 0, alpha=0.5)+
  geom_boxplot(width=0.1, alpha=0.2)+
  ggtitle("")+
  xlab("Gender")+ylab("Home range - MCP 95% (hectares)")+
  theme_bw() + 
  theme (panel.grid.major.y = element_blank(), 
         panel.grid.minor.y = element_blank()) + 
  theme (panel.grid.major.x = element_blank(), 
         panel.grid.minor.x = element_blank())
dev.off()

library(magick)
library(magrittr) 

# Call back the plot
plot <- image_read("Fig3_Gender_Trajetory.png")
plot2<-image_annotate(plot, "", 
                      color = "green", size = 16,
                      location = "10+50", gravity = "north")
plot3<-image_annotate(plot2, "Data: Pearl et al. 2022 (doi.org/10.1670/20-060) | Image credit: @PhyloPic | Visualization by @fblpalmeira", 
                      color = "gray", size = 10, 
                      location = "10+50", gravity = "southeast")
# And bring in a logo
logo_raw <- image_read("http://www.phylopic.org/assets/images/submissions/104e8d55-3704-4c15-bade-2dfe3731f073.512.png") 
out<-image_composite(plot3,image_scale(logo_raw,"x30"), offset = "+360+50")
image_browse(out)

# And overwrite the plot without a logo
image_write(out, "Fig7_Gender_Trajetory2.png")
