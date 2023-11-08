#read csv
setwd("/Users/fantasy2fry/Documents/informatyczne/iadstudia/twd/fast_food_data_analysis_project/spider_plot")
df_fast_food <- read.csv("../data/fastfood.csv")
df_restaurants <- read.csv("../data/Fast_Food_Restaurants_US.csv")

library(fmsb)
library(ggplot2)
library(dplyr)
library(tidyr)
# install.packages("devtools")
# devtools::install_github("ricardo-bion/ggradar")
#library(ggradar)

logos_links=c('https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/Arby%27s_logo.svg/640px-Arby%27s_logo.svg.png',
              'https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/Burger_King_logo_%281999%29.svg/2024px-Burger_King_logo_%281999%29.svg.png',
              'https://upload.wikimedia.org/wikipedia/commons/thumb/0/02/Chick-fil-A_Logo.svg/2560px-Chick-fil-A_Logo.svg.png',
              'https://upload.wikimedia.org/wikipedia/commons/thumb/a/ae/Dairy_Queen_logo.svg/1200px-Dairy_Queen_logo.svg.png',
              'https://upload.wikimedia.org/wikipedia/commons/thumb/3/36/McDonald%27s_Golden_Arches.svg/1200px-McDonald%27s_Golden_Arches.svg.png',
              'https://download.logo.wine/logo/Sonic_Drive-In/Sonic_Drive-In-Logo.wine.png',
              'https://1000logos.net/wp-content/uploads/2017/06/Subway-Logo-2002.png',
              'https://static.wikia.nocookie.net/logopedia/images/4/45/Taco_Bell.svg/revision/latest?cb=20200112222740')


df_restaurants_prepaired_for_spider_plot=df_fast_food %>% 
  group_by(restaurant) %>%
  summarise(average_calories=mean(calories),
            average_total_fat=mean(total_fat),
            average_cholesterol=mean(cholesterol),
            average_carbohydrates=mean(total_carb),
            average_sugar=mean(sugar),
            average_protein=mean(protein, na.rm = TRUE))

df_restaurants_prepaired_for_spider_plot=
  df_restaurants_prepaired_for_spider_plot %>% 
  mutate(average_calories=100*average_calories/max(average_calories),
         average_total_fat=100*average_total_fat/max(average_total_fat),
         average_cholesterol=100*average_cholesterol/max(average_cholesterol),
         average_carbohydrates=100*average_carbohydrates/max(average_carbohydrates),
         average_sugar=100*average_sugar/max(average_sugar),
         average_protein=100*average_protein/max(average_protein))

df_max_min_for_spider_plot=
  df_restaurants_prepaired_for_spider_plot %>%
  summarise(max_calories=max(average_calories),
            max_total_fat=max(average_total_fat),
            max_cholesterol=max(average_cholesterol),
            max_carbohydrates=max(average_carbohydrates),
            max_sugar=max(average_sugar),
            max_protein=max(average_protein),
            min_calories=min(average_calories),
            min_total_fat=min(average_total_fat),
            min_cholesterol=min(average_cholesterol),
            min_carbohydrates=min(average_carbohydrates),
            min_sugar=min(average_sugar),
            min_protein=min(average_protein))
df_max_min_for_spider_plot=as.data.frame(t(df_max_min_for_spider_plot))
df_max_min_for_spider_plot=rbind(df_max_min_for_spider_plot[1:6,],
                                 df_max_min_for_spider_plot[7:12,])

#########################
#create spider plot for every restaurant
df=df_restaurants_prepaired_for_spider_plot[1,2:7]
name=df_restaurants_prepaired_for_spider_plot[1,1]
df=as.data.frame(t(df))
rownames=row.names(df)
df=cbind(rownames,df)
colnames(df)=c("name","value")
#radar plot
ggplot(df, aes(x = name, y = value)) +
  geom_point(size=3)+
  geom_polygon(aes(group=1),alpha=0.4)+
  coord_polar(start = 0) +
  theme_minimal()

ggplot(df, aes(x = name, y = value)) +
  geom_point(size = 3) +  # Kropki
  geom_line(aes(group = 1), linetype = "solid", color = "blue") +  # Poprawnie połączone linie
  coord_polar(start = 0)+
  theme_minimal()

#https://rpubs.com/tshapiro/super-radar-plots

ggplot(df,aes(x=name,y=value))+
  geom_col()+
  coord_polar()


library(geomtextpath)

#custom dataframe for line segments
segments<- data.frame(
  x1=rep(0,5),
  x2=rep(5.5,5),
  y1=c(0,25,50,75,100),
  y2=c(0,25,50,75,100)
)

plot<-ggplot(df, aes(x=name, y=value, fill=name))+
  #circular coordinates
  coord_polar()+
  #blank canvas
  theme_void()+
  #new x labels
  geom_textpath(inherit.aes=FALSE,
                mapping=aes(x=name, label=name, y=130),
                fontface="bold", upright=TRUE, text_only=TRUE, size=3)+
  #new grid lines - leave space to add in our y axis labels later
  geom_segment(inherit.aes=FALSE,
               data = segments,
               mapping=aes(x=x1,xend=x2,y=y1,yend=y2), size=0.35)

plot

labels<-data.frame(
  y = c(25,50,75,100),
  x = rep(0.25,4)
)
plot<-plot+
  #overlay dataset
  geom_col(width=.8, show.legend = FALSE)+
  #create donut hole and add some room at the top with limits (our labels are at 130)
  scale_y_continuous(limits=c(-70,135))+
  #add the y axis labels
  geom_textsegment(inherit.aes=FALSE,
                   data=labels,
                   mapping=aes(x=5.5, xend=6.5, y= y, yend=y, label=y),
                   linewidth=0.35, size=2.5)

plot


library(ggimage)

#link to png file
image<-'https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/Arby%27s_logo.svg/640px-Arby%27s_logo.svg.png'


plot+
  #add portrain in center
  geom_image(mapping=aes(y=-70,x=1), image=image, size=0.225)+
  #customize bar colors
  scale_fill_manual(values=c("#E1341A","#FF903B","#ffe850","#27f897",
                             "#4bd8ff" ,"#6F02CE"))+
  #add plot titles and labels
  labs(title="BLACK WIDOW",
       subtitle="Power Stats by Superhero. Abilities scaled from 0 to 100.",
       caption = "Data from Superhero API | Graphic @tanya_shapiro")+
  #make some tweaks to plot theme
  theme(
    plot.title=element_text(face="bold", hjust=0.5, size=18, vjust=-2),
    plot.subtitle=element_text(hjust=0.5, vjust=-5),
  )



#####################
# true radar plot
# for every restaurant



for (i in 1:8) {
  df=df_restaurants_prepaired_for_spider_plot[i,2:7]
  name=df_restaurants_prepaired_for_spider_plot[i,1]
  df=as.data.frame(t(df))
  rownames=row.names(df)
  df=cbind(rownames,df)
  colnames(df)=c("name","value")
  df=pivot_wider(df,names_from = name,values_from = value)
  colnames(df_max_min_for_spider_plot)=colnames(df)
  df=rbind(df_max_min_for_spider_plot ,df)
  #radar plot
  png(paste("radar_plot_",name,".png",sep=""))
  radarchart(df, axistype=1 , 
             pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , plty=1, 
             cglcol="grey", cglty=2, axislabcol="white", cglwd=2,vlcex=0.8 )+
    title(paste("Stats for",name[[1]] ,"brand"))
  dev.off()
  }






ggplot(df, aes(x=name,y=value))+
  coord_polar()+
  theme_minimal()+
  geom_point()+geom_line(aes(group=1),linetype="solid",color="blue")+
  labs(title=name)+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0, 100)
