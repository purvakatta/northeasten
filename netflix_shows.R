
nfs <- read.csv("C:/Users/purva/OneDrive/Documents/Netflix Shows.csv", header = TRUE, 
                stringsAsFactors = FALSE,)                                               ###my dataset

nfs<-na.omit(nfs)
nfs$User.Rating.Score <- as.numeric(nfs$User.Rating.Score)  ### to remove NAs


dim(nfs)



library(shiny)  #for web applications
library(shinydashboard)
library(DataExplorer)
library(scales)
library(dplyr)
library(ggplot2)

############################################################################################################################
# The customizations for this dashboard are : 3 value boxes, 4 ggplots, dashboard & dropdown menu on the left #
############################################################################################################################


##############################################################################################################
############################################################################################################
#######################Dashboard header carrying the title of the dashboard and STATIC NOTIFICATIONS MENU

header <- dashboardHeader(title = "Netflix Shows", titleWidth=350, 
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = "50 New Subscribers Today",
                                         icon("users")
                                       ),
                                       notificationItem(
                                         text = "20 complaints resolved",
                                         icon("question"),
                                         status = "success"
                                       ),
                                       notificationItem(
                                         text = "Server load at 86%",
                                         icon = icon("exclamation-triangle"),
                                         status = "warning"
                                       )
                          ))
                          

########################################################################################################
########################################################Sidebar Content of the dashboard

sidebar <- dashboardSidebar(
  selectInput(
    inputId = "Title",
    label = "Current shows:",
    choices = nfs$Title,
    selected = "13 Reasons Why",
    selectize = FALSE                           #####Drop-down List on the sidebar
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)


frow2 <- fluidRow(
  
  box(
      title = "No. of Shows by Maturity Ratings"             #### Graph 1
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("grouprating", height = "300px")
  )

  ,box(
    title = "Percent of Kids Shows on Netflix"
    ,status = "primary"                                                  ######## Graph 2
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("percentshows", height = "300px")
  ) 
  
  ,box(
    title = "Top 20 Shows Running"
    ,status = "primary"
    ,solidHeader = TRUE                                       ##### Graph 3
    ,collapsible = TRUE 
    ,plotOutput("top.shows", height = "300px")
 )
 ,box(
   title = "Current Shows"
   ,status = "primary"                                      #### Graph 4
   ,solidHeader = TRUE 
   ,collapsible = TRUE 
   ,plotOutput("runningshows", height = "300px")
 )
)

########################################################################################################## 
############################################combine the two fluid rows to make the body

body <- dashboardBody(frow1, frow2)


########################################################################################################
###################################################################completing the ui part with dashboardPage


ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='green')



########################################################################################################
####################################################create the server functions for the dashboard  

  server <- function(input, output) { 
  
#######################################################################################################
##################################some data manipulation to derive the values on the boxes on top
    
    total.count <- unique(nfs$Title)
    length(total.count)
  
  show.min.rating <- nfs %>% 
    arrange(User.Rating.Score) %>%
    select(User.Rating.Score) %>% 
    filter(row_number() ==1)
    
  
  show.max.rating <- nfs %>% 
    arrange(desc(User.Rating.Score)) %>%
    select(User.Rating.Score) %>%
    filter(row_number() ==1)
  

 ############################################################################################################# 
  ############################################### creating the valueBoxOutput content
  
  output$value1 <- renderValueBox({
    valueBox(
      formatC(length(total.count), format="d", big.mark=',')
      ,'Netflix original Shows'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")
  })
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(show.min.rating, format="d", big.mark=',')
      ,'Minimum User Rating'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "navy")
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(show.max.rating, format="d", big.mark=',')
      ,'Maximum User Rating'
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
  })
  
 ################################################################# creating the plotOutput content

  #################################### Graph 1 ###################################################
  
 nfs_top.shows<- nfs %>% 
   group_by(Rating) %>%
   tally() %>%
   collect() %>%                         #### collected total shows according to Ratings
   arrange(desc(n)) %>%
   head(14)
  
  
 output$grouprating <- renderPlot({
    ggplot(nfs_top.shows, aes(x=Rating, y=n)) +
     geom_bar(stat="identity", show.legend = TRUE,
              fill = c("light blue","light grey","light grey","light grey","light grey","light grey",
                       "light grey","light grey", "light grey", "light grey", "light grey", "light grey"
                       ))+
     geom_text(aes(label= n), position=position_dodge(0.9),vjust=-0.2) + 
     
     ylab("No. of Shows") + 
     xlab("Rating ")+
     ggtitle("Majority of Netflix Shows are for Adults (TV-14) ")
     
  })
 

########################################## Graph 2 #############################################################

 dfs <- data.frame(Rating = c('G', 'PG', 'Other'), cnt = c(138, 170, 498))
 
 
 bp <- ggplot(dfs, aes(x="", y=cnt, fill=Rating))+
   geom_bar(width = 1, stat = "identity")
 bp                                                  ###barplot needed to make Pie Chart
  
 
 pie <- bp + coord_polar("y", direction = 1)
 
 output$percentshows <- renderPlot({                   ##dividing by 1000( total no. of shows) to get percentage
   pie + 
     theme(axis.text.x=element_blank()) +
     geom_text(aes(y = cnt/3 + c(0, cumsum(cnt)[-length(cnt)]), 
                   label = percent(cnt/1000)), size=7, position = position_stack(vjust = 0.5)) +
     scale_fill_brewer(palette="GnBu")+ggtitle("Kids Shows count for just 30% of Total Shows") + 
     theme_minimal() + 
     labs(x = NULL)
 
 })

############################################# Graph 3 ########################################################   

 nfs_top<- nfs %>% 
   group_by(Rating, User.Rating.Score, Title) %>%
   tally() %>%
   collect() %>%                                         #### Collecetd the Top 20 Shows and found out
                                                         #### that only 3 Kids Shows are in Top 20
   arrange(desc(User.Rating.Score)) %>%
   head(20) 
 
 output$top.shows <- renderPlot({
   ggplot(nfs_top, aes(x=Rating, y=User.Rating.Score, colour=Title)) +
     geom_point(size = 4) + geom_text(aes(label=Title), size=2.2, nudge_x = 0.07, nudge_y = 0.07, 
                                      check_overlap = T)   + 
     ggtitle("Only 3 Kids Shows in Top 20")
   
 })
 
 
########################################### Graph 4 ######################################################## 
 
 library(tidyverse) 
 
 library(hrbrthemes)
 
 library(viridis)
 library(plotly)
 library(d3heatmap)
 library(heatmaply)
 
 nfs_kids <- nfs %>%
   group_by(Title, Rating, User.Rating.Score) %>%
   filter(Rating %in% c("G", "PG", "TV-G", "TV-Y", "TV-Y7", "TV-Y7-FV")) %>%
   tally() %>%
   collect() %>%
   arrange(desc(User.Rating.Score))%>%            ####collected the Top 50 Kids Shows
   head(30)
 
 
 nfs_kids <- nfs_kids[-4]
 nfs_kids <- data.matrix(nfs_kids[1:ncol(nfs_kids)])    ## turned the df into matrix to plot heatmap

 output$runningshows <- renderPlot({
   
   heatmaply(nfs_kids,
             dendrogram = "none",
             xlab = "", ylab = "",
             main = "",
             scale = "column",
             margins = c(60,100,40,20),
             grid_color = "white",
             grid_width = 0.00011,
             titleX = TRUE,
             hide_colorbar = TRUE,
             branches_lwd = 0.1,
             label_names = c("Title", "Rating:", "Score"),
             fontsize_row = 7, fontsize_col = 10,
             labCol = colnames(nfs_kids),
             labRow = rownames(nfs_kids),
             heatmap_layers = theme(axis.line=element_blank())
   )                               
   
   
 })
 
###########################################################################################   
  }
  
shinyApp(ui, server)



