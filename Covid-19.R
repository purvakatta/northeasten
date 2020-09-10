
covid <- read.csv("C:/Users/purva/OneDrive/Documents/covid19data.csv", header = TRUE, 
                stringsAsFactors = FALSE,)                                               ###my dataset
covid <- na.omit(covid)
covid$Most.Recent.Date <- mdy(covid$Most.Recent.Date)



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

header <- dashboardHeader(title = "Covid-19 in California", titleWidth=350, 
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = "500 New Cases Today",
                                         icon("users")
                                       ),
                                       notificationItem(
                                         text = "20 Recovered",
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
    inputId = "County.Name",
    label = "County Name",
    choices = covid$County.Name,
    selected = "Los Angeles",
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
      title = "Top 50 CA Counties by Deaths"             #### Graph 1
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("groupcases", height = "300px")
  )

  ,box(
    title = "Positive Covid-19 Patients Trend"
    ,status = "primary"                                                  ######## Graph 2
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("totalcnf", height = "300px")
  ) 
  
  ,box(
    title = "Positive vs Suspected ICU Cases"
    ,status = "primary"
    ,solidHeader = TRUE                                       ##### Graph 3
    ,collapsible = TRUE 
    ,plotOutput("icucases", height = "300px")
 )
 
 
 ,box(
   title = "Confirmed vs Deaths"
   ,status = "primary"
   ,solidHeader = TRUE                                       ##### Graph 3
   ,collapsible = TRUE 
   ,plotOutput("deaths", height = "300px")
 )
 
 )

########################################################################################################## 
############################################combine the two fluid rows to make the body

body <- dashboardBody(frow1, frow2)


########################################################################################################
###################################################################completing the ui part with dashboardPage


ui <- dashboardPage(title = 'COVID-19 in California', header, sidebar, body, skin='green')



########################################################################################################
####################################################create the server functions for the dashboard  

  server <- function(input, output) { 
  
#######################################################################################################
##################################some data manipulation to derive the values on the boxes on top
    
    total.p <- sum(covid$Total.Count.Confirmed)
    
    total.d <- sum(covid$Total.Count.Deaths)
    
    total.s <- sum(covid$Suspected.COVID.19.Positive.Patients)
    

  

 ############################################################################################################# 
  ############################################### creating the valueBoxOutput content
  
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total.p, format="d", big.mark=',')
      ,'Total Confirmed Cases In CA'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")
  })
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(total.d, format="d", big.mark=',')
      ,'Total Deaths In CA'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "navy" )
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(total.s, format="d", big.mark=',')
      ,'Total Suspected In CA'
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
  })
  
 ################################################################# creating the plotOutput content

  #################################### Graph 1 ###################################################
  p_top <- covid %>% 
    group_by(County.Name, COVID.19.Positive.Patients, 
             Suspected.COVID.19.Positive.Patients) %>%
    tally() %>%
    collect() %>%                                       
    arrange(desc(n)) %>%
    head(50) 
  
  
 output$groupcases <- renderPlot({
   ggplot(p_top, aes(x=COVID.19.Positive.Patients,
                     y=Suspected.COVID.19.Positive.Patients,
                     colour=County.Name)) +
     geom_point(size = 4) + 
     geom_text(aes(label=County.Name), size=2.2, 
               nudge_x = 0.07, nudge_y = 0.07, 
               check_overlap = T)   + 
     ggtitle("INYO County has the Highest Cases")
     
  })
 

########################################## Graph 2 #############################################################
 
 output$totalcnf <- renderPlot({                   
   ggplot(data = covid, aes(Most.Recent.Date ,
                            y = Total.Count.Confirmed)) + 
     geom_line(color = "#00AFBB", size = 1) + 
     xlab("Date") + 
     ylab("Total Confirmed Cases") + ggtitle("Covid 19 Cases exponentially Increased")
 
 })

############################################# Graph 3 ########################################################   

 a <- sum(covid$ICU.COVID.19.Positive.Patients)
 b <- sum(covid$ICU.COVID.19.Suspected.Patients)
 icu_cases <- data.frame(ICU_Cases=c("Positive Patients", "Suspected Patients"),
                 Total_Count=c(68165L, 19468L))
 
 output$icucases <- renderPlot({
   ggplot(data=icu_cases, aes(x=ICU_Cases, y=Total_Count)) + xlab("ICU Cases") + ylab("Total Count") + 
     geom_bar(stat="identity", fill="steelblue")+ ggtitle("Positive Cases are much more than Suspected ") + 
     theme_minimal()
   
 })
 
 
########################################### Graph 4 ######################################################## 
 
 p_death <- covid %>% 
   group_by(County.Name, Total.Count.Confirmed, Total.Count.Deaths) %>%
   tally() %>%
   collect() %>%  
   arrange(desc(Total.Count.Deaths)) %>%
   head(500)
  
 output$deaths <- renderPlot({
   ggplot(p_death, aes(x=Total.Count.Confirmed, y=Total.Count.Deaths, group = County.Name)) +
     geom_line(aes(color = County.Name ), size = 2) +
     
     ylab("Total Deaths") + 
     xlab("Total confirmed")+
     ggtitle("LA has highest Cases") 
   
 })

 
###########################################################################################   
  }
  
shinyApp(ui, server)



