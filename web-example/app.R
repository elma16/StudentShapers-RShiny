library(tidyverse)
library(tibbletime)
library(glue)

# reactivePoll update functions
last_timestamp <- function(){
  sql_query <- "select max(time) as current_time from public.speed_test"
  dbGetQuery(con, sql_query)[1,1]
}

update_data <- function(){
  sql_query <- "SELECT * FROM public.speed_test ORDER BY time"
  dbGetQuery(con, sql_query)
}

# Database connection
connection_string <- glue("Driver={{PostgreSQL ANSI}};\\
                          Uid={Sys.getenv('MY_UID')};\\
                          Pwd={Sys.getenv('MY_PWD')};\\
                          Server={Sys.getenv('MY_REMOTE')};\\
                          Port=5432;\\
                          Database=movistar;")
con <- dbConnect(odbc::odbc(), .connection_string = connection_string, encoding = "utf8")

# UI
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Internet Speed"),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      fluidRow(
                        box(plotOutput('speed_plot'),
                            width = 12
                        )
                      )
                    )
)

# Server response
server <- function(input, output, session) {
  
  # Check for new data every 5 seconds
  pollData <- reactivePoll(5000, session,
                           checkFunc = last_timestamp,
                           valueFunc = update_data
  )
  
  # Connection speed plot
  output$speed_plot <- renderPlot({
    pollData() %<>%
      tail(24) %>% 
      as_tbl_time(index = time) %>% 
      collapse_by('1 hour', side = 'start', clean = TRUE) %>% 
      gather(type, speed, Download, Upload) %>%
      ggplot(aes(x = time, y = speed)) +
      geom_line(aes(color = type)) +
      geom_point() +
      labs(title = 'Connection Speed',
           subtitle = "Last 24 hours",
           x = 'Time',
           y = 'Speed (Mbits/s)',
           color = 'Traffic Type:') +
      scale_x_datetime(date_breaks = "1 hour",
                       date_labels = '%b-%d %H:%S',
                       expand = c(0.01,0.01)) +
      scale_y_continuous(breaks = seq(0, 22, by = 2), limits = c(0, 23)) +
      NULL 
  })
}

onStop(function() {
  # Close connection on exit
  dbDisconnect(con)
})

shinyApp(ui, server)