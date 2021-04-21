#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

library(readr)





# Define UI for application that draws a histogram
ui <- fluidPage(
 #Title Page
    titlePanel("Determinant of terrorist attack casualties"),
    
    #Configure layout
    sidebarLayout(
        #Control Panel
        sidebarPanel(
            h3("Figure 1 Controls"),
            #Slider Input for Number of Fatalities
            sliderInput("fatalities",
                "Fatalities Caused By Different Terrortist Attacks",
                min = min(gtd_df$nkill, na.rm=TRUE),
                max = max(gtd_df$nkill, na.rm=TRUE),
                value=c(min(gtd_df$nkill, na.rm=TRUE), max(gtd_df$nkill, na.rm=TRUE)),
                sep="")
            ),
        #Main panel
        mainPanel(
            h1("Analyses"),
           h3("Fatalities Caused by Weapon Type"),
             plotOutput("gtd_plot"),
            p("Description text goes here.")
             )
    )
    
)
    


# Define server logic required to draw a histogram
server <- function(input, output) {
#Figure 1 Plot - Fatalities According to Weapon Type
    output$gtd_plot <- renderPlot({
        data <- gtd_raw %>% 
            filter(nkill >= input$fatalities [1] & nkill <= input$fatalities [2])%>% 
           group_by(nkill, weaptype1_txt) %>% 
            count()
        
          #Figure 1 Plot Call  
            data %>% 
             ggplot(aes(x=nkill, y=n, fill = weaptype1_txt)) +
            geom_bar(stat="identity")
    })
}
        

# Run the application 
shinyApp(ui = ui, server = server)
