# initiate library
library(shiny)
library(shinydashboard)

# header
header <- dashboardHeader(title="PPRK")

# sidebar
sidebar <- dashboardSidebar(
  width=280,
  sidebarMenu(
    menuItem("Page 1", tabName = "pageOne"),
    menuItem("Page 2", tabName = "pageTwo"),
    menuItem("Page 3", tabName = "pageThree")
  )
)

# body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "pageOne",
            h2("Page 1"),
            fileInput("vars1", "name1", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("vars2", "name2", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("vars3", "name3", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("vars4", "name4", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("vars5", "name5", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("vars6", "name6", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("vars7", "name7", buttonLabel="Browse...", placeholder="No file selected"),
            fileInput("vars8", "name8", buttonLabel="Browse...", placeholder="No file selected")
    ),
    
    tabItem(tabName = "pageTwo",
            h2("Page 2")
            
    ),
    
    tabItem(tabName = "pageThree",
            h2("Page 3")
    )
  )
)


# Setup UI shiny
# Dashboard page
ui <- dashboardPage(
  skin = 'black', 
  header,
  sidebar,
  body
)

# Define server 
server <- function(input, output) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)
