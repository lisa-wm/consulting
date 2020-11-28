library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel(" "),
  
  sidebarPanel(
    # This is intentionally an empty object.
    h6(textOutput("save.results")),
    h5("Experiment zur Gewinnung der Grundstimmung in deutschsprachigen politischen Texten. Durchgang 1."),
    tags$a("Kontakt: Christian Rauh", 
           href="http://www.wzb.eu/de/personen/christian-rauh"),
    h6("Details zu Forschungszielen und Ergebnissen sind nach Umfrageteilnahme vom Projektleiter verfügbar."),
    h6(" "),
    h6("Die Abfragemaske basiert auf der Vorlage von Francis Smart:"),
    tags$a("Survey-Tool", 
           href=paste0("https://github.com/EconometricsBySimulation/",
                       "Shiny-Demos/tree/master/Survey")),
    # Display the page counter text.
    h6(textOutput("counter"))
  ),
  
  
  # Show a table summarizing the values entered
  mainPanel(
    # Main Action is where most everything is happenning in the
    # object (where the welcome message, survey, and results appear)
    uiOutput("MainAction"),
    # This displays the action putton Next.
    actionButton("Click.Counter", "Weiter")    
  )
))