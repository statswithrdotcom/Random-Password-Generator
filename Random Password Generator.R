# library required packages
library(shiny)
library(shinythemes)
library(bslib)
library(rclipboard)


# Generates random passwords
Pass_Generator <- function(Minimum = 15, Maximum = 20, Seed, Action){

  # Sets the possible characters
  Pos <- as.character(c(letters,LETTERS,"?",".","!",0:9,"'",
                        "/","+","-","<",">","@","$","%","#","^","&",
                        "*","(",")","_","=",","))
  
  # Set the initial seed and increments each time the action button is clicked
  Seed_Factors = (as.numeric(Seed) %% 10^9) + Action + round(100*proc.time()[3],0)
  set.seed(Seed_Factors %% 10^9) # The %% ensures the number can not be over 10^9
    
  # Randomly determines how long the password will
  Pass_Length <- sample(Minimum:Maximum, size = 1)
  
  # Randomly assigns characters in the "Pos" vector to the password
  Pass_Indexes <- sample(1:length(Pos), size = Pass_Length, replace = TRUE)
  
  # Puts it all together into a randomly generated password
  Generated_Pass <- paste(Pos[Pass_Indexes], collapse = "")
  
  # Returns the password
  return(Generated_Pass)

}



# The user interface for the application
ui <- fluidPage(theme = shinytheme("cosmo"),
                rclipboardSetup(),
                navbarPage(
                  # Title for overall application
                  "Random Password Generator",
                  # Page for the scramble functionality
                  tabPanel("Generate",
                           sidebarLayout(
                             sidebarPanel(
                               textInput("Min",
                                         "The minimum password length",
                                         value = 15),
                               textInput("Max",
                                         "The maximum password length",
                                         value = 20),
                               textInput("Start",
                                         "The seed for this session",
                                         value = sample(1:10^9,1)),
                               actionButton(inputId = "action", label = "Submit")
                             ),
                             mainPanel(
                               h1("The Generated Password(s)"),
                               h6("The password generator depends on the seed, the submit button 
                                  that generates a new password after every click, and the system time,
                                  ensuring every user should get unique results."),
                               verbatimTextOutput("Password"),
                               uiOutput("clip1"),
                               h4("Made by statswithr.com")
                             )
                           ) 
                           
                  )
                  
            
                )
)

# The server portion of the application
server <- function(input, output) {
  

  # The text output rendered on screen
  output$Password <- renderText({
    Pass_Generator(Minimum = input$Min, Maximum = input$Max, 
                   Action = input$action, Seed = input$Start)
  })
  
  
  # The text output given to the copy button
  output$clip1 <- renderUI({
    rclipButton("clipbtn",label = "Copy Password",
                clipText = Pass_Generator(Minimum = input$Min, Maximum = input$Max, 
                                          Action = input$action, Seed = input$Start)
    )
  })
  
  
}

# Connects the ui and server into a single application
shinyApp(ui = ui, server = server)
