library(shiny)

ui <- fluidPage(

    titlePanel("Titanic Ticket Booth"),

    sidebarLayout(
        sidebarPanel(

            textInput("nameInput", "Name", placeholder = "Name"),
            textInput("surnameInput", "Surname", placeholder = "Surname"),
            selectInput("sexChoiceInput", "Sex", c("male","female")),
            numericInput("ageInput", "Age", NULL, min=0, max=120),
            selectInput("ticketClassInput", "Ticket Class", list("1st" = 1, "2nd" = 2, "3rd" = 3)),
        ),

        mainPanel(
           verbatimTextOutput("ticketDetails"),
           plotOutput("titanicAge"),
           plotOutput("titanicFare"),
           plotOutput("titanicPClass"),
           plotOutput("titanicSex")
        )
    )
)


server <- function(input, output) {
    
    output$ticketDetails <- renderText({
        validate(
            need(input$nameInput, "Name field cannot be empty!"),
            need(input$surnameInput, "Surname field cannot be empty!"),
            need(input$sexChoiceInput, "Sex field cannot be empty!"),
            need(input$ageInput, "Age field cannot be empty!"),
            need(input$ageInput > 0 && input$ageInput <= 120, "Invalid Age value!"),
            need(input$ticketClassInput, "Ticket Class field cannot be empty!")
        )
        
        paste("**TICKET**",
              paste("NAME:\t", input$nameInput, input$surnameInput),
              paste("SEX:\t", input$sexChoiceInput),
              paste("AGE:\t",input$ageInput),
              paste("CLASS:\t",input$ticketClassInput),
              paste("FARE:\t",get_prediction()),
              sep="\n")
        
    })
    
    output$titanicAge <- renderPlot({
        
        df = titanic_df()
        
        hist(df$Age, col = "blue", border = "white", breaks=25, main = "Histogram of Titanic Passenger Age", xlab = "Age", ylab = "Frequency")
        
        validate(
            need(input$ageInput, NULL),
            need(input$ageInput > 0 && input$ageInput <= 120, NULL)
        )
        
        abline(v=mean(input$ageInput), col="red", lwd=5, lty=4)
    })
    
    output$titanicFare <- renderPlot({
        
        df = titanic_df()
        
        hist(df$Fare, col = "blue", border = "white", breaks=25, main = "Histogram of Titanic Ticket Fare", xlab = "Fare", ylab = "Frequency")
        
        prediction <- get_prediction()
        
        abline(v=prediction, col="green", lwd=5, lty=4)
    })
    
    output$titanicSex <- renderPlot({
        
        df = titanic_df()
        
        barplot(table(df$Sex), col = "blue", border = "white", main = "Distribution of Passengers' Gender", xlab = "Gender", ylab = "Frequency")
    })
    
    output$titanicPClass <- renderPlot({
        
        df = titanic_df()
        
        barplot(table(df$Pclass), col = "blue", border = "white", main = "Distribution of Ticket Class", xlab = "Ticket Class", ylab = "Frequency")
    })
    
    titanic_df <- reactive({
        df <- read.csv("../titanic_clean.csv")
        return(df)
    })
    
    titanic_model <- reactive({
        titanic_name <- load(file="titanic_regr.model")
        return(get(titanic_name))
    })
    
    get_prediction <- reactive({
        
        validate(
            need(input$sexChoiceInput, "Sex field cannot be empty!"),
            need(input$ageInput, "Age field cannot be empty!"),
            need(input$ageInput > 0 && input$ageInput <= 120, "Invalid Age value!"),
            need(input$ticketClassInput, "Ticket Class field cannot be empty!")
        )
        
        titanic_regr <- titanic_model()
        new.ticket <- data.frame(Age=input$ageInput, Sex = input$sexChoiceInput, Pclass = as.integer(input$ticketClassInput))
        prediction <- predict(titanic_regr, new.ticket)
        return(prediction[[1]])
    })
}

shinyApp(ui = ui, server = server)
