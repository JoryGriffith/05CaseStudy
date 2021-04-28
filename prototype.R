library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

# check to see if the data exists in the environment
if (!exists("injuries")) { # if injuries does not exist, read in the following data
  injuries <- vroom::vroom("injuries.tsv.gz")
  products <- vroom::vroom("products.tsv")
  population <- vroom::vroom("population.tsv")
}

# codes to get the products for filtering
prod_codes <- setNames(products$prod_code, products$title)

# function that creates better tables by lumping everything after the 5 most frequent things in table together
  # into row called other
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

# start the UI
ui <- fluidPage(
  # allow user to chose between raw number and rate per 10,000
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    # user input to select variable to plot
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    # output from the server as tables
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    # output from server as plot
    column(12, plotOutput("age_sex"))
  ), # make sure that there is a comma separating different rows
  # user can look at the narrative of the injury
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)
#>>

# server
server <- function(input, output, session) {
  # make a reactive to select product coded
  selected <- reactive(injuries %>% filter(prod_code == input$code))
 # output tables
  # table for diagnosis using the function that we made above called count_top
output$diag <- renderTable(count_top(selected(), diag), width = "100%")
      # width=100% fforces the table to take up 100% of the space that is given to it
  # table for body part
output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  # table for location
output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  # create another reactive for getting rate if injury per 10k and raw number
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  # create plots for rate of injury 
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  
  # use the eventReactive() to create a reactive that only updates when the button is clicked or the underlying data changes
  # will randomly sample one of the stories from the category that we select
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  output$narrative <- renderText(narrative_sample())
}
#>>

shinyApp(ui, server)