### Content from Mastering Shiny: https://mastering-shiny.org/basic-case-study.html

### Exercise answers for:
# 1. Add an input control that lets the user decide how many rows to show in the summary tables
# 2. Provide a way to step through every narrative systematically with forward and back buttons
# 3. Advanced: Make the list of narratives "circular" so that advancing forward from the last narrative takes you to the first

library(shiny)
library(vroom)
library(tidyverse)

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")


#### prep for app #### 
prod_codes <- setNames(products$prod_code, products$title)

# return the top 5 most frequent categories in order of decreasing frequency
count_top <- function(df, var, n=5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{var}}) %>%
    summarise(n = as.integer(sum(weight)))
}


### APP ### 

ui <- fluidPage(
  
  fluidRow(
    column(8, selectInput("code", "Product",
                          choices = setNames(products$prod_code, products$title),
                          width = "100%")),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  
  fluidRow(
    column(4, DT::DTOutput("diag")),
    column(4, DT::DTOutput("body_part")),
    column(4, DT::DTOutput("location"))
  ),
  
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  
  fluidRow(
    column(2, actionButton("prev_narr", "Previous story")),
    column(2, actionButton("next_narr", "Next story")),
    column(8, textOutput("narrative")),
    br(),
    br(),
    br()
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$diag <- DT::renderDT(
    count_top(selected(), diag),
    options=list(searching=FALSE), width = "100%"
  )
  
  output$body_part <- DT::renderDT(
    count_top(selected(), body_part),
    options=list(searching=FALSE), width = "100%"
  )
  
  output$location <- DT::renderDT(
    count_top(selected(), location),
    options=list(searching=FALSE), width = "100%"
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n/population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour=sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour=sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
    
  }, res=96)
  
  ### Pseudocode to help with development ### 
  # create a subset of narratives based on the user's selection from a drop-down list
  # have the first line of the subset printed/rendered to screen when a selection is made from the drop-down list
  # have two buttons that will show the next (or previous) narrative in the subset on clicking
  # have an index counter that will increase if the user clicks on next and decrease if the user clicks on previous
  # when one of the buttons are clicked (i.e. the index increases/decreases by one), a single narrative has to be printed to screen (renderText)
  
  selection_index <- reactiveVal(1)
  
  observeEvent(selected(),
               selection_index(1))
  
  observeEvent(input$prev_narr, {
    if(selection_index() == 1) {
      selection_index(nrow(selected()))
    } else {
      selection_index(selection_index()-1)
    }
  }
  )
  
  observeEvent(input$next_narr, {
    next_index <- selection_index()+1
    if (next_index > nrow(selected())) {
      selection_index(1)
    } else {
      selection_index(next_index)
    }
  }
  )
  
  
  narrative_sample <- reactive({
    selected() %>%
      pull(narrative)
  })
  
  output$narrative <- renderText(narrative_sample()[selection_index()])
  
}  


shinyApp(ui, server)
