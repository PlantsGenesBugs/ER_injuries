library(shiny)
library(vroom)
library(tidyverse)

dir.create("neiss")
download <- function(name) {
  url <- "https://raw.github.com/hadley/mastering-shiny/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet=TRUE)
}

download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")

#### EDA ####
selected <- injuries %>% filter(prod_code == 649)
nrow(selected)

# calculate number of toilet-related injuries using weight variable to "weight" counts
# weight * count = estimated total injuries (for that class)

selected %>% count(location, wt = weight, sort=TRUE)
selected %>% count(body_part, wt=weight, sort=TRUE)
selected %>% count(diag, wt=weight, sort=TRUE)

summary <- selected %>%
  count(age, sex, wt=weight)

summary %>%
  ggplot(aes(x=age, y=n, colour=sex)) +
  geom_line() +
  labs(y = "Estimated number of injuries",
       title = "Your toilet wants to kill you")

# control for population size i.e. per capita (fewer older people than younger, so affected
# proportion is larger when count is large)

### injury rate ###
summary <- selected %>%
  count(age, sex, wt = weight) %>%
  left_join(population, by = c("age", "sex")) %>%
  mutate(rate = n/population * 1e4)

summary %>%
  ggplot(aes(age, rate, colour = sex)) +
  geom_line(na.rm = TRUE) +
  labs(y = "Injuries per 10,000 people",
       title = "Your toilet wants to kill you")

# check narratives to see if our hypotheses are correct; get more ideas for further exploration
selected %>%
  sample_n(10) %>%
  pull(narrative)

# how to tidy tables
injuries %>%
  mutate(diag = fct_lump(fct_infreq(diag), n=5)) %>%
  group_by(diag) %>%
  summarise(n = as.integer(sum(weight)))


#### build app #### 
prod_codes <- setNames(products$prod_code, products$title)

# table tidying after initial test
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
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
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
  
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  
  output$narrative <- renderText(narrative_sample())
  
}  
  

shinyApp(ui, server)
