  library(rintrojs)
  library(bsplus)
  library(shiny)
  library(shinycssloaders)
  library(shinyWidgets)
  library(reactable)
  library(tidyverse)
  source("AxelrodCopy.R")


ui <- fluidPage(
  navbarPage("Iterated Prisoners Dillemma",
             theme = shinythemes::shinytheme("flatly"),
             use_bs_tooltip(),
             use_bs_popover(),
             introjsUI(),
                   tabPanel("Game", 
                            sidebarLayout(
                              sidebarPanel(
                                div(
                                  titlePanel("Single Game"), align = "center"
                                ),
                                fluidRow(
                                    column(4,
                                           introBox(
                                           selectInput("strategy1", "Strategy", choices = c(names(strats), ""), selected =""),
                                           data.step = 1,
                                           data.intro = "Choose a strategy to face another in an iterated prisoners dillemma game")
                                           ),
                                    column(1, "vs"
                                           ),
                                    column(4,
                                           selectInput("strategy2", "Strategy", choices = c(names(strats), ""), selected ="")
                                           ),
                                    column(3,
                                           numericInput("moves", label = "Moves", value = 100, min = 30, max = 500)
                                           )
                                    ),
                                fixedRow(
                                  column(1,
                                    uiOutput("icons", style = "font-size:20px")
                                  ),
                                  column(7,
                                    textOutput(outputId = "StratText")
                                    ),
                                  column(1,
                                         conditionalPanel(
                                           condition = "input.strategy1",
                                           dropdown(inputId = "codebtn", label = "Code", size = "sm",
                                                    verbatimTextOutput(outputId = "code")))
                                         )
                                ),
                                hr(style="height:200px"),
                                fluidRow(div(titlePanel("Tournament"), align = "center")),
                                fluidRow(
                                  column(4,
                                    awesomeCheckboxGroup("checkstrats", "Choose Strategies", choices = names(strats)),
                                    numericInput("brnoise", label = list(icon("info-circle") |> 
                                    bs_embed_tooltip(title = "nonzero probability that a cooperation will accidentally be changed 
                                                     into a defection action and vice versa."), "% Noise"),
                                    value = 0, min = 0, max = 100)
                                    ),
                                  column(5,
                                    numericInput("tmoves", label = "Moves", value = 100, min = 30, max = 500),
                                    fluidRow(
                                      column(6,
                                             numericInput("Rew", value = 3, 
                                                          label = list(icon("info-circle") |> bs_embed_tooltip(title = "Reward,
                                                                                                                the payout from both players cooperating"), "R")
                                             )
                                             ),
                                      column(6,
                                             numericInput("Suc", value = 0, label = list(icon("info-circle") |> 
                                                                                           bs_embed_tooltip(title = "Sucker, typically the smallest payout.
                                               The result of a beneficiary player defecting and another player cooperating"), "S")) 
                                              
                                             )
                                      ),
                                    fluidRow(
                                      column(6,
                                             numericInput("Tem", value = 5, label = list(icon("info-circle") |> 
                                                          bs_embed_tooltip(title = "Temptation, typically the largest payout.
                                               The result of a player benefiting from defecting and another player cooperating"), "T"))
                                               ),
                                      column(6,
                                             numericInput("Pun", value = 1, label = list(icon("info-circle") |> 
                                                                                           bs_embed_tooltip(title = "Punishment, 
                                                                                                            the result of both players defecting"), "P"))
                                             ))
                                    
                                  ),
                                  column(3,
                                         br(),
                                         fluidRow(
                                           column(6,
                                                  actionButton( "rungame", "Start"))
                                         ),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         fluidRow(
                                           column(6,
                                                  actionButton( "reset1", "Reset"))
                                         ))
                                ),
                                width = 4
 
                            ),
                            mainPanel(
                              uiOutput("tooltip"),
                              #plot and conditional text
                              fluidRow(
                                    dropdownButton(
                                      icon = introBox(
                                        inputId = "dropdownbtn",
                                        icon("cog"),
                                        data.step = 2,
                                        data.intro = "After selecting your strategies, you can alter the parameters of the game"
                                        ),
                                      fluidRow(
                                        column(2,
                                               numericInput("Re", value = 3, 
                                                            label = list(icon("info-circle") %>% bs_embed_tooltip(title = "Reward,
                                                                                                                the payout from both players cooperating"), "R"))
                                               ),
                                        column(2,
                                               numericInput("Su", value = 0, label = list(icon("info-circle") %>%
                                                                                             bs_embed_tooltip(title = "Sucker, typically the smallest payout.
                                               The result of a player defecting and a benefiting player cooperating"), "S"))
                                               ),
                                        column(2,
                                               offset =6,
                                               actionButton("reset2",
                                                          label = "Reset"
                                                          )
                                               )
                                      ),
                                      fluidRow(
                                        column(2,
                                               numericInput("Te", value = 5, label = list(icon("info-circle") %>% 
                                                                                             bs_embed_tooltip(title = "Temptation, typically the largest payout.
                                               The result of a player benefiting from defecting and another player cooperating"), "T"))
                                        ),
                                        column(2,
                                               numericInput("Pu", value = 1, label = list(icon("info-circle") %>% 
                                                                                             bs_embed_tooltip(title = "Punishment, 
                                                                                                            the result of both players defecting"), "P"))
                                        ),
                                        column(2, numericInput("singlenoise", label = list(icon("info-circle") |> 
                                        bs_embed_tooltip(title = "nonzero probability that a cooperation will accidentally be changed 
                                                     into a defection action and vice versa."), "% Noise"),
                                                               value = 0, min = 0, max = 100)))
                                    ),
                                column(8,
                                       plotOutput("battle")
                                       ),
                                column(4, 
                                       tags$br(),
                                       conditionalPanel(
                                         condition = 
                                           "(input.strategy1 && input.strategy2 && input.Te == 5 && input.Re == 3 
                                         && input.Pu == 1 && input.Su == 0)",
                                       #un-ordered list to contain list items
                                       tags$ul(
                                           tags$li(#list tag for bullet points
                                             textOutput(outputId = "condText"))))
                                       )
                                ),
                              #Tournament data with tournament text
                              fluidRow(
                                hr(),
                                conditionalPanel(
                                  condition = "input.rungame",
                                  navlistPanel(id="formats", well = FALSE, widths = c(2,10),
                                               tabPanel(
                                                 "Table",
                                                 fixedRow(
                                                   column(8,
                                                          withSpinner(reactableOutput("tourney", height  = "400px"))
                                                          )
                                                   )
                                               ),
                                               tabPanel(
                                                 "Heatmap",
                                                 br(),
                                                 fixedRow(
                                                   column(8,
                                                          withSpinner(plotOutput("heatmap"))
                                                   )
                                                 )
                                               ),
                                               tabPanel(
                                                 "Plot",
                                                 fluidRow(
                                                   column(8,
                                                          withSpinner(plotOutput("allplot")))
                                                 )
                                               )
                                  )
                                )
                            )
                            )
                   )
                   )
             
  )

)

server <- function(input, output, session) {
  introjs(session)
  
  #give a warning to user instead of an error message, battle data
  data <- reactive(if(input$strategy1 != "" & input$strategy2 != "") {
    battle((strat1 = strats[[input$strategy1]]), strat2 = strats[[input$strategy2]], move =  input$moves, 
           payoffs = c(input$Te, input$Re, input$Pu, input$Su), noise = input$singlenoise)
  } else {
    warning("Select your strategies")
  })
  
  #linear line plot data
  plot_data <- reactive(myplot(data(), input$moves, input$strategy1, input$strategy2))
  
  output$battle <- renderPlot( plot_data()[[1]] )
  
  text <- reactive(cond_text(plot_data()))
  
  long_game <- reactive(
    battle_royale(
      strats[input$checkstrats],strats[input$checkstrats], input$tmoves,
      payoffs = c(input$Tem, input$Rew, input$Pun, input$Suc), noise = input$brnoise
      ))
  
  sumtable <- reactive(
    #apply mutate row sum over the data generated from long_data
    map(long_game()[["data"]], function(a) {
    mutate(a[1:input$tmoves,], sum = apply(a[1:input$tmoves,],1, sum)) 
  }) %>% 
    #combine the summed columns and cumsum across all columns
    map_dfr(., ~ .x$sum) %>% mutate(across(everything(), ~ cumsum(.x)))
  )


    output$condText <- renderText({
      tryCatch(text())
    })
  
  output$tourney <- renderReactable( 
    reactable(long_game()[["table"]], striped = TRUE)) %>% 
    bindEvent(input$rungame)
  
  output$heatmap <- renderPlot( heatm(long_game()[["table"]]) ) %>% 
    bindCache(input$tmoves,input$checkstrats, input$Tem, input$Suc, input$Rew, input$Pun, input$brnoise) %>% 
    bindEvent(input$rungame)
  
  output$allplot <- renderPlot( allPlot(sumtable()) ) %>% bindEvent(input$rungame)
  
  output$StratText <- renderText(stratText(input$strategy1) )
  
  output$icons <- renderUI( switch(input$strategy1,
                                   "Tester" = icon("otter"),
                                   "Tit for Tat" = icon("cat"),
                                   "Tit for Two" = icon("dove"),
                                   "Joss" = icon("crow"),
                                   "Random" = icon("horse-head"),
                                   "Always Defect" = icon("paw"),
                                   "Permanent Retaliation" = icon("horse")
  ))
  
  output$code <- renderPrint( switch(input$strategy1,
                                     "Tester" = format(strats$Tester),
                                     "Tit for Tat" = format(strats$`Tit for Tat`),
                                     "Tit for Two" = format(strats$`Tit for Two`),
                                     "Joss" = format(strats$Joss),
                                     "Random" = format(strats$Random),
                                     "Always Defect" = format(strats$`Always Defect`),
                                     "Permanent Retaliation" = format(strats$`Permanent Retaliation`)
  ))
  
  observe({
    updateAwesomeCheckboxGroup(inputId = "checkstrats", selected = "")
    map2(c("Tem","Rew", "Pun", "Suc", "tmoves", "brnoise"),c(5,3,1,0, 100, 0), ~ updateNumericInput(inputId = .x, value = .y))
  }) %>% bindEvent(input$reset1)
  
  #reset button for drop down list
  observe({
    map2(c("Te","Re", "Pu", "Su", "singlenoise"),c(5,3,1,0, 0), ~ updateNumericInput(inputId = .x, value = .y))
  }) %>% bindEvent(input$reset2)
  

  
  
  
}

shinyApp(ui, server)


