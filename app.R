
# uncomment the next line to install the packages you need
# install.packages(c("shiny", "RMariaDB", "tidyverse", "bayesbio", "DT", "formattable", "reactable"))

library(shiny)
library(RMariaDB)
library(tidyverse)
library(bayesbio)
library(DT)
library(formattable)
library(reactable)
shiny::devmode()

source("app functions.R")

## load in data from db ##

# read in athletes table
# load all of table query
table_name = "athletes"
load_all_query <- paste0("SELECT * FROM ", table_name, ";")
# print(load_all_query)
blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
rs = dbSendQuery(blastDb, load_all_query)
loadedData <- dbFetch(rs)
# Clear the result
dbClearResult(rs)
# Disconnect to clean up the connection to the database.
dbDisconnect(blastDb)
athletes <- loadedData

hitters = unique(athletes$tm_name)
hitters_anon = rep(paste("Hitter", c(1:length(hitters))))
hitter_anon_df = data.frame(hitters, hitters_anon)


# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(column(5, h1("Blast Data Viewer"))),
    fluidRow(column(3, selectInput("selecthitter", label = "", choices = hitters_anon, selected = "Hitter 1")),
             column(2, radioButtons("environment", label = h3("Environment"), 
                                          choices = list("BP" = 1, "Live" = 2),
                                          selected = 1),),
             column(2, img(height = 150, width = 352.5, src="carolina.svg.png"))),
    
    tabsetPanel(
        tabPanel("Home", 
                 br(),
                 # em("View a summary of your most recent session compared with your season averages, 
                 #    as well as your most recent notes and swing focuses/adjustments"),
                 # fluidRow(hr(), column(9, h2("Last Session"),
                 #            tableOutput("recent_session_summary"))),
                 fluidRow(hr(), column(4, h2("Averages"),
                                       tableOutput("hitter_summary")),
                          column(8, uiOutput("key1"))
                          )#,
                 # fluidRow(
                 #   column(3, h2("Notes"),
                 #          tableOutput("recent_notes")),
                 #          column(5, h2("Focuses"),
                 #          tableOutput("recent_mechanics"))
                 # )
                 ),
        tabPanel("Progress",
                 hr(),
          sidebarLayout(
            sidebarPanel(width = 3,
                         selectInput("metric1", label = "Metric", choices = c("Bat Speed (mph)", 
                                                                              "Hand Speed (mph)", "Rotation Score", "Attack Angle (deg)", "Attack AA (deg)",
                                                                              "On Plane Efficiency (%)", "Vertical Bat Angle (deg)", "Early Connection (deg)", "Connection at Impact (deg)", "Smash Factor"), selected = "Bat Speed (mph)"),
                         checkboxInput("fall", label = "Include Fall",
                                       value = F)
            ),
            mainPanel(
              plotOutput("trend_graph")
            )
          ),
        ),
        tabPanel("Analysis",
                 br(),
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"
                 ),
                 fluidRow(hr(),
                     column(4, h2("Session Summary"),
                            selectInput("selectSession2", label = "Session", choices = NULL),
                            tableOutput("session_summary")),
                     column(5, 
                            br(),
                            br(),
                            br(),
                            selectInput("metric2", label = "Metric", choices = c("Bat Speed (mph)", 
                                                                                 "Hand Speed (mph)", "Rotation Score", "Attack Angle (deg)", "Attack AA (deg)",
                                                                                 "On Plane Efficiency (%)", "Vertical Bat Angle (deg)", "Early Connection (deg)", "Connection at Impact (deg)", "Smash Factor"), 
                                        selected = "Bat Speed (mph)"),
                            
                            em("dashed line represents average"),
                            plotOutput("density_graph")),
                     column(3, br(),
                            br(),
                            br(),
                            br(),
                            dataTableOutput("anomaly_table"))
                     
                            )
                     ############
                 # fluidRow(
                 #   column(3,  
                 #          selectInput("best_worst_metric", label = "Choose Metric", choices = c("Bat Speed (mph)", 
                 #                                                                                "Hand Speed (mph)", "Bat Speed Efficiency", "Rotational Acceleration (g)", 
                 #                                                                                "Attack Angle (deg)", "Attack AA (deg)","Exit Velo", "Smash Factor",
                 #                                                                                "Time to Contact (sec)"), selected = "Bat Speed (mph)"),
                 #          h3("Best Swings"),
                 #          dataTableOutput("best_swings"),
                 #          h3("Worst Swings"),
                 #          dataTableOutput("worst_swings"))
                 # )
                 ),
        tabPanel("Film Room",
                 tags$style(
                     type = 'text/css',
                     # '.modal-dialog { width: fit-content !important; }',
                     '.modal-lg {
                         width: 1100px;
                     }'
                 ),
                 br(),
                 p("Very Low", style = "background-color: #3333FF; width: 60px;"),
                 p("Low", style = "background-color: #CCCCFF; width: 30px;"),
                 p("Normal"),
                 p("High", style = "background-color: #CCFFCC; width: 30px;"),
                 p("Very High", style = "background-color: #00FF00; width: 60px;"),
                 
                 # em("Leave notes on full sessions or individual swings"),
                 br(),
                 br(),
                 sidebarLayout(
                   # sidebarPanel(width = 2,
                   #              checkboxInput("all_sessions", label = "All Sessions", value = F),
                   #              checkboxInput("only_video", label = "Only Swings with Video", value = F),
                   # ), 
                   sidebarPanel(width = 2,
                         checkboxInput("all_sessions", label = "All Sessions", value = F),
                         checkboxInput("only_video", label = "Only Swings with Video", value = F),
                         p("Change the Environment to Live to see swings with video"),
                         selectInput("selectSession", label = "Choose Session", choices = NULL)#,
                         # actionButton("make_swing_note", "Leave Swing Note"),
                         # br(),
                         # br(),
                         # actionButton("make_session_note", "Leave Session Note"),
                         # br(),
                         # br(),
                         # actionButton("make_edit", "Edit/Delete Note"),
                         # em(h5("Refresh your browser to see edits"))
                     ),
                     mainPanel(
                         dataTableOutput("notebook_add")#,
                         # tableOutput("notebook_view")
                     )
                 )
                 )#,
        # tabPanel("Full Team",
        #          br(),
        #          tabsetPanel(
        #            tabPanel("Summary",
        #                     mainPanel(
        #                       br(),
        #                       em("Click on a column to sort"),
        #                       dataTableOutput("full_team")
        #                     )
        #            ),
        #            tabPanel("Film",
        #              br(),
        #              sidebarLayout(
        #                sidebarPanel(width = 2,
        #                             checkboxInput("all_sessions2", label = "All Sessions", value = T),
        #                             selectInput("selectSession3", label = "Choose Session", choices = as.character(rev(sort(unique(fullteam_joinedLive$Date))))), 
        #                             ),
        #                mainPanel(
        #                  dataTableOutput("fullteam_video")
        #                )
        #              )
        #              
        #            )
        #          )
        # ),
        # tabPanel("Logs",
        #          tags$style(
        #              type = 'text/css',
        #              # '.modal-dialog { width: fit-content !important; }',
        #              '.modal-lg {
        #                  width: 1100px;
        #              }'
        #          ),
        #          br(),
        #          em("Make note of what you focused on during a session to keep track of your goals"),
        #          br(),
        #          br(),
        #          sidebarLayout(
        #              sidebarPanel(width = 2,
        #                           actionButton("log_adjustment", "Log a Focus"),
        #                           br(),
        #                           br(),
        #                           actionButton("make_edit_adj", "Edit/Delete"),
        #                           br(),
        #                           br(),
        #                           em(h5("Refresh your browser to see edits"))
        #              ),
        #              mainPanel(
        #                  # dataTableOutput("notebook_add")#,
        #                fluidRow(column(1, ""),
        #                         column(5, h2("All Notes"), tableOutput("notes_history")),
        #                         column(6, h2("All Focuses"), tableOutput("mechanics_history"))
        #                )
        #              )
        #          )
        # )#,
        # tabPanel("Logs",
        #          fluidRow(column(1, ""),
        #              column(5, h2("All Notes"), tableOutput("notes_history")),
        #                   column(6, h2("All Focuses"), tableOutput("mechanics_history"))
        #          )
        #          )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # name <- reactive({athletes[which(athletes$tm_name == input$selecthitter), "blast_name"])
    name <- reactive({athletes[which(athletes$tm_name == hitter_anon_df[which(hitter_anon_df$hitters_anon == input$selecthitter), "hitters"]), "blast_name"]})
    
    env <- reactive({
       ifelse(input$environment == 1, "_joinedBP", "_joinedLive")
      })
     
    player_joined <- eventReactive({input$selecthitter
                                   input$environment}, {
      # name = "horvath"
      # load all of table query
      table_name = paste0(name(), env())
      load_all_query <- paste0("SELECT * FROM ", table_name, ";")
      # print(load_all_query)
      blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
      rs = dbSendQuery(blastDb, load_all_query)
      loadedData <- dbFetch(rs)
      # Clear the result
      dbClearResult(rs)
      # Disconnect to clean up the connection to the database.
      dbDisconnect(blastDb)
      loadedData
    })

    observe({
      updateSelectInput(inputId = "selectSession", choices = as.character(rev(sort(unique(player_joined()$Date)))))
      updateSelectInput(inputId = "selectSession2", choices = as.character(rev(sort(unique(player_joined()$Date)))))
    })
    
    output$key1 <- renderUI({
      tagList(
        h2("Key"),
        selectInput("key_metric", label = "Metric", choices = c("Bat Speed (mph)",
                                                                "Hand Speed (mph)", "Time to Contact (sec)", "Rotational Acceleration (g)",
                                                                "Rotation Score", "Attack Angle (deg)", "Attack AA (deg)", "On Plane Efficiency (%)",
                                                                "Vertical Bat Angle (deg)", "Early Connection (deg)", "Connection at Impact (deg)", "Smash Factor")
                                                                ),
        strong("Definition: "),
        textOutput("key_definition"),
        br(),
        uiOutput("key_visual")
      )
    })

    output$key_visual <- renderUI({
      height = key.visual(metric = input$key_metric)[[1]]
      width = key.visual(metric = input$key_metric)[[2]]
      src1 = key.visual(metric = input$key_metric)[[3]]
      src2 = key.visual(metric = input$key_metric)[[4]]
      # src2 = key.visual(metric = "Bat Speed (mph)")[[4]]
      src3 = key.visual(metric = input$key_metric)[[5]]
      statement1 = key.visual(metric = input$key_metric)[[6]]
      statement2 = key.visual(metric = input$key_metric)[[7]]
      if (input$key_metric %in% c('Bat Speed (mph)', 'Hand Speed (mph)', 'Time to Contact (sec)', 'Smash Factor', 
                                  'Rotational Acceleration (g)', 'Rotation Score')) {
        tagList(
          fluidRow(
            column(4, 
                   img(src=src1),
                   br(),
                   br(),
                   p(statement1),
                   tags$video(src = src2, type = "video/mov", controls = "controls")
            )
          )
          
        )
      } else {
        
        tagList(
          fluidRow(
            column(4, 
                   img(src=src1),
                   br(),
                   br(),
                   p(statement1),
                   tags$video(src = src2, type = "video/mov", controls = "controls"),
                   br(),
                   br(),
                   p(statement2),
                   tags$video(src = src3, type = "video/mov", controls = "controls")
            )
          )
          
        )
        
      }
    })
    
    output$key_definition <- renderText({
      key.definition(metric = input$key_metric)
    })

    output$recent_session_summary <- renderTable({
        data = recent.session.summary(name(), env = input$environment)
        data$`Time to Contact (sec)` <- formatC(data$`Time to Contact (sec)`, digits = 2)
        data$`# Swings` <- formatC(data$`# Swings`, digits = 4)
        data
    }, rownames=T, digits=1, align = "r")
    
    output$hitter_summary <- renderTable({
      hitter.summary(name(), env = input$environment) 
    }, rownames=T, align = "r")
    
    output$trend_graph <- renderPlot({
      trend.graph(name(), env = input$environment, metric = input$metric1, fall = input$fall)
    })
    
    output$density_graph <- renderPlot({
      density.graph(name(), env = input$environment, date = input$selectSession2, metric = input$metric2)
    })
    
    output$session_summary <- renderTable({
        data = session.summary(name(), date = input$selectSession2, env = input$environment)
        # data$`Time to Contact (sec)` <- formatC(data$`Time to Contact (sec)`, digits = 2)
        # data$`# Swings` <- formatC(data$`# Swings`, digits = 4)
        data
        
    }, rownames=T, digits=1, align = "r", sanitize.text.function = function(x) x)

    output$best_swings <- renderDataTable({
        head(best.swings(name(), metric = input$best_worst_metric, date = input$selectSession2, env = input$environment) %>% na.omit(), 3)
    }, escape = FALSE, rownames = F, selection = 'none', options = list(dom = 't'))
    
    output$worst_swings <- renderDataTable({
        head(worst.swings(name(), metric = input$best_worst_metric, date = input$selectSession2, env = input$environment) %>% na.omit(), 3)
    }, escape = FALSE, rownames = F, selection = 'none', options = list(dom = 't'))

    output$recent_notes <- renderTable({
        recent.notes(name())
    })

    output$recent_mechanics <- renderTable({
        df = data.frame(get.mechanics(name())) %>% arrange(desc(id))
        df %>% select(-id)
    })

    notebook1 <- reactive({
      if (input$all_sessions & input$only_video) {
        tbl = notebook.table(name(), env = input$environment) %>% filter(str_detect(`Video Link`, "unchighspeedvideo"))
      } else if (input$all_sessions & !(input$only_video)) {
        tbl = notebook.table(name(), env = input$environment)
      } else if (!(input$all_sessions) & input$only_video) {
        tbl = notebook.table(name(), env = input$environment) %>% filter(Date == input$selectSession, str_detect(`Video Link`, "unchighspeedvideo")) %>% select(-Date)
      } else {
        tbl = notebook.table(name(), env = input$environment) %>% filter(Date == input$selectSession) %>% select(-Date)
      }
      
      tbl
      
    })
    
    notebook4 <- reactive({
      if (input$all_sessions2) {
        fullteam.notebook.table(env = input$environment) %>% filter(str_detect(`Video Link`, "unchighspeedvideo"))
      } else {
        fullteam.notebook.table(env = input$environment) %>% filter(Date == input$selectSession3) %>% select(-Date)
      }
    })
    
    notebook5 <- reactive({
      session.anomaly.table(name = name(), date = input$selectSession2, env = input$environment, metric = input$metric2)
    })
    
    notebook2 <- reactive({
        get.notes(name()) %>% arrange(desc(id))
    })
    
    notebook3 <- reactive({
        get.mechanics(name()) %>% arrange(desc(id))
    })

    observeEvent(input$swing_submit, {
        note = as.character(input$swing_note_textbox)
        # name
        date = input$selectSession
        # data = notebook1()
        id = as.numeric(notebook1()[as.integer(Clicked()), "Swing ID"])
        add.swing.note(note = note, id = id, name = name(), date = date)
    })

    observeEvent(req(input$sess_submit), {
        note = as.character(input$session_note_textbox)
        # name
        date = req(input$selectSession)
        # data = notebook1()
        # print("good")
        add.session.note(note = note, name = name(), date = date)
    })
# 
#     observeEvent(req(input$edit_submit), {
#         note = as.character(input$edit_note_textbox)
#         # name
#         date = req(input$selectSession)
#         # data = notebook1()
#         # print("good")
#         id = as.numeric(notebook2()[as.integer(ClickedEdit()), "id"])
#         edit.note(id = id, name = name(), note = note)
#     })

    output$notebook_add <- renderDataTable({
        # date = input$selectSession
        # name = athletes[which(athletes$tm_name == input$selecthitter), "blast_name"]
        # notes.datatable(name(), date = input$selectSession)
        # df = notebook1() %>% filter(Date == input$selectSession) %>% select(-Date)
        conditional.formatting.film.room(datatable(notebook1(), selection = 'single', options = list(pageLength = 15, dom = "tpi"), rownames = F, escape = F), name = name(), env = input$environment)
    }, escape = FALSE)
    
    output$fullteam_video <- renderDataTable({
      datatable(notebook4(), selection = 'single', options = list(pageLength = 25, dom = "tpi"), rownames = F, escape = F)
    }, escape = FALSE)
    
    output$anomaly_table <- renderDataTable({
      suppressMessages(datatable(notebook5(), selection = 'single', options = list(pageLength = 10, dom = "tpi"), rownames = F, escape = F))
    }, escape = FALSE)

    output$notebook_add_modal <- renderDataTable({
        # notebook1 <- notebook.add.table(name = name(), date = input$selectSession)
        # df = notebook1() %>% filter(Date == input$selectSession) %>% select(-Date)
        datatable(notebook1(), selection = 'single', options = list(pageLength = 5, dom = "tpi"), rownames = F, escape = F)
    }, escape = FALSE)
    
    output$notebook_edit_modal <- renderDataTable({
        # notebook1 <- notebook.add.table(name = name(), date = input$selectSession)
        # df = notebook1() %>% filter(Date == input$selectSession) %>% select(-Date)
        datatable(notebook2(), selection = 'single', options = list(pageLength = 5, dom = "tpi"), rownames = F, escape = F)
    }, escape = FALSE)
    
    output$notebook_edit_modal_adj <- renderDataTable({
        # notebook1 <- notebook.add.table(name = name(), date = input$selectSession)
        # df = notebook1() %>% filter(Date == input$selectSession) %>% select(-Date)
        datatable(notebook3(), selection = 'single', options = list(pageLength = 5, dom = "tpi"), rownames = F, escape = F)
    }, escape = F)

    # output$Table <- renderDataTable({datatable(mtcars, selection = 'single')})
    
    output$full_team <- renderDataTable({
      datatable(fullteam_summary(name = "fullteam", env = input$environment), selection = 'single', options = list(pageLength = 25, dom = "tpi"), rownames = F, escape = F)
    }, escape = FALSE)

    Clicked <- eventReactive(input$notebook_add_modal_rows_selected,{
        input$notebook_add_modal_rows_selected
    })
    
    ClickedEdit <- eventReactive(input$notebook_edit_modal_rows_selected,{
        input$notebook_edit_modal_rows_selected
    })
    
    ClickedEditAdj <- eventReactive(input$notebook_edit_modal_adj_rows_selected,{
        input$notebook_edit_modal_adj_rows_selected
    })

    output$selected <- renderText({
        paste0("Selected Swing ID: ", notebook1()[as.integer(Clicked()), "Swing ID"])
        })
    
    output$selected_edit <- renderText({
        paste0("Selected Note ID: ", notebook2()[as.integer(ClickedEdit()), "id"])
    })
    
    output$selected_edit_adj <- renderText({
        paste0("Selected ID: ", notebook3()[as.integer(ClickedEditAdj()), "id"])
    })

    observeEvent(input$make_session_note, {
        showModal(modalDialog(h2("Leave Session Note"),
                              textAreaInput("session_note_textbox", value = "", label = NULL, placeholder = "leave note here", rows = 5),
                              footer = tagList(
                                  modalButton("Cancel"),
                                  actionButton("sess_submit", "Submit")
                              )
        ))
    })

    observeEvent(input$make_swing_note,{
        showModal(modalDialog(h2("Leave Swing Note"),
                              em("click the row you want to add a note to, then leave a note below and press submit"),
                              DT::dataTableOutput("notebook_add_modal"),
                              size = "l", br(),
                              textOutput("selected"),
                              textAreaInput("swing_note_textbox", value = "", label = NULL, placeholder = "leave note here", rows = 3),
                              footer = tagList(
                                  modalButton("Cancel"),
                                  actionButton("swing_submit", "Submit")
                              )
        ))
    })
    
    observeEvent(input$make_edit, {
        showModal(modalDialog(h2("Edit/Delete"),
                              em("click the row you want to edit/delete, then leave an updated note below and press submit or press delete"),
                              DT::dataTableOutput("notebook_edit_modal"), 
                              size = "l", br(),
                              textOutput("selected_edit"),
                              textAreaInput("edit_note_textbox", value = "", label = NULL, placeholder = "leave note here", rows = 3),
                              footer = tagList(
                                  modalButton("Cancel"),
                                  actionButton("edit_delete", "Delete"),
                                  actionButton("edit_submit", "Submit"),
                              )
        ))
    })

    observeEvent(input$swing_submit, {
        removeModal()
    })

    observeEvent(input$sess_submit, {
        removeModal()
    })
    
    observeEvent(input$edit_submit, {
        id1 = as.numeric(notebook2()[as.integer(ClickedEdit()), "id"])
        id2 = as.numeric(notebook2()[as.integer(ClickedEdit()), "Swing ID"])
        note = input$edit_note_textbox
        type = as.character(notebook2()[as.integer(ClickedEdit()), "Type"])
        edit.note(id1 = id1, id2 = id2, name = name(), note = note)
        removeModal()
    })
    
    observeEvent(input$edit_delete, {
        id1 = as.numeric(notebook2()[as.integer(ClickedEdit()), "id"])
        id2 = as.numeric(notebook2()[as.integer(ClickedEdit()), "Swing ID"])
        delete.note(id1 = id1, id2 = id2, name = name())
        removeModal()
    })

    ## Swing Adjustments ##

    all_days = seq(as.Date("2022-09-05"), as.Date(Sys.Date()), by="days")

    observeEvent(input$log_adjustment, {
        showModal(modalDialog(h2("Log Swing Focus or Adjustment"),
                              selectInput("selectAdjSession", label = "Choose Date", choices = rev(all_days)),
                              textAreaInput("adjustment_desc_textbox", value = "", label = "Description", placeholder = "Describe your focus or swing adjustment", rows = 3),
                              textAreaInput("adjustment_reasoning_textbox", value = "", label = "Reasoning", placeholder = "What is your goal?", rows = 3),
                              footer = tagList(
                                  modalButton("Cancel"),
                                  actionButton("adj_submit", "Submit")
                              )
        ))
    })
    
    observeEvent(input$make_edit_adj, {
        showModal(modalDialog(h2("Edit/Delete"),
                              em("click the row you want to edit/delete, then update the fields below and press submit or press delete"),
                              DT::dataTableOutput("notebook_edit_modal_adj"), 
                              size = "l", br(),
                              textOutput("selected_edit_adj"),
                              br(),
                              textAreaInput("edit_adjustment_desc_textbox", value = "", label = "Description", placeholder = "Describe your focus or swing adjustment", rows = 3),
                              textAreaInput("edit_adjustment_reasoning_textbox", value = "", label = "Reasoning", placeholder = "What is your goal?", rows = 3),
                              br(),
                              footer = tagList(
                                  modalButton("Cancel"),
                                  actionButton("edit_adj_delete", "Delete"),
                                  actionButton("edit_adj_submit", "Submit"),
                              )
        ))
    })

    observeEvent(req(input$adj_submit), {
        desc = as.character(input$adjustment_desc_textbox)
        reas = as.character(input$adjustment_reasoning_textbox)
        # name = "blaser"
        date = req(input$selectAdjSession)
        # data = notebook1()
        # print("good")
        log.mechanics(desc = desc, reas = reas, name = name(), date = date)
    })
    
    observeEvent(req(input$edit_adj_submit), {
        desc = as.character(input$edit_adjustment_desc_textbox)
        reas = as.character(input$edit_adjustment_reasoning_textbox)
        # name
        # data = notebook1()
        # print("good")
        id = as.numeric(notebook3()[as.integer(ClickedEditAdj()), "id"])
        edit.mechanics(id, name(), desc, reas)
        removeModal()
    })
    
    observeEvent(input$edit_adj_delete, {
        id = as.numeric(notebook3()[as.integer(ClickedEditAdj()), "id"])
        delete.mechanics(id, name())
        removeModal()
    })

    observeEvent(input$adj_submit, {
        removeModal()
    })

    output$mechanics <- renderTable({
        df = data.frame(get.mechanics(name())) %>% arrange(desc(id))
        df %>% select(-id)
    })
    
    ## History ##
    output$notes_history <- renderTable({
        recent.notes(name())
    })
    
    output$mechanics_history <- renderTable({
        recent.mechanics(name())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
