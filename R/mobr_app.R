# Outside code for CSV file read in used in ui

# Module UI function
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label)
  )
}

# Module server function
csvFile <- function(input, output, session, stringsAsFactors = TRUE) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = TRUE,
             stringsAsFactors = stringsAsFactors)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}
# End external CSV file read in code


######################################################################################################################## 
#########################################################UI############################################################# 
######################################################################################################################## 

# User Interface using Shiny Dashboard
ui <- dashboardPage(
  # Title for upper left hand corner (above sidebar)
  dashboardHeader(title = "MoB-R"),

  # Initalize and name tabs in the sidebar
  dashboardSidebar(
    sidebarMenu(style = "position: fixed; overflow: visible;",
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Data Tab", tabName = "DataTab", icon = icon("table")),
      menuItem("Exploratory Analysis", tabName = "exploritory_analysis", icon = icon("angle-right")),
      menuItem("MoB Metrics", tabName = "mob_metrics", icon = icon("angle-right"),
        menuSubItem("All MoB Metrics", tabName = "all_mob_tab", icon = icon("angle-right")),
        menuSubItem("Individual MoB Metrics", tabName = "ind_mob_tab", icon = icon("angle-right"))),
      menuItem("Delta Stats", tabName = "delta_stats", icon = icon("angle-right")),
      menuItem("MoB-R GitHub Page", icon = icon("github"), 
               href = "https://github.com/MoBiodiv/mobr"),
      menuItem("Got an Issue? Submit it!", icon = icon("exclamation-triangle"), 
               href = "https://github.com/MoBiodiv/mobr/issues")
    )
  ),
  
  # Main page or body of tabs
  # This is all content that will be presented when the tab is clicked on in the main body of the app
  # Each 'tabItem' is labled with is corresponding sidebar tab name using a comment
  dashboardBody(
    
    
    tabItems(
      
      # Home tab content
      tabItem(tabName = "Home",
              h2("Welcome to the MoB-R App!"),
              h4("NOTE: Graphics work best if viewed in full window OR when side panel is closed"),
              fluidRow(
              # Instructions for navigating the MoB-R app detailed in this first box
              box(
            
                # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                width = 6,
                
                # Heading for the box - name preceded by icon of a globe
                h3(icon("globe"), "Navigation Tabs"),
                
                # br() stands for break line in code
                br(),
                h4(icon("table"),"DataTab"),
                "- Where you will enter you Community Matrix and Plot Data as CSV files", 
                br(), 
                br(), 
                h4(icon("angle-right"),"Plot Rarefaction"),
                "- Graphs and Code for Plot Rarefaction data",
                br(),
                br(), 
                h4(icon("angle-right"),"MoB Metrics"),
                "- A pull down tab containing 'All MoB Metrics' and 'Individual MoB Metrics'", 
                br(), 
                "- Stats for both can be found in the 'Individual MoB Metrics' tab",
                br(),
                br(), 
                h4(icon("angle-right"),"Delta Stats"),
                "- Graph and Code for Delta Stats data"
              ),
              # Box detailing the link tabs and how best to use them
              box(
                
                # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                width = 5,
                # Heading for the box - name preceded by icon of a link
                h3(icon("link"), "Link Tabs"),
                
                # br() stands for break line in code
                br(),
                "The last two tabs represent links to the MoB-R Github page",
                br(),
                h3(icon("github")),
                "If you want to learn more about MoB-R fromthe source check out the first link. It will take you to MoB-R's main GitHub page.",
                br(),
                br(),
                h4(icon("exclamation-triangle")),
                "If you run into a problem while using this app, please let the developers know! Use the second link to navigate to the issues page on the MoB-R GitHub.",
                br(),
                "There you can submit an issue describing your problem. We appreciate your feedback!"
              )
              ),
              fluidRow(
                # Box giving credit to authors of MoB-R/the MoB-R app
                box(
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  width = 4,
                  # Heading for the box - name preceded by icon of a small group of people or 'users'
                  h3(icon("users"), "The people behind MoB-R:"),
                  tags$b("Author and Creator: "),
                  # br() stands for break line in code
                  br(),
                  "Xiao Xiao - email: xiao@weecology.org",
                  br(),
                  tags$b("Authors: "),
                  br(),
                  "Daniel McGlinn - email: danmcglinn@gmail.com",
                  br(),
                  "Felix May - email: felix.may@idiv.de",
                  br(),
                  "Caroline Oliver - email: olivercs@g.cofc.edu"
                )
              )
      ),

######################################################################################################################## 
                                    # Data tab content #

      tabItem(tabName = "DataTab",
              # Heading for main body of tab
              h2("Enter your data below in CSV file format:"),
              
              # csvFileInput command for Community Matrix Data
              csvFileInput("comm", "Upload community data"),
              
              # Horizontal line ----
              tags$hr(),
              
              # csvFileInput command for Plot Attribute Data
              csvFileInput("plot_attr", "Upload plot attribute data")
              
      ),
      
########################################################################################################################         
                                       # Exploratory Analysis tab content #      
      
      tabItem(tabName = "exploritory_analysis",
              
              # Heading for main body of tab
              h2("Exploratory Analysis"),
              
              # sub- heading deatiling abreviation meanings
              h4("SR = Spacial Rarefaction, IR = Indiviudal Rarefaction, Abu = Abundance"),
              
              # First/top box on Exploratory Analysis tab that displays graphs
              # each 'tabPanel' represents a difference exploratory graph
              fluidRow(
                tabBox(
                  # Title of the box
                  title = "Exploratory Analysis",
                  
                  # 'Side' represents what orientation you would like the box to be in (left, right, center)
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  side = "left", width = "10",
                  
                  # Initially select far left tab to be displayed first
                  selected = "SR",
                  
                  # the first parameter in quotation marks represents the graph type
                  # the plotOutput funtion parameter corresponds to the name of an output code that creates the plot in the server portion of the app
                  
                  # Species Rarefaction Graph
                  tabPanel("SR", withSpinner(plotOutput('s_rare'))),
                  
                  # Individual Rarefaction Graph - UNPOOLED
                  tabPanel("IR Unpooled", withSpinner(plotOutput('i_rare_up'))),
                  
                  # Individual Rarefaction Graph - POOLED
                  tabPanel("IR Pooled", withSpinner(plotOutput('i_rare_p'))),
                  
                  # Species Abundance Graph - UNPOOLED
                  tabPanel("Unpooled Abu", withSpinner(plotOutput('up_abu'))),
                  
                  # Species Abundance Graph - POOLED
                  tabPanel("Pooled Abu", withSpinner(plotOutput('p_abu')))
                )
              ),
              
              # Second/bottom box on Exploratory Analysis tab that displays code used to create graphs
              fluidRow(
                tabBox(
                  # Title of the box
                  title = "Exploratory Analysis Code",
                  
                  # 'Side' represents what orientation you would like the box to be in (left, right, center)
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  side = "left", width = "10",
                  
                  # Initially select far left tab to be displayed first
                  selected = "SR",
                  
                  # the first parameter in quotation marks represents the graph type
                  # the htmlOutput funtion parameter corresponds to the name of an output code that diplays plot code text in the server portion of the app
                  
                  # Species Rarefaction Code
                  tabPanel("SR", withSpinner(htmlOutput('s_rare_code'))),
                  
                  # Individual Rarefaction Code - UNPOOLED
                  tabPanel("IR Unpooled", withSpinner(htmlOutput('ir_up_code'))),
                  
                  # Individual Rarefaction Code - POOLED
                  tabPanel("IR Pooled", withSpinner(htmlOutput('ir_p_code'))),
                  
                  # Species Abundance Code - UNPOOLED
                  tabPanel("Unpooled Abu", withSpinner(htmlOutput('up_abu_code'))),
                  
                  # Species Abundance Code - POOLED
                  tabPanel("Pooled Abu", withSpinner(htmlOutput('p_abu_code')))
                )
                
              )
      ),

########################################################################################################################         
                                               # All Mob Metrics Tab #
      
      tabItem(tabName = "all_mob_tab",
              
              # Heading for main body of tab
              h2("All MoB Metrics"),
              
              # First/top box that displays graph with all MoB Metric data
              fluidRow(
                box(
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  width = 7,
                  
                  # 'Height' refers to how many pixels high your box will be (different than width implementation)
                  height = "750px",
                  
                  # Title of the box
                  title = "All MoB Metrics",
                  
                  # the plotOutput funtion parameter corresponds to the name of an output code that creates the plot in the server portion of the app
                  # height of graph must be smaller (less pixels) than box height to fit appropriately
                  withSpinner(plotOutput('mob_all', height = "695px"))
                ),
                
                # Second/bottom box that displays code for all MoB Metric graph
                fluidRow(
                  box(
                    # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                    width = 7,
                    
                    # Title of the box
                    title = "All MoB Metrics Code",
                    
                    # the htmlOutput funtion parameter corresponds to the name of an output code that diplays plot code text in the server portion of the app
                    withSpinner(htmlOutput('all_mob_code'))
                  )
                )
              )
        ),
     
######################################################################################################################## 
                                            # Indiviudal Mob stats tab #
      
      tabItem(tabName = "ind_mob_tab",
              
              # Heading for main body of tab
              h2("MoB Metrics"),
              
              # First/top box that displays graph with all MoB Metric data
              fluidRow(
                
                # Tab Box containing MoB Metric GRAPHS
                tabBox(
                  title = "Individual MoB Metrics",
                  
                  # side = screen orientation
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  side = "left", width = "10",
                  
                  # Initially selected graph - observed specied richness
                  selected = "S",
                  
                  # observed specied richness graph code
                  tabPanel("S", withSpinner(plotOutput('mob_s'))),
                  
                  # number of individuals graph code
                  tabPanel("N", withSpinner(plotOutput('mob_n'))),
                  
                  # rarefied species richness graph code
                  tabPanel("S_n", withSpinner(plotOutput('mob_Sn'))),
                  
                  # effective number of species based on PIE graph
                  tabPanel("S_PIE", withSpinner(plotOutput('mob_Spie')))
                )
              ),
              fluidRow(
                # Statistical output for GROUPS STATS
                box(
                  title = "Groups Stats",
                  
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  width = 4,
                  
                  withSpinner(verbatimTextOutput('mob_groups_stats'))
                ),
                # Statistical output for SAMPLE TESTS
                box(
                  title = "Samples Tests",
                  
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  width = 5,
                  
                  withSpinner(verbatimTextOutput('mob_samples_tests'))
                )
                ),
              fluidRow(
                # Statistical output for GROUPS TESTS
                box(
                  title = "Groups Tests",
                  
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  width = 4,
                  
                  withSpinner(verbatimTextOutput('mob_groups_tests'))
                ),
                
                # DOWNLOAD BUTTON BOX FOR MOB METRICS
                box(
                  title = "Download MoB Data to CSV",
                  
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  width = 5,
                  
                  # Download button for GROUPS STATS DATA
                  downloadButton('download_GS', "Download Groups Stats Data"),
                  br(),
                  br(),
                  
                  # Download button for SAMPLES TEST DATA
                  downloadButton('download_ST', "Download Samples Test Data"),
                  br(),
                  br(),
                  
                  # Download button for GROUPS TESTS DATA
                  downloadButton('download_GT', "Download Groups Tests Data"),
                  br(),
                  br(),
                  
                  # Download button for SAMPLES STATS DATA
                  downloadButton('download_SS', "Download Samples Stats Data")
                )
              ),
              fluidRow(
                # Tab Box for MoB Metrics Code
                tabBox(
                  title = "MoB Metrics Code",
                  
                  # side = screen orientation
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  side = "left", width = 8,
                  
                  # Initially selected graph code - observed specied richness
                  selected = "S",
                  
                  # CODE Tab Panels labeled by their type of graph
                  
                  # observed specied richness graph code
                  tabPanel("S", withSpinner(htmlOutput('s_code'))),
                  
                  # number of individuals graph code
                  tabPanel("N", withSpinner(htmlOutput('n_code'))),
                  
                  # rarefied species richness graph code
                  tabPanel("S_n", withSpinner(htmlOutput('sn_code'))),
                  
                  # effective number of species based on PIE graph code
                  tabPanel("S_PIE", withSpinner(htmlOutput('spie_code')))
                )
                
              )
      ),
      
######################################################################################################################## 
                                              # Delta stats tab content #

      tabItem(tabName = "delta_stats",
              h2("Delta Stats"),
              
              # DELTA STATS GRAPH
              fluidRow(
                box(
                  title = "Delta Stats",
                  
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  width = 8,
                  
                  withSpinner(plotOutput('delta_plot'))
                  
                )
              ),
              
              # DOWNLOAD BUTTON BOXES
              fluidRow(
                # TEST BOX
                box(
                  title = "Download Delta Stats Tests Data to CSV",
                  
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  width = 4,
                  
                  # SAD TEST DATA button
                  downloadButton('download_delta_SAD', "Download SAD Test Data"),
                  br(),
                  br(),
                  
                  # N TEST DATA button
                  downloadButton('download_delta_N', "Download N Test Data"),
                  br(),
                  br(),
                  
                  # agg TEST DATA button
                  downloadButton('download_delta_agg', "Download agg Test Data")
                ),
                
                # RAREFACTION BOX
                box(
                  title = "Download Delta Stats Rarefaction Data to CSV",
                  
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  width = 4,
                  
                  # Individual Rarefaction Data button
                  downloadButton('download_rare_ind', "Download Individual Rarefaction Data"),
                  br(),
                  br(),
                  
                  # Sample Rarefaction Data button
                  downloadButton('download_rare_sample', "Download Sample Rarefaction Data")
                )
              ),
              fluidRow(
                # DELTA STATS CODE BOX
                box(
                  title = "Delta Stats Code",
                  
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  width = 8,
                  
                  withSpinner(htmlOutput('delta_code'))
                )
              )
      )
    )
    )
)
  

######################################################################################################################## 
#####################################################SERVER############################################################# 
######################################################################################################################## 

server <- function(input, output) {
  
######################################################################################################################## 
                              # INITIAL SET UP OF REACTIVE VARIABLES# 

  comm <- callModule(csvFile, "comm")
  
  plot_attr <- callModule(csvFile, "plot_attr")
  
  mob_in <- reactive(make_mob_in(comm(), plot_attr()))

  mob_stats <- reactive(get_mob_stats(mob_in(), 'group'))
  
  delta_stats <- reactive(get_delta_stats(mob_in(), 'group',
                                   ref_group = 'uninvaded',
                                   type='discrete', 
                                   log_scale=TRUE,
                                   n_perm=20))
  
  
  
######################################################################################################################## 
                                       # EXPLORITORY ANALYSIS PLOT OUTPUT # 
  
  # Species Rarefaction GRAPH Output
  output$s_rare <- renderPlot({
    plot_rarefaction(mob_in(), 'group', 'spat', lwd = 4, leg_loc = 'topright')
  })
  
  # Individual Rarefaction GRAPH Output - UNPOOLED
  output$i_rare_up <- renderPlot({
    plot_rarefaction(mob_in(), 'group', 'indiv', pooled = F, lwd = 2)
  })
  
  # Individual Rarefaction GRAPH Output - POOLED
  output$i_rare_p <- renderPlot({
    plot_rarefaction(mob_in(), 'group', 'indiv', pooled = T, lwd = 2)
  })
  
  # Species Abundance GRAPH Output - UNPOOLED
  output$up_abu <- renderPlot({
    plot_abu(mob_in(), 'group', type = 'rad', pooled = F, log='x')
  })
  
  # Species Abundance GRAPH Output - POOLED
  output$p_abu <- renderPlot({
    plot_abu(mob_in(), 'group', type = 'rad', pooled = T, log='x')
  })
  
  
######################################################################################################################## 
                                      # EXPLORITORY ANALYSIS CODE OUTPUT #
  
  # Species Rarefaction CODE Output
  output$s_rare_code <- renderText({
    # <br> stands for break line in code
    paste("mob_in <- make_mob_in(community_matrix_FILENAMEHERE, plot_attribute_FILENAMEHERE)",
          "<br>", 
          "<br>",
          "plot_rarefaction(mob_in, 'group', 'spat', lwd = 4, leg_loc = 'topright')")
  })
  
  # Individual Rarefaction CODE Output - UNPOOLED
  output$ir_up_code <- renderText({
    # <br> stands for break line in code
    paste("mob_in <- make_mob_in(community_matrix_FILENAMEHERE, plot_attribute_FILENAMEHERE)",
          "<br>", 
          "<br>",
          "plot_rarefaction(mob_in, 'group', 'indiv', pooled = F, lwd = 2)")
  })
  
  # Individual Rarefaction CODE Output - POOLED
  output$ir_p_code <- renderText({
    # <br> stands for break line in code
    paste("mob_in <- make_mob_in(community_matrix_FILENAMEHERE, plot_attribute_FILENAMEHERE)",
          "<br>", 
          "<br>",
          "plot_rarefaction(mob_in, 'group', 'indiv', pooled = T, lwd = 2)")
  })
  
  # Species Abundance CODE Output - UNPOOLED
  output$up_abu_code <- renderText({
    # <br> stands for break line in code
    paste("mob_in <- make_mob_in(community_matrix_FILENAMEHERE, plot_attribute_FILENAMEHERE)",
          "<br>", 
          "<br>",
          "plot_abu(mob_in, 'group', type = 'rad', pooled = F, log='x')")
    
  })
  
  # Species Abundance CODE Output - POOLED
  output$p_abu_code <- renderText({
    # <br> stands for break line in code
    paste("mob_in <- make_mob_in(community_matrix_FILENAMEHERE, plot_attribute_FILENAMEHERE)",
          "<br>", 
          "<br>",
          "plot_abu(mob_in, 'group', type = 'rad', pooled = T, log='x')")
    
  })
  
  
  
######################################################################################################################## 
                                             # MOB METRIC PLOT OUTPUT # 

  # Species Richness GRAPH Output
  output$mob_s <- renderPlot({
      plot(mob_stats(), 'S')
  })
  
  # Number of Individuals GRAPH Output
  output$mob_n <- renderPlot({
    plot(mob_stats(), 'N')
  })
  
  # Rarefied Species Richness GRAPH Output
  output$mob_Sn <- renderPlot({
    plot(mob_stats(), 'S_n')
  })
  
  # Effective Number ofSpeciesbased on PIE GRAPH Output
  output$mob_Spie <- renderPlot({
    plot(mob_stats(), 'S_PIE')
  })
  
  # ALL MoB Metric GRAPH Output
  output$mob_all <- renderPlot({
    plot(mob_stats(), multi_panel = TRUE)
  })
  
  
######################################################################################################################## 
                                        # MOB METRIC STATISTICAL OUTPUT # 
                        # FOLLOWED BY CORRESPONDING DOWNLOAD BUTTON CODE - downloadHandler #  

  # GROUPS STATS Statistical Output   
  output$mob_groups_stats = renderPrint({
    mob_stats()$groups_stats
  })
  
  # GROUPS STATS CSV download button Output 
  output$download_GS <- downloadHandler(
    filename = "mob_groups_stats.csv",
    content = function(file) {
      write.csv(mob_stats()$groups_stats, file)
    }
  )
  
  # SAMPLES TESTS Statistical Output  
  output$mob_samples_tests = renderPrint({
    mob_stats()$samples_tests
  })
  
  # SAMPLES TESTS CSV download button Output
  output$download_ST <- downloadHandler(
    filename = "mob_samples_tests.csv",
    content = function(file) {
      write.csv(mob_stats()$samples_tests, file)
    }
  )
  
  # GROUPS TESTS Statistical Output
  output$mob_groups_tests = renderPrint({
    mob_stats()$groups_tests
  })
  
  # GROUPS TESTS CSV download button Output
  output$download_GT <- downloadHandler(
    filename = "mob_groups_tests.csv",
    content = function(file) {
      write.csv(mob_stats()$groups_tests, file)
    }
  )
  
  # SAMPLES STATS CSV download button Output
  # NO Statistical Output in app for SAMPLES STATS
  output$download_SS <- downloadHandler(
    filename = "mob_samples_stats.csv",
    content = function(file) {
      write.csv(mob_stats()$samples_stats, file)
    }
  )
 
  
######################################################################################################################## 
                                            # MOB METRIC CODE OUTPUT # 
 
  # Species Richness CODE Output 
  output$s_code <- renderText({
    # <br> stands for break line in code
    paste("mob_in <- make_mob_in(community_matrix_FILENAMEHERE, plot_attribute_FILENAMEHERE)",
          "<br>", 
          "<br>",
          "mob_stats <- get_mob_stats(mob_in, 'group')",
          "<br>",
          "<br>",
          "plot(mob_stats, 'S')")
  })
  
  # Number of Individuals CODE Output
  output$n_code <- renderText({
    # <br> stands for break line in code
    paste("mob_in <- make_mob_in(community_matrix_FILENAMEHERE, plot_attribute_FILENAMEHERE)",
          "<br>", 
          "<br>",
          "mob_stats <- get_mob_stats(mob_in, 'group')",
          "<br>",
          "<br>",
          "plot(mob_stats, 'N')")
  })
  
  # Rarefied Species Richness CODE Output
  output$sn_code <- renderText({
    # <br> stands for break line in code
    paste("mob_in <- make_mob_in(community_matrix_FILENAMEHERE, plot_attribute_FILENAMEHERE)",
          "<br>",
          "<br>",
          "mob_stats <- get_mob_stats(mob_in, 'group')",
          "<br>",
          "<br>",
          "plot(mob_stats, 'S_n')")
  })
  
  # Effective Number ofSpeciesbased on PIE CODE Output
  output$spie_code <- renderText({
    # <br> stands for break line in code
    paste("mob_in <- make_mob_in(community_matrix_FILENAMEHERE, plot_attribute_FILENAMEHERE)",
          "<br>", 
          "<br>",
          "mob_stats <- get_mob_stats(mob_in, 'group')",
          "<br>",
          "<br>",
          "plot(mob_stats, 'S_PIE')")
  
  })
  
  # All MoB Metrics CODE Output
  output$all_mob_code <- renderText({
    # <br> stands for break line in code
    paste("mob_in <- make_mob_in(community_matrix_FILENAMEHERE, plot_attribute_FILENAMEHERE)",
          "<br>", 
          "<br>",
          "mob_stats <- get_mob_stats(mob_in, 'group')",
          "<br>",
          "<br>",
          "plot(mob_stats, multi_panel = TRUE)")
    
  })
  
######################################################################################################################## 
                                             # DELTA STATS PLOT OUTPUT #   

  output$delta_plot <- renderPlot({
    plot(delta_stats(), 'invaded', 'uninvaded')
    
  })

######################################################################################################################## 
                                   # DOWNLOAD BUTTONS FOR DELTA STATS STATISTICS #     
  
  # SAD Test Download Button
  output$download_delta_SAD <- downloadHandler(
    filename = "delta_SAD_test.csv",
    content = function(file) {
      write.csv(delta_stats()$SAD, file)
    }
  )
  
  # N Test Download Button
  output$download_delta_N <- downloadHandler(
    filename = "delta_N_test.csv",
    content = function(file) {
      write.csv(delta_stats()$N, file)
    }
  )
  
  # agg Test Download Button
  output$download_delta_agg <- downloadHandler(
    filename = "delta_agg_test.csv",
    content = function(file) {
      write.csv(delta_stats()$agg, file)
    }
  )
  
  # Individual Rarefaction Data Download Button
  output$download_rare_ind <- downloadHandler(
    filename = "delta_indiv_rare.csv",
    content = function(file) {
      write.csv(delta_stats()$indiv_rare, file)
    }
  )
  
  # Sample Rarefaction Data Download Button
  output$download_rare_sample <- downloadHandler(
    filename = "delta_sample_rare.csv",
    content = function(file) {
      write.csv(delta_stats()$sample_rare, file)
    }
  )
  
######################################################################################################################## 
                                          # DELTA STATS CODE OUTPUT # 
  
  output$delta_code <- renderText({
    paste("mob_in <- make_mob_in(community_matrix_FILENAMEHERE, plot_attribute_FILENAMEHERE)",
          "<br>", 
          "<br>",
          "delta_stats <- get_delta_stats(mob_in, 'group', ref_group = 'uninvaded', type='discrete', log_scale=TRUE, n_perm=20)",
          "<br>",
          "<br>",
          "plot(delta_stats, 'invaded', 'uninvaded')")
    
  })
  
}

######################################################################################################################## 
                                         # GUI FUNCTION SET UP TO RUN APP # 

# Create a Shiny app object
#shinyApp(ui = ui, server = server)

#' mobr package Graphic User Interface
#'
#' User interface of the mobr package.
#'
#' @param port char. The TCP port that the application should listen on (see
#'   \code{\link[shiny]{runApp}} for more details).
#' @param host char. The IPv4 address that the application should listen on (see
#'   \code{\link[shiny]{runApp}} for more details).
#' @param working.directory char. Directory in which the application will run.
#'
#' @return Open a window with a shiny app to use the soar package with an
#'   user-friendly interface.
#'
#' @examples
#' \dontrun{
#' gui()
#' }
#'
#' @export
gui <- function(port = getOption("shiny.port"),
                host = getOption("shiny.host", "127.0.0.1")) {

  shiny::runApp(shinyApp(ui = ui, server = server),
                display.mode = "normal", port = port, host = host)
  rm(ui, server, envir = .GlobalEnv)
}
