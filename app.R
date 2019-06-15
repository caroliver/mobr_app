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
  dashboardHeader(title = "mobr app"),

  # Initalize and name tabs in the sidebar
  dashboardSidebar(
    sidebarMenu(style = "position: fixed; overflow: visible;",
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Data Input", tabName = "DataInput", icon = icon("table")),
      menuItem("Exploritory Analysis", tabName = "exploritory_analysis", icon = icon("angle-right"),
        menuSubItem("Rarefaction", tabName = "rare_tab", icon = icon("angle-right")),
        menuSubItem("Abundance", tabName = "abu_tab", icon = icon("angle-right"))),
      menuItem("MoB Metrics", tabName = "mob_tab", icon = icon("angle-right")),
      menuItem("Delta Stats", tabName = "delta_stats", icon = icon("angle-right")),
      menuItem("mobr GitHub Page", icon = icon("github"), 
               href = "https://github.com/MoBiodiv/mobr"),
      menuItem("Submit a CODE Issue", icon = icon("exclamation-triangle"), 
               href = "https://github.com/MoBiodiv/mobr/issues"),
      menuItem("Submit an APP Issue", icon = icon("exclamation-triangle"), 
               href = "https://github.com/MoBiodiv/mobr_app/issues")
    )
  ),
  
  # Main page or body of tabs
  # This is all content that will be presented when the tab is clicked on in the main body of the app
  # Each 'tabItem' is labled with is corresponding sidebar tab name using a comment
  dashboardBody(
    
    tags$script(HTML("$('body').addClass('fixed');")), 
    
    tabItems(
      
      # Home tab content
      tabItem(tabName = "Home",
              h2("Measurement of Biodiversity Shiny App"),
              fluidRow(
              # Instructions for navigating the mobr app detailed in this first box
              box(
                status = "primary",
            
                # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                width = 6,
                
                # Heading for the box - name preceded by icon of a globe
                h3(icon("globe"), "Navigation Tabs"),
                
                # br() stands for break line in code
                br(),
                h4(icon("table"),"Data Input"),
                "- Where you will enter your community matrix and plot data as csv files", 
                br(), 
                br(), 
                h4(icon("angle-right"),"Exploratory Analysis"),
                "- Graphs and Code for Exploratory Analysis data",
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
                status = "primary",
                # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                width = 5,
                # Heading for the box - name preceded by icon of a link
                h3(icon("link"), "Link Tabs"),
                # br() stands for break line in code
                br(),
                h3(icon("github")),
                "If you want to learn more about mobr from the source check out the first link tab 'mobr Github Page'. It will take you to mobr's main GitHub page.",
                br(),
                br(),
                h4(icon("exclamation-triangle")),
                "If you run into a problem with the underlying code or the application itself, please let the developers know:",
                br(),
                br(),
                "- Use the 'Submit a CODE Issue' button to submit an issue with the underlying application code.",
                br(),
                "- Use the 'Submit an APP Issue' button to submit an issue with the application itself.",
                br(),
                br(),
                "We appreciate your feedback!"
              )
              ),
              fluidRow(
                # Box giving credit to authors of mobr/the mobr app
                box(
                  status = "primary",
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  width = 4,
                  # Heading for the box - name preceded by icon of a small group of people or 'users'
                  h3(icon("users"), "The people behind mobr:"),
                  tags$b("Primary Author: "),
                  # br() stands for break line in code
                  br(),
                  "Daniel McGlinn - email: danmcglinn@gmail.com",
                  br(),
                  br(),
                  tags$b("Secondary Authors: "),
                  br(),
                  "Xiao Xiao - email: xiao@weecology.org",
                  br(),
                  "Felix May - email: felix.may@idiv.de",
                  br(),
                  "Thor Engel - email: thore.engel@idiv.de",
                  br(),
                  "Caroline Oliver - email: olivercs@g.cofc.edu"
                  )
              )
      ),

######################################################################################################################## 
                                    # Data tab content #

      tabItem(tabName = "DataInput",
              # Heading for main body of tab
              fluidRow(
                
                box(
                  status = "primary",
                  h2("Choose your data type below:"),
                  
                  selectInput("sample_data",
                              label = "Select Data Input Type:",
                              choices = list("Use Sample Data" = 'TRUE',"Use Personal Data" = 'FALSE'),
                              selected = 'FALSE'),
                  
                  uiOutput("conditionalInput_commData"),
                  uiOutput("conditionalInput_plotData"),
                  
                  uiOutput("conditionalInput_coordNamesNumeric"),
                  
                  uiOutput("conditionalInput_SingleCoordNameParam"),
                  uiOutput("conditionalInput_LatitudeParam"),
                  uiOutput("conditionalInput_LongitudeParam")
                  
                  
                )
        )
      ),


########################################################################################################################         
                                       # Exploratory Analysis tab content #      

# RAREFACTION (SUB) TAB      
tabItem(tabName = "rare_tab",
        
        # Heading for main body of tab
        h2("Rarefaction"),
        
        # First/top box on Exploratory Analysis tab that displays graphs
        # each 'tabPanel' represents a difference exploratory graph
        fluidRow(
          box(
            status = "primary",
            width = 4,
            # Title of the box
            title = "Rarefaction Parameter Selection",
            
            selectInput("rare_method",
                        label = "Select Method Type:",
                        choices = list("Individual" = 'indiv',"Sample" = 'samp',"Spatial" = 'spat'),
                        selected = 'indiv'),
            
            uiOutput("conditionalInput_rare_groupParam"),
            
            uiOutput("conditionalInput_choice"),
            
            #numericInput("dens_rat", "Density Ratio:", 1, min = -100, max = 100),
            
            selectInput("rare_legend",
                        label = "Select Legend Location:",
                        choices = list("Top Left" = 'topleft',"Bottom Left" = 'bottomleft',"Top Right" = 'topright', "Bottom Right" = 'bottomright'),
                        selected = 'topleft'),
            
            br(),
            br(),
            
            h4("Download Current Plot"),
            
            downloadButton(outputId = "rare_plot", label = "Download Current Plot")
            
          ),
          box(
            status = "primary",
            # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
            width = 8,
            
            # 'Height' refers to how many pixels high your box will be (different than width implementation)
            height = "800px",
            
            # Title of the box
            title = "Rarefaction Plot",
            
            # the plotOutput funtion parameter corresponds to the name of an output code that creates the plot in the server portion of the app
            # height of graph must be smaller (less pixels) than box height to fit appropriately
            withSpinner(plotOutput('rarefaction_plot', height = "725px"))
          )
        )
),

# ABUNDANCE (SUB) TAB      
tabItem(tabName = "abu_tab",
        
        # Heading for main body of tab
        h2("Abundance"),
        
        # First/top box on Exploratory Analysis tab that displays graphs
        # each 'tabPanel' represents a difference exploratory graph
        fluidRow(
          box(
            status = "primary",
            width = 4,
            # Title of the box
            title = "Abundance Parameter Selection",
            
            selectInput("abu_type",
                        label = "Select Abundance Type:",
                        choices = list("Species Abundance" = 'sad',"Rank Abundance" = 'rad'),
                        selected = 'sad'),
            
            uiOutput("conditionalInput_abu_groupParam"),
            
            radioButtons("pool_abu",
                         label = "Pooled or Unpooled:",
                         choices = list("Pooled" = 'T',"Unpooled" = 'F'),
                         selected = 'T'),
            
            selectInput("abu_legend",
                        label = "Select Legend Location:",
                        choices = list("Top Left" = 'topleft',"Bottom Left" = 'bottomleft',"Top Right" = 'topright', "Bottom Right" = 'bottomright'),
                        selected = 'topleft'),
            
            br(),
            br(),
            
            h4("Download Current Plot"),
            
            downloadButton(outputId = "abu_plot", label = "Download Current Plot")
            
          ),
          box(
            status = "primary",
            # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
            width = 8,
            
            # 'Height' refers to how many pixels high your box will be (different than width implementation)
            height = "800px",
            
            # Title of the box
            title = "Abundance Plot",
            
            # the plotOutput funtion parameter corresponds to the name of an output code that creates the plot in the server portion of the app
            # height of graph must be smaller (less pixels) than box height to fit appropriately
            withSpinner(plotOutput('abundance_plot', height = "725px"))
          )
        )
        ),

########################################################################################################################         
                                               # Mob Metrics Tab #
# MOB METRICS TAB     
      tabItem(tabName = "mob_tab",
              
              # Heading for main body of tab
              h2("MoB Metrics"),
              
              # First/top box that displays graph with all MoB Metric data
              fluidRow(
                box(
                    status = "primary",
                    h2("MoB Metrics Paramenter Selection"),
                    width = 4,
                    
                    uiOutput("conditionalInput_mob_groupParam"),
                    numericInput("effort_min_param", "Rarefied Richness Calculation Minimum # of Individuals:", 5, min = 1, max = 100000),
                    
                    selectInput("extrapolate_param",
                                label = "Extrapolation:",
                                choices = list("True" = 'TRUE',"False" = 'FALSE'),
                                selected = 'TRUE'),
                    
                    uiOutput("conditionalInput_return_NA"), 
                    
                    numericInput("rareThres_param", "Rarefaction Threshold:", 0.05, min = 0, max = 1),
                    
                    numericInput("nPerm_mob_param", "Number of Permutations:", 199, min = 1, max = 100000),
                    
                    radioButtons("boot_groups_param",
                                 label = "Bootstrap Group:",
                                 choices = list("True" = 'TRUE',"False" = 'FALSE'),
                                 selected = 'FALSE'),
                    
                    uiOutput("conditionalInput_conf_level"),
                    
                    actionButton("newMobModel", "Calculate MoB Plot")
                  ),
                
                  tabBox(
                    width = 8,
                    height = "800px",
                    title = "MoB Metrics Plot",
                    tabPanel("N", withSpinner(plotOutput('mob_plot_N', height = '750px'))),
                    tabPanel("S", withSpinner(plotOutput('mob_plot_S', height = '750px'))),
                    tabPanel("f_0", withSpinner(plotOutput('mob_plot_f_0', height = '750px'))),
                    tabPanel("pct_rare", withSpinner(plotOutput('mob_plot_pct_rare', height = '750px'))),
                    tabPanel("S_PIE", withSpinner(plotOutput('mob_plot_S_PIE', height = '750px')))
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
                  status = "primary",
                  h2("Delta Stats Paramenter Selection"),
                  width = 4,
                  
                  uiOutput("conditionalInput_delta_groupParam"),
                  
                  #uiOutput("conditionalInput_delta_treatGroup"),
                  
                  selectInput("tests_param",
                              label = "Select Test Type:",
                              choices = list("SAD" = 'SAD',"N" = 'N',"agg" = 'agg'),
                              selected = 'SAD', multiple = TRUE),
                  
                  #uiOutput("conditionalInput_minPlots"),
                  
                  selectInput("type_param",
                              label = "Select Type:",
                              choices = list("Continuous" = 'continuous',"Discrete" = 'discrete'),
                              selected = 'discrete'),
                  
                  uiOutput("conditionalInput_refGroup"), 
                  
                  textInput("stats_param", label = "Enter Stats Value:", value = "NULL"),
                  
                  textInput("inds_param", label = "Enter Inds Value:", value = "NULL"),
                  
                  radioButtons("logScale_param",
                               label = "Log Scale:",
                               choices = list("True" = 'TRUE',"False" = 'FALSE'),
                               selected = 'TRUE'),
                  
                  selectInput("denStat_param",
                              label = "Select Density Statistic:",
                              choices = list("Mean" = 'mean',"Max" = 'max',"Min" = 'min'),
                              selected = 'mean'),
                  
                  numericInput("nPerm_param", "Number of Permutations:", 20, min = 1, max = 100000),
                  
                  radioButtons("overallP_param",
                               label = "Overall P-value:",
                               choices = list("True" = 'TRUE',"False" = 'FALSE'),
                               selected = 'FALSE'),
                  
                  actionButton("newDeltaModel", "Calculate Delta Plot")
                ),
                box(
                  status = "primary",
                  title = "Delta Stats",
                  
                  # 'Width' refers to how much of the screen you would like the box to take up (max = 12)
                  width = 8,
                  height = "800px",
                  
                  withSpinner(plotOutput('delta_plot', height = "725px"))
                  
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
                              # SAMPLE/PERSONAL DATA UPLOAD # 

  comm_data <- read.csv(text = getURL("https://raw.githubusercontent.com/caroliver/mobr_app/master/data/inv_comCSV.csv"))
  comm_data <- comm_data[,2:ncol(comm_data)]
  
  plot_data <- read.csv(text = getURL("https://raw.githubusercontent.com/caroliver/mobr_app/master/data/inv_plotCSV.csv"))
  plot_data <- plot_data[,2:4]
  
  output$conditionalInput_rare_groupParam <- renderUI({
    if (is.null(plot_data) == TRUE){
      textOutput("error_message", "Please enter data in the Data Tab")
    }
    else{
      selectInput("group_param_rare", "Variable Name: ", choices = colnames(plot_data))
    }
  })
  
  output$conditionalInput_abu_groupParam <- renderUI({
    if (is.null(plot_data) == TRUE){
      textOutput("error_message", "Please enter data in the Data Tab")
    }
    else{
      selectInput("group_param_abu", "Variable Name: ", choices = colnames(plot_data))
    }
  })
  
  output$conditionalInput_mob_groupParam <- renderUI({
    if (is.null(plot_data) == TRUE){
      textOutput("error_message", "Please enter data in the Data Tab")
    }
    else{
      selectInput("group_param_mob", "Variable Name: ", choices = colnames(plot_data))
    }
  })
  
  output$conditionalInput_delta_groupParam <- renderUI({
    if (is.null(plot_data) == TRUE){
      textOutput("error_message", "Please enter data in the Data Tab")
    }
    else{
      selectInput("group_param_delta", "Variable Name: ", choices = colnames(plot_data))
    }
  })
  
  
  output$conditionalInput_commData <- renderUI({
    if(input$sample_data == 'FALSE'){
      # csvFileInput command for Community Matrix Data
      csvFileInput("comm", "Upload Community Matrix Data")
    }
  })
  
  output$conditionalInput_plotData <- renderUI({
    if(input$sample_data == 'FALSE'){
      # csvFileInput command for Plot Attribute Data
      csvFileInput("plot_attr", "Upload Plot Attribute Data")
    }
  })
  
  output$conditionalInput_coordNamesNumeric <- renderUI({
    if(input$sample_data == 'FALSE'){
      # coord_names numerical param
      selectInput("coord_namesNumeric",
                  label = "Select Number of Coordinate Names:",
                  choices = list("0" = 0,"1" = 1,"2" = 2),
                  selected = 0)
    }
  })
  
  output$conditionalInput_SingleCoordNameParam <- renderUI({
    if(input$coord_namesNumeric  == 1){
      # csvFileInput command for Plot Attribute Data
      textInput("mobIn_singleCoordParam", label = "Enter Single Coordinate Name:", value = "NULL")
    }
  })
  
  output$conditionalInput_LatitudeParam <- renderUI({
    if(input$coord_namesNumeric  == 2){
      # csvFileInput command for Plot Attribute Data
      textInput("mobIn_latitude_param", label = "Enter Latitude Column Name:", value = "NULL")
    }
  })
  
  output$conditionalInput_LongitudeParam <- renderUI({
    if(input$coord_namesNumeric  == 2){
      # csvFileInput command for Plot Attribute Data
      textInput("mobIn_longitude_param", label = "Enter Longitude Column Name:", value = "NULL")
    }
  })
  
  
  
  mob_in <- reactive({
    if(input$sample_data == 'TRUE'){
      comm <- comm_data
      plot_attr <- plot_data
      
      mob_in <- reactive(make_mob_in(comm, plot_attr))
      
    }
    else if(input$sample_data != 'TRUE'){
      comm <- callModule(csvFile, "comm")
      plot_attr <- callModule(csvFile, "plot_attr")
      
      if(input$coord_namesNumeric  == 0){
        coord_names_param = NULL
      }
      else if(input$coord_namesNumeric  == 1){
        coord_names_param = c(input$mobIn_singleCoordParam)
      }
      else if(input$coord_namesNumeric  == 2){
        # longitude supplied first and latitude is supplied second in accordance with the make_mob_in documentation
        coord_names_param = c(input$mobIn_longitude_param, input$mobIn_latitude_param)
      }
      
      mob_in <- reactive(make_mob_in(comm(), plot_attr(), coord_names = coord_names_param))
    }
    return(mob_in())
  })



  
  
######################################################################################################################## 
                                       # EXPLORITORY ANALYSIS OUTPUT # 
  
  # RAREFACTION
  
  # Rarefaction Reactive Variables
  # Rarefaction method selection
  rare_group_variable <- reactive({input$group_param_rare})
  rare_method <- reactive({input$rare_method})
  
  # Pooled vs unpooled condidtional selection option
  # Ability to select only shows up when the -individual- rarefaction method is chosen
  output$conditionalInput_choice <- renderUI({
    if(input$rare_method == 'indiv'){
      radioButtons("pool_rare",
                   label = "Pooled or Unpooled:",
                   choices = list("Pooled" = 'T',"Unpooled" = 'F'),
                   selected = 'T')
    }
  })
  
  # Rarefaction legend location selection
  rare_legend <- reactive({input$rare_legend})
  
  # Pooled vs unpooled variable definition
  # Takes into account if -indiviudal- rarfaction is NOT selected and sets varaible approproately
  # Individual = pooled vs unpooled selection / Not individual = default to pooled
  pool_rare_val <- reactive({
    if(input$rare_method == 'indiv'){
      pool_rare_val <- input$pool_rare
    }else if(input$rare_method != 'indiv'){
      pool_rare_val <- TRUE
    }
    return(pool_rare_val)
  })
  
  # Rarefaction Plot Output
  output$rarefaction_plot <- renderPlot({plot_rarefaction(mob_in(), rare_group_variable(),
                                                          rare_method(), pooled = pool_rare_val(), 
                                                          leg_loc = rare_legend())})

  output$rare_plot <- downloadHandler(
    filename = function() {
      "ExploratoryAnalysisPlots_Rarefaction.pdf"
    },
    content = function(file) {
      pdf(file)
      print(
        plot_rarefaction(mob_in(), rare_group_variable(), rare_method(), pooled = pool_rare_val(), leg_loc = rare_legend())
      )
      dev.off()
    }
  )   
  
  # ABUNDANCE
  
  # Abundance Reactive Variables
  abu_group_variable <- reactive({input$group_param_abu})
  abu_type <- reactive({input$abu_type})
  pool_abu <- reactive({input$pool_abu})
  abu_legend <- reactive({input$abu_legend})
  
  # Abundance Plot Output
  output$abundance_plot <- renderPlot({
    plot_abu(mob_in(), abu_group_variable(), type = abu_type(), pooled = pool_abu(), log='x', leg_loc = abu_legend())
  })
  
  output$abu_plot <- downloadHandler(
    filename = function() {
      "ExploratoryAnalysisPlots_Abundance.pdf"
    },
    content = function(file) {
      pdf(file)
      print(
        plot_abu(mob_in(), abu_group_variable(), type = abu_type(), pooled = pool_abu(), log='x', leg_loc = abu_legend())
      )
      dev.off()
    }
  ) 
  
######################################################################################################################## 
                                             # MOB METRIC PLOT OUTPUT # 

  # Delta Stat Reactive Parameter Variables 
  group_param_mob <- reactive({input$group_param_mob})
  #index_param <- reactive({input$index_param})
  effort_min_param <- reactive({input$effort_min_param})
  extrapolate_param <- reactive({input$extrapolate_param})
  returnNA_param <- reactive({input$returnNA_param})
  rareThres_param <- reactive({input$rareThres_param})
  nPerm_mob_param <- reactive({input$nPerm_mob_param})
  boot_groups_param <- reactive({input$boot_groups_param})
  conf_level_param <- reactive({input$conf_level_param})
  
  # Reference Group Conditional Parameter Rendering
  output$conditionalInput_return_NA <- renderUI({
    if(input$extrapolate_param == 'FALSE'){
      radioButtons("returnNA_param",
                   label = "Return NA:",
                   choices = list("True" = 'TRUE',"False" = 'FALSE'),
                   selected = 'FALSE')
    }
  })
  
  # Reference Group Conditional Parameter Rendering
  output$conditionalInput_conf_level <- renderUI({
    if(input$boot_groups_param == 'TRUE'){
      numericInput("conf_level_param", "Confidence Level:", 0.95, min = 0, max = 10)
    }
  })
  
  
  mob_stats <- eventReactive(input$newMobModel, {
    get_mob_stats(mob_in(), group_param_mob(), index = c("N", "S", "f_0", "pct_rare","S_PIE"), effort_samples = 100, effort_min = effort_min_param(), 
                           extrapolate = extrapolate_param(), return_NA = returnNA_param(),
                           rare_thres = rareThres_param(), n_perm = nPerm_mob_param(), 
                           boot_groups = boot_groups_param(), conf_level = conf_level_param())
                          #   "N", "S", "S_n", "S_asymp", "f_0", "pct_rare", "PIE", "S_PIE"
  })
  
  mob_stats2 <- function(){
    get_mob_stats(mob_in(), group_param_mob(), index = c("N", "S", "f_0", "pct_rare","S_PIE"), effort_samples = 100, effort_min = effort_min_param(), 
                  extrapolate = extrapolate_param(), return_NA = returnNA_param(),
                  rare_thres = rareThres_param(), n_perm = nPerm_mob_param(), 
                  boot_groups = boot_groups_param(), conf_level = conf_level_param())
    #   "N", "S", "S_n", "S_asymp", "f_0", "pct_rare", "PIE", "S_PIE"
  }
  
  # Species Richness GRAPH Output
  output$mob_plot_N <- renderPlot({
      plot(mob_stats(), "N")
  })#, height = "750px")
  
  output$mob_plot_S <- renderPlot({
    plot(mob_stats(), "S")
  })#, height = "750px")
  
  output$mob_plot_f_0 <- renderPlot({
    plot(mob_stats(), "f_0", multi_panel = TRUE)
  })#, height = "750px")
  
  output$mob_plot_pct_rare <- renderPlot({
    plot(mob_stats(), "pct_rare")
  })#, height = "750px")
  
  output$mob_plot_S_PIE <- renderPlot({
    plot(mob_stats(), "S_PIE")
  })#, height = "750px")
 
######################################################################################################################## 
                                             # DELTA STATS PLOT OUTPUT #   
  
  # Delta Stat Reactive Parameter Variables 
  group_param_delta <- reactive({input$group_param_delta})
  tests_param <- reactive({input$tests_param})
  type_param <- reactive({input$type_param})
  stats_param <- reactive({input$stats_param})
  inds_param <- reactive({input$inds_param})
  logScale_param <- reactive({input$logScale_param})
  denStat_param <- reactive({input$denStat_param})
  nPerm_param <- reactive({input$nPerm_param})
  overallP_param <- reactive({input$overallP_param})
  
  
  group_param_delta_name <- isolate(input$group_param_delta)
  
  output$conditionalInput_delta_treatGroup <- renderUI({
    if (is.null(plot_data) == FALSE){
      whichcol = input$group_param_delta
      selectInput("treatGroup_param", "Treatment Group Name: ", choices = levels(unique(plot_data$whichcol)))
    }
  })
  
  refGroup_param <- reactive({
    if(input$type_param == 'discrete'){
      refGroup_param <- input$refGroup_param
      
    }else if(input$type_param != 'discrete'){
      refGroup_param <- NULL
    }
    return(refGroup_param)
  })
  
  # Reactive Delta Stats 'get_delta_stats' Function Call
  delta_stats <- eventReactive(input$newDeltaModel, {get_delta_stats(mob_in(), group_param_delta(), ref_group = refGroup_param(), 
                                          tests = tests_param(),
                                          type = type_param(),
                                          stats = NULL, inds = NULL,
                                          log_scale = logScale_param(),
                                          density_stat = denStat_param(),
                                          n_perm=nPerm_param(), overall_p = overallP_param()
                                          ) 
  })
  
  # Delta Stats Plot Rendering
  output$delta_plot <- renderPlot({
    plot(delta_stats(), stat = 'b1')
  })
}
  
shinyApp(ui = ui, server = server)

