
# Sun Apr 22 21:02:32 2018


# Page controls ###############


library( leaflet )


navbarPage( "ForC", id = "nav",
            tabPanel( "Interactive map",
                      div( class = "outer",
                           tags$head(
                             # Include our custom CSS
                             includeCSS( "styles.css" )
                           ),
                           
                           # leaflet map
                           leafletOutput( outputId = "map", width = "100%", height = "100%" ),
                           
                           
                           # side ponel to allow user to choose among variables
                           # shiny versions prior to 0.11 should use class="modal" instead.
                           if ( usingSimplified )
                           {
                             absolutePanel( id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 500, height = "auto",
                                            
                                            h3( "Select pattern" ),
                                            
                                            radioButtons( inputId = "varType", label = h4( "Variable type:" ), 
                                                          choices = list( "Flux" = "flux",
                                                                          "Stock" = "stock" ), selected = 'flux' ),
                                            
                                            # radioButtons( "measType", h4( "Type of measurement:" ),
                                            #               list( "Carbon" = "carbon" ), selected = 'carbon' ),
                                            
                                            checkboxGroupInput( inputId = "ExcludRec", label = h4( "Exclude the following stands:" ),
                                                          choices = list( "Managed" = "managed",
                                                                          "Disturbed since stand initiation" = "disturbed")),
                                            sliderInput("age", label = h4("Select a range of stand age"), min = 0, max = 4000, value = c(0, 4000), width = "100%"),
                                            checkboxGroupInput( inputId = "ageInclude", label = h5( "Include:" ),
                                                                choices = list( "Mature stands of unknown age (999)" = "999",
                                                                                "Unknown age (NAC, NI, NRA)" = "unknown"),
                                                                selected = c("999", "unknown")),
                                            
                                            uiOutput( 'color' ), 
                                            # selectInput( "color", h4( "Variable" ), choices = varLst ),
                                            # selectInput( "size", "Size", vars, selected = "npp"), # isa
                                            plotOutput("histCentile", height = 200),
                                            textOutput("nbrec")#,
                                            
                                            # sliderInput("dates",
                                            #             h4( "Stand age:" ),
                                            #             min = 0,  max = 100, value = c( 0, 100 ), step = 1 )
                             )
                           } else
                           {
                             absolutePanel( id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 350, height = "auto",
                                            
                                            h3( "Select pattern" ),
                                            
                                            radioButtons( inputId = "varType", label = h4( "Variable type:" ), 
                                                          choices = list( "Flux" = "flux",
                                                                          "Stock" = "stock",
                                                                          "Secondary" = "secondary" ), selected = 'flux' ),
                                            
                                            radioButtons( "measType", h4( "Type of measurement:" ),
                                                          list( "Organic matter" = "orgmat",
                                                                "Carbon" = "carbon" ), selected = 'carbon' ),
                                            
                                            uiOutput( 'color' ), 
                                            # selectInput( "color", h4( "Variable" ), choices = varLst ),
                                            # selectInput( "size", "Size", vars, selected = "npp"), # isa
                                            plotOutput("histCentile", height = 200)#,
                                            
                                            # sliderInput("dates",
                                            #             h4( "Stand age:" ),
                                            #             min = 0,  max = 100, value = c( 0, 100 ), step = 1 )
                             )
                           },
                           
                           
                           
                           # citation
                           tags$div( id = "cite",
                                     'Exploring ', tags$em('ForC: A global database of forest carbon stocks and fluxes'), ' [',
                                     tags$a( href = "https://doi.org/10.1002/ecy.2229", 
                                                     "Anderson-Teixeira et al 2018", 
                                                     target="_blank"), '], [',
                                     tags$a( href = "https://github.com/forc-db/ForC/tree/master/shiny/global_data_visualization", 
                                             "Shiny App Source Code", 
                                             target="_blank"), '], '
                                     
                           )
                           
                      )
            )
)
