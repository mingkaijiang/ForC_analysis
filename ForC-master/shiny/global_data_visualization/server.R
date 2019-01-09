
# Sun Apr 22 21:02:32 2018


# Aplication behavior #########


library( leaflet )
library( RColorBrewer )
library( scales )
library( reshape2 )
library( plyr )
library( ggplot2 )




function( input, output, session ) {
  
  ## Interactive Map ###########
  
  # Create the map
  map <-  renderLeaflet({
      leaflet() %>%
      # addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")
     addProviderTiles( providers$CartoDB.Positron) %>% #CartoDB.Positron #OpenMapSurfer.Grayscale
      setView( lng = 0, lat = 0, zoom = 3 ) %>%
      setMaxBounds( lng1 = -240, lat1 = -90, lng2 = 240, lat2 = 90)
  })
    
  output$map <- map
  # output$map <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles( providers$OpenMapSurfer.Grayscale ) %>% #CartoDB.Positron #OpenMapSurfer.Grayscale
  #     setView( lng = 0, lat = 0, zoom = 3 ) %>%
  #     setMaxBounds( lng1 = -240, lat1 = -90, lng2 = 240, lat2 = 90 )
  # })
  
  # Subset the data
  
  data <- reactive({
    
    ## subset for the variable of interest
    ddk <- dd[dd$variable.name %in% input$color,]

    ## exclude managed or disturbed, or not
    if(length(input$ExcludRec) == 1) {
      if ("managed" %in% input$ExcludRec) {
        ddk <- subset( ddk, managed %in% 0)
      }
      if ("disturbed" %in% input$ExcludRec) {
        ddk <- subset( ddk, disturbed %in% 0)
      }
    }
    
    if(length(input$ExcludRec) == 2) {
      if (all(c("managed", "disturbed") %in% input$ExcludRec)) {
       ddk <- subset( ddk, managed %in% 0 & disturbed %in% 0)
      }
    }
    
    ## select stand age
    
    ### make a numeric vector of age 
    num.age <- as.numeric(as.character(ddk$stand.age))
    
    ### select between min and max of the input, depending on how to treat missing values
    
    if(length(input$ageInclude) == 0) {
      ddk <- ddk[(num.age >= input$age[1] & num.age <= input$age[2]) & !is.na(num.age) & num.age != 999,]
    }
    
    if(length(input$ageInclude) == 1) {
      
      if(grepl("999", input$ageInclude)) {
        ddk <- ddk[(num.age >= input$age[1] & num.age <= input$age[2]) & !is.na(num.age),] #  | num.age == 999
      }
      
      if(grepl("unknown", input$ageInclude)) {
        ddk <- ddk[((num.age >= input$age[1] & num.age <= input$age[2]) | is.na(num.age)) & num.age != 999,]
      }
      
    }
    
    if(length(input$ageInclude) == 2) {
      ddk <- ddk[(num.age >= input$age[1] & num.age <= input$age[2]) | is.na(num.age) | num.age == 999,]
    }
    
    # 
    # sliderInput("age", label = h4("Select a range of stand age"), min = 0, max = 4000, value = c(0, 4000)),
    # checkboxGroupInput( inputId = "ageInclude", label = h5( "Include:" ),
    #                     choices = list( "Mature stands of unknown age (999)" = "999",
    #                                     "Unknown age (NAC, NI, NRA)" = "unknown"),
    #                     selected = c("999", "unknown")),
    
    
    return(ddk)
  })
  
  # subset by stand age
  # dd <- subset( dd, stand.age >= input$dates[ 1 ] & stand.age <= input$dates[ 2 ] )
   
  # Histogram

    output$histCentile <- renderPlot({
      hist( data()$meanvar,
      main = '',
      xlab = input$color,
      col =  '#66CC00', # '#00DD00',
      border = 'white')
      })
  
    
  # Number of records
 
  output$nbrec <- renderText({paste("Number of records =", nrow(data()))})

  
  # color varies depending on the variable selected
  observe({
    output$color <- renderUI( selectInput(  inputId = "color", label = h4( "Variable" ), 
                                            selectize = TRUE, choices = varLst[[ input$varType ]], 
                                            # selectize = TRUE, choices = varLst[[ input$varType ]][[ input$measType ]],
                                            selected = 'GPP_C' ) )
  })
  
  
  # point size and color vary according to user input
  observe({
 
    ddk <- data()
    colorData <- ddk$meanvar
    if ( length( colorData ) > 0 )
    {
      pal <- colorBin( "viridis", colorData, 7, pretty = FALSE, na.color = NA )
      if ( sum( !is.na( colorData ) ) == 1 )
      {
        pal <- colorBin( "viridis", na.omit( colorData ) * c( 0.99, 1.01 ) , 7, pretty = FALSE, na.color = NA )
      }
    }
    
    radius <- ddk$meanvar
    if ( length( radius ) > 0 & length( colorData ) > 0 )
    {
      ddk$radius <- 100000 + radius / max( radius, na.rm = TRUE ) * 300000 # isa # 150000
      ddk$colorData <- colorData
      ddk$palcolorData <- pal( ddk$colorData )
      
      ddk <- na.omit( ddk[ c( 'lat', 'lon', 'radius', 'site', 'colorData', 'palcolorData' ) ] )
      leafletProxy( "map", data = ddk ) %>%
        clearShapes() %>%
        addCircles( ~lon, ~lat, radius = ~radius, layerId = ~site,
                    stroke = FALSE, fillOpacity = 0.4, fillColor = ~palcolorData ) %>%
        addLegend( "bottomleft", pal = pal, values = colorData, title = input$color,
                   layerId = "colorLegend" )
    } else
    {
      leafletProxy( "map", data = NULL ) %>%
        clearShapes()
      return()
    }
  })
  
  
  # click over a point in the map raises a call to showSitecodePopup
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showSitecodePopup( event$id, event$lat, event$lng )
    })
  })
  
  
  # popup info about the target location for the selected variable
  showSitecodePopup <- function( site, lat, lng ) {
    selectedSite <- dd[ dd$site == site & dd$variable.name == input$color, ]
    lonstr <- paste( sprintf( '%0.2f', abs( selectedSite$lon ) ), ifelse( selectedSite$lon < 0, 'W', 'E' ), sep = '' )
    latstr <- paste( sprintf( '%0.2f', abs( selectedSite$lat ) ), ifelse( selectedSite$lat < 0, 'S', 'N' ), sep = '' )
    content <- as.character(
      tagList(
        tags$h5( "Site:", selectedSite$site ),
        tags$body( HTML( sprintf( paste( input$color, ": %0.4f", sep = '' ), selectedSite$meanvar ) ) ), 
        tags$br(),
        tags$body( HTML( paste( lonstr, latstr, sep = ', ' ) ) ), 
        tags$br()
        ) 
      )
    leafletProxy( "map" ) %>% addPopups( lng, lat, content, layerId = site )
  }
  
  
  # go from table to a specific location -- not working right now
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      site <- input$goto$site
      lat <- input$goto$lat
      lng <- input$goto$lng
      showSitecodePopup(site, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
}


