# Scottish Burden of Diseases Accelerator programme
# Author: Caterina Constantinescu, Data Scientist @ The Data Lab, Scotland
# Program participant: Maite Thrower – Senior Analyst from Public Health Intelligence – ISD (NHS National Services Scotland)
# Example app code used for mentorship in the Accelerator programme run with members of ScotGov, between 19 April 2018 - 6 September 2018.
# Please acknowledge this source as:
# Constantinescu, A.C. (2018, August). Exploring the Scottish Burden of Diseases Data using R Shiny [R script]. Edinburgh, Scotland: The Data Lab Innovation Centre. Retrieved [Month] [Day], [Year], from https://github.com/TheDataLabScotland/Public_ScotGovAccelerator_2018/blob/master/BurdenOfDiseasesShiny/app.R




library( shiny )
library( shinyjs )
library( shinythemes )

library( data.table )
library( stringr ) # To automatically wrap text longer than n chars.

library( ggplot2 )

library( jsonlite )

library( googleVis )

setwd( "/home/caterina/Documents/ScotGovAccelerator/" )

# We are using a custom version of the googleVis R package, because we want to customise the tooltips appearing over each rectangle in the treemap:
# See how/why here: https://stackoverflow.com/questions/32150206/customization-of-a-tooltip-in-a-treemap-using-r-and-googlevis/51866493#51866493
source( "./gvis.R" )
source( "./gvisTreeMap.R" )




# TODOs ---------------------------------------------------------------------------------------

# TODO
# Leave any TODOs here, e.g., add functionality for downloading the data.
# Update data used etc.




shinyApp(
  
  
  # UI ------------------------------------------------------------------------------------------
  
  
  ui = 
    tagList(
      useShinyjs( debug = TRUE ), # shinyjs is loaded here to allow menu enabling/disabling later
      
      navbarPage(
        
        theme = shinytheme( "flatly" ), # or simplex
        
        "Scottish Burden of Diseases", # title
        
        
        tabPanel( "Visualisations",
                  
                  tags$head( tags$style( HTML( 
                    "hr {border-top: 1px solid #000080;}" ) ) # Setting the style for horizontal line appearing over 'Legend' below
                  ),
                  
                  sidebarPanel(
                    
                    width = 3, # Total allowed in Shiny is 12. So this means the sidebar panel will be 1/4 of full screen width
                    
                    # Make this menu disabled by default, on startup. The re-enable if user navigates to another tab where choosing L2 becomes relevant:
                    disabled( 
                      # No 'Any' option required here, because the treemap below takes care of showing all the data / L2 categories:
                      selectInput( inputId = "Level2",
                                   label = "Disease Level II:",
                                   sort( c( "HIV/AIDS and tuberculosis", "Diarrhoea lower respiratory,and other common infectious diseases",
                                            "Neglected tropical diseases and malaria", "Maternal disorders",
                                            "Neonatal disorders", "Nutritional deficiencies",
                                            "Other communicable and nutritional diseases", "Neoplasms",
                                            "Cardiovascular diseases", "Chronic respiratory diseases",
                                            "Cirrhosis", "Digestive diseases",
                                            "Neurological disorders", "Mental and substance use disorders",
                                            "Diabetes, urogenital, blood, and endocrine diseases", "Musculoskeletal disorders",
                                            "Other non-communicable diseases", "Transport injuries",
                                            "Unintentional injuries",
                                            " Forces of nature, war, and legal intervention", # [sic!]
                                            "Unknown cause of injury" ) ) 
                      )
                    ),
                    
                    
                    
                    uiOutput( "renderedLevel3UI" ),
                    
                    br(), br(),
                    
                    radioButtons( inputId = "Measure",
                                  label = "Measure:",
                                  c( "Disability-Adjusted Life Years (DALY)" = "DALY",
                                     "Years Lost of Life (YLL)" = "YLL",
                                     "Years Lived with Disability (YLD)" = "YLD" )
                    ),
                    
                    
                    br(), br(),
                    
                    radioButtons( inputId = "isUsingSIMD",
                                  label = "Subset by SIMD scores?",
                                  c( "No",
                                     "Yes" ),
                                  selected = "No"
                    ),
                    
                    conditionalPanel(
                      # Notice this is JavaScript code here, enclosed within double quotes, and using a . for indexing, instead of $:
                      condition = "input.isUsingSIMD == 'Yes'",
                      
                      numericInput( inputId = "simdSlider",
                                    label = "Deprivation score:",
                                    min = 1,
                                    max = 10,
                                    value = 1,
                                    step = 1
                      )
                    ), 
                    
                    
                    br(),
                    
                    # Adding the horizontal bar styled above, plus some legend text.
                    HTML( "<hr>" ),
                    
                    HTML( "<u>Legend</u><br/>" ),
                    HTML( "<b>Level I</b>: All diseases across Scotland (not applicable here)<br/>"),
                    HTML( "<b>Level II</b>: Major disease categories<br/>"),
                    HTML( "<b>Level III</b>: Disease subcategories<br/>"),
                    br(),
                    
                    HTML( "<i>NB: Some graphs may only be suitable for viewing Level II data, in which case the Level III menu will be inactive. In the case of the treemap, all Level II and Level III data is used, hence both menus are inactive.</i>" )
                    
                  ),
                  
                  
                  mainPanel(
                    
                    width = 9, # Because the sidebar is 3, and the total must be exactly 12.
                    
                    tabsetPanel(
                      
                      id = "tabs", 
                      
                      tabPanel( "Tree map",
                                htmlOutput( "treemap" ) # Dimensions for treemap are set within the treemap function itself.
                                
                      ),
                      
                      tabPanel( "Heatmap / balloon plot",
                                plotOutput( "heatmap", width = "90%", height = "550px" ),
                                br(),
                                HTML( "<i>NB. In some cases, a <strong>Level II</strong> category will have very few <b>Level III</b> subcategories. In such cases, a balloon plot will be printed instead of a heatmap.</i>" )
                      ),
                      
                      tabPanel( "Line graph",
                                plotOutput( "linegraph", width = "90%", height = "550px" )
                      )
                    )
                  )
        ),
        tabPanel( "Data tables", "This panel is intentionally left blank" ),
        tabPanel( "Data export", "This panel is intentionally left blank" )
      )
    ),
  
  
  
  
  
  
  
  # Server --------------------------------------------------------------------------------------
  
  
  
  server = function( input, output ) {
    
    # Reading in tidy version of the data (that has been prepped beforehand)
    BoDS <<- fread( "./BoDS_tidy.csv" ) # Assigning data to global environment, visible to all layers
    BoDS[ , simd := ordered( as.numeric( simd ) ) ]
    BoDS[ , age := ordered( age, levels =  unique( BoDS$age ) ) ]
    
    
    # Filling in L3 menu based on actual data that was read in:
    output$renderedLevel3UI <- renderUI({
      disabled(
        selectInput( "Level3RenderedAfterTheFact", "Disease Level III, if applicable:", 
                     choices = sort( unique( BoDS[ level2 == input$Level2, level3 ] ) ) ) 
      )
    })
    
    
    
    
    # Make sure only the relevant menus are enabled, depeding on what tab is currently active:
    observeEvent( input$tabs, {
      
      if ( input$tabs == "Tree map" ) {
        
        disable( "Level2" )
        disable( "Level3RenderedAfterTheFact" )
        enable( "isUsingSIMD" )
        enable( "simdSlider" )
        
      }
      else if ( input$tabs == "Heatmap / balloon plot" ) {
        
        enable( "Level2" )
        disable( "Level3RenderedAfterTheFact" )
        disable( "isUsingSIMD" )
        disable( "simdSlider" )
        
      }
      else if ( input$tabs == "Line graph" ) {
        
        enable( "Level2" )
        enable( "Level3RenderedAfterTheFact" )
        disable( "isUsingSIMD" )
        disable( "simdSlider" )
        
      }
      else {
        stop ( "Malformed tab switch ?" )
      }
      
    })
    
    
    
    
    
    
    
    
    
    # Treemap -------------------------------------------------------------------------------------
    
    
    
    output$treemap <- renderGvis({
      
      # For the treemap to work, we must comply with a coupls of special conditions:
      # 1. The dataset itself needs to reflect a hierarchical parent-child structure: the L3s are nested into L2s by default, but then we must add extra rows of data to nest the L2 categories within an L1, overarching category ("All diseases"). Then, even L1 needs to be nested within an 'NA' parent - as a way to make it explicit that the L1 is the top-most category, with no further parent above it.
      # 2. No situations are allowed where the L2 category has an identical name to an L3 category - so this is addressed below.
      
      
      if ( input$isUsingSIMD == "Yes" ) {
        
        
        if ( input$simdSlider %in% 1:10 ) {
          LevelIII_Tier <- setDT( aggregate( Value ~ level2 + level3, 
                                             FUN = mean, 
                                             data = BoDS[ Type == input$Measure & simd == input$simdSlider, ] ) )
        }
        else {
          stop( "Please choose an integer between 1 and 10 as the Deprivation score.")
        }
        
        
      } 
      else {
        
        LevelIII_Tier <- setDT( aggregate( Value ~ level2 + level3, 
                                           FUN = mean, 
                                           data = BoDS[ Type == input$Measure, ] ) )
        
      }
      
      setnames( LevelIII_Tier, c( "Parent", "Child", "Value" ) )
      
      # Parent categories with a single child, named the same as the parent, are not allowed in googlevis treemaps. 
      # Hence need to distinguish them:
      LevelIII_Tier[ Parent == Child, Child := paste( "Single subcategory:", Child ) ]

      LevelII_Tier <- data.table( Parent = "All diseases", 
                                  Child = unique( LevelIII_Tier$Parent )
      )
      LevelII_Tier_Value <- aggregate( Value ~ Parent, FUN = mean, data = LevelIII_Tier )
      LevelII_Tier <- merge( LevelII_Tier, LevelII_Tier_Value, by.x = "Child", by.y = "Parent" )
      
      
      LevelI_Tier <- data.table( Parent = NA, 
                                 Child = "All diseases",
                                 Value = mean( LevelII_Tier$Value ) )
      
      
      nested_diseases <- rbind( LevelI_Tier, LevelII_Tier, LevelIII_Tier )
      
      
      
      
      gvisTreeMap( nested_diseases, idvar = "Child", parentvar = "Parent",
                   sizevar = "Value", colorvar = "Value",
                   options = list(
                     # It's important to set the scale endpoints to values 
                     # which are relevant for each measure separately (DALY vs. YLL vs. YLD)
                     minColorValue = min( LevelIII_Tier$Value ), 
                     maxColorValue = max( LevelIII_Tier$Value ),
                     minColor = 'green',
                     midColor = '#fff',
                     maxColor = 'red',
                     showScale = TRUE,
                     maxDepth = 2,
                     # headerColor = "white",
                     # Relative sizes (recommended for width only) or absolute size in pixels (height only)
                     width = "100%", height = 800
                     
                   )
                   
      )
      
      
      
    })
    
    
    
    
    
    
    # Heatmap -------------------------------------------------------------------------------------
    
    
    
    output$heatmap <- renderPlot( {
 
      level2_aggregated <- setDT( aggregate( Value ~ level3 + simd, 
                                             FUN = mean, 
                                             data = BoDS[ level2 == input$Level2 & Type == input$Measure, ] ) )
      
      # Relationship between burden of disease and SIMD
      # CONVERT TO Z SCORES BECAUSE THE RANGE OF SOME DALYS TOTALLY DWARFS OTHERS, e.g., Bladder level2_subset vs. Lung level2_subset.
      level2_aggregated$Value <- ave( level2_aggregated$Value, level2_aggregated$level3, FUN = scale )
      
      
      isolate({
        # Heatmaps don't look good when there are fewer than 5 categories... Hence:
        if ( length( unique( BoDS[ level2 == input$Level2, level3 ] ) ) < 5 ) {
          
          # Save space when labels are too long. This transform is here because it looks ugly with heatmap, but great with balloon plot.
          level2_aggregated[ , level3 := str_wrap( level3, width = 20 ) ]
          
          ggplot( level2_aggregated, 
                  aes( x = simd, y = level3, size = Value, fill = Value ), color = "black" ) + 
            geom_point( shape = 21 ) +
            guides( size = FALSE ) +
            scale_size( range = c( 1, 25 ) ) +
            scale_fill_continuous( high = "red", low = "yellow" ) +
            theme_bw(  ) +
            theme( text = element_text( size = 15.5 ),
                   axis.line = element_blank(),            # disable axis lines
                   panel.border = element_blank(),         # disable panel border
                   panel.grid.major.x = element_blank(),   # disable lines in grid on X-axis
                   panel.grid.minor.x = element_blank() ) +
            labs( y = paste( "Standardised ", input$Measure, sep = "" ), 
                  x = "\nSIMD / Deprivation scores" ) +
            ggtitle( paste( "Standardised ", input$Measure, " values for ",
                            input$Level2, ",\n according to SIMD scores", sep = "" ) )
          
        }
        else {
          
          ggplot( level2_aggregated, aes( x = simd,
                                          # y = reorder( level3, Value, median, order = TRUE ), # This would sort the heatmap
                                          y = level3, 
                                          fill = Value ) ) +
            geom_tile( color = "transparent") + 
            #  "black" possible too - this colours the grid which separates the squares
            # , width = 0.99, height = 0.99 to create some spacing in between the tiles.
            scale_fill_gradient( low = "yellow", high = "red",
                                 space = "Lab", na.value = "transparent",
                                 guide = "colourbar" ) +
            labs( fill = paste( "Standardised\n", input$Measure, sep = "" ), 
                  x = "\nSIMD / Deprivation scores",
                  y = isolate( paste( "Subcategories of ", input$Level2, "\n", sep = "" ) ) ) +
            ggtitle( isolate( paste( "Standardised ", input$Measure, " values for ", input$Level2, 
                                     ",\n according to SIMD scores", sep = "" ) ) ) +
            theme( axis.text.x = element_text( angle = 0, hjust = 0.5 ),
                   text = element_text( size = 15.5 ) )
        }
        
      })
      
      
      
      
    })
    
    
    
    
    
    # Line graph ----------------------------------------------------------------------------------
    
    
    output$linegraph <- renderPlot({
      
      ggplot( data = BoDS[ level3 == input$Level3RenderedAfterTheFact & Type == input$Measure, ],
              aes( x = simd, y = Value, color = age, group = age ) ) +
        geom_line( size = 1.5 ) +
        facet_wrap( ~ sex ) +
        labs( color = "Age",
              x = "\nSIMD / Deprivation scores",
              y = isolate( paste( input$Measure, " values for ", input$Level2, "\n", sep = "" ) ) ) +
        ggtitle( isolate( paste( "\n", input$Level2, " (subtype: ", input$Level3RenderedAfterTheFact,
                                 "),\naccording to SIMD scores and age group\n",
                                 sep = "" ) ) ) +
        theme( axis.text.x = element_text( angle = 0, hjust = 0.5 ),
               text = element_text( size = 15.5 ) )
      
      
    })
    
    
    
    
  })






