### Interactive Map in Shiny Apps
### Created by: Wei Shen Lim
### Created on: 2026-11-11
# UI Layout and bslib dashboard structure adapted from:
# Silbiger, N. (2026). What's In Your Water? - Hawaiʻi Ocean Sampling Dashboard
# GitHub repository: https://github.com/njsilbiger/whats_in_your_water
###############################################################################

### Load Libraries
library(shiny)
library(tidyverse)
library(here)
library(bslib) #modern UI toolkit for Shiny
library(leaflet) #interactive map
library(fontawesome) #generate cool icons

### Load Functions
# Function to assign predefined marker colors supported by Leaflet awesomeIcons() to each taxon
assign_taxon_color <- function(data, taxon_column) {
  
  #The 19 predefined marker colors supported by Leaflet awesomeIcons()
  # Paired with their CCS hex codes for the legend
  available_colors <- c(
    red        = "#D63E2A",
    darkred    = "#A23336",
    lightred   = "#FF8E7F",
    orange     = "#F69730",
    beige      = "#FFCB92",
    green      = "#72B026",
    darkgreen  = "#728224",
    lightgreen = "#BBF970",
    blue       = "#38AADD",
    darkblue   = "#0067A3",
    lightblue  = "#8ADAFF",
    purple     = "#D252B9",
    darkpurple = "#5B396B",
    pink       = "#FF91EA",
    cadetblue  = "#436978",
    gray       = "#575757",
    lightgray  = "#A3A3A3",
    black      = "#303030" #Omitted "white" because marker icon will be white
  )
  
  #Extract the column specified by taxon_column from the dataset
  taxa <- unique(data[[taxon_column]]) #Get all unique taxa in that column
  taxa <- sort(taxa[!is.na(taxa)])     #Remove NA and sort remaining taxon names alphabetically
  
  #Assign a marker color to each unique taxon
  taxon_colors <- setNames(         #Uses taxon names as names for their corresponding colors
    rep(names(available_colors),    #Repeat the 18 available colors as many times as needed
        length.out = length(taxa)), #Sets the total number of colors to match the number of taxa
    taxa                            #Name each entry with its corresponding taxon
  )
  
  #Add new columns named marker_color and legend_color to the original dataset
  data <- data |> 
    mutate(
      marker_color = unname( #Removes the names from the resulting color vector so mutate() inserts only the color values
        taxon_colors[as.character(.data[[taxon_column]])]#Converts the taxon values to character strings so they match the names used in taxon_colors
      ),
      legend_color = unname(
        available_colors[marker_color] #hex codes for legend
      )
    )
  
  #Return the modified dataset, including the new marker_color column
  return(data)
}

# Function to build consistent summary value boxes with right-aligned title icons
make_summary_box <- function(title, icon_name, output_id, bg_color) {
  value_box(                          #Create a Bootstrap 5 value box card
    title = tags$div(                 #Custom flex container for the header row
      class = "d-flex justify-content-between align-items-center w-100", #Align title left, icon right
      tags$span(title),               #Display the box title text passed to the function
      fa(icon_name, height = "1.5em") #Render the specified FontAwesome icon at 1.5em height
    ),
    value  = textOutput(output_id),                          #Link the box's main value to its server textOutput ID
    theme  = value_box_theme(bg = bg_color, fg = "#ffffff"), #Apply custom background color and white text
    height = "75px"                                          #Enforce a compact 75-pixel card height
  )
}

# Function to italicize scientific names while keeping "cf." and "geppiorum4" upright
# Using <span> tags to avoid Leaflet's .legend i swatch CSS conflict
format_taxon <- function(x) {
  x |>
    str_replace_all(" cf\\. ", "</span> cf. <span style='font-style: italic;'>") |>             #Close italics before "cf." and reopen after
    str_replace_all('"geppiorum4"', '</span>"geppiorum4"<span style=\'font-style: italic;\'>') |> #Close italics before "geppiorum4" and reopen after
    (\(s) paste0("<span style='font-style: italic;'>", s, "</span>"))() |>                      #Wrap entire string in italic <span> tags
    str_remove_all("<span style='font-style: italic;'></span>")                                 #Remove any empty trailing <span> tag
}

# Function to build standardized HTML popups with clickable GenBank links
make_popup <- function(d) {
  paste0(
    "Taxon: ", format_taxon(d$Taxon),                                #Italicized species name (upright cf./geppiorum4)
    "<br>Authority: ", d$Authority,                                  #Taxonomic authority
    ifelse(is.na(d$Type), "", paste0("<br>Type: ", d$Type)),         #Include Type only if Holotype/Topotype
    "<br>Location: ", d$Location,                                    #Specific site/reef name and island
    "<br>Collection Date: ", d$Date,                                 #Collection date
    "<br>Collector: ", d$Collector,                                  #Collector name(s)
    ifelse(is.na(d$Depth), "", paste0("<br>Depth: ", d$Depth, "m")), #Include Depth only if recorded
    "<br>Sherwood Number: ", d$Sherwood,                             #Sherwood lab voucher ID
    ifelse(is.na(d$BISH), "", paste0("<br>BISH Number: ", d$BISH)),  #Include BISH number if deposited
    ifelse(is.na(d$Field), "", paste0("<br>Field Code: ", d$Field)), #Include field code if available
    ifelse(is.na(d$rbcL), "", paste0("<br>GenBank <i>rbc</i>L: ",
        "<a href='https://www.ncbi.nlm.nih.gov/nuccore/", d$rbcL, "' target='_blank'>", d$rbcL, "</a>")), #Clickable NCBI link for rbcL if sequenced
    ifelse(is.na(d$tufA), "", paste0("<br>GenBank <i>tuf</i>A: ",
        "<a href='https://www.ncbi.nlm.nih.gov/nuccore/", d$tufA, "' target='_blank'>", d$tufA, "</a>"))  #Clickable NCBI link for tufA if sequenced
    )
}

### Load Data
data <- read_csv("Data/Codium_collection_data.csv", show_col_types = FALSE) |> 
  mutate(
    Date = dmy(Date),                               #Parse dates into R Date objects
    Year = year(Date),                              #Extract collection year 
    Month = month(Date, label = TRUE, abbr = FALSE) #Extract full month name as an ordered factor
    ) |> 
  assign_taxon_color(taxon_column = "Taxon") #Append marker_color and legend_color column for Leaflet

### Pre-compute filter choices
taxon_choices  <- sort(unique(na.omit(data$Taxon)))       #Alphabetical list of unique species for dropdown
island_choices <- sort(unique(na.omit(data$Island)))      #Alphabetical list of unique islands/atolls for dropdown
year_choices   <- sort(unique(na.omit(data$Year)))        #Chronological list of unique collection years
month_choices  <- levels(droplevels(na.omit(data$Month))) #Calendar-ordered collection month names
min_depth      <- floor(min(data$Depth, na.rm = TRUE))    #Minimum collection depth rounded down for slider lower bound
max_depth      <- ceiling(max(data$Depth, na.rm = TRUE))  #Maximum collection depth rounded up for slider upper bound

### Citation Banner
citation_banner <- tags$div(   #Create a styled HTML container for the data citation
  style = "background: #f8f9fa; border-top: 1px solid #dee2e6; padding: 12px 18px; margin-top: 14px; border-radius: 8px;",
  tags$h5(                     #Heading inside the citation card
    class = "mb-2 fw-bold",    #Add bottom margin and bold font weight
    style = "color: #2E7D32;", #Match heading text to the app's primary green
    fa("quote-left", fill = "#2E7D32", height = "0.9em"), #Add green quotation icon
    " Cite These Data"         #Heading text
  ),
  div(                                                     #Inner callout box containing the formatted reference
    class = "alert alert-light border mb-0",               #Light Bootstrap alert box with border and no bottom margin
    style = "font-size: 0.85rem; font-family: monospace;", #Compact monospace typography for citation text
    "Sherwood, A.R., Allsopp, K.R., Alvarado, E.A., Kosaki, R.K., Paiano,",
    "M.O., Rentsch, P., Spalding, H.L., & Wade, R.M. (2025). ",
    tags$i("Systematics and taxonomy of Codium (Bryopsidales, Chlorophyta) in", #Italicize article title
           "the Hawaiian Islands: Description of six new species."),
    " Journal of Phycology. ",                                                  #Journal name
    tags$a(                    #Clickable hyperlink to the publication DOI
      href   = "https://doi.org/10.1111/jpy.70091",
      target = "_blank",       #Open link in a new browser tab
      class  = "text-muted",   #Muted link color
      "DOI: 10.1111/jpy.70091" #Visible link text
    )
  )
)

### UI
ui <- page_navbar( #Create a full-page Bootstrap 5 layout with a top banner
  
  # Top title banner with italicized genus name
  title = tags$span("Hawaiʻi ", tags$em("Codium"), " Diversity — Specimen & Collection Map"),
  
  # Set top banner background color to deep seaweed green
  navbar_options = navbar_options(bg = "#2E7D32"),
  
  # Configure Bootstrap 5 theme, primary accent color, and Google Fonts
  theme = bs_theme(
    version      = 5,                          #Use Bootstrap version 5
    bootswatch   = "flatly",                   #Apply the clean "flatly" Bootswatch theme
    primary      = "#2E7D32",                  #Set primary color for buttons, sliders, and active controls
    base_font    = font_google("Inter"),       #Use Inter for clean body and UI control typography
    heading_font = font_google("Merriweather") #Use Merriweather serif font for headings and banner title
  ),
  
  # Set CSS for banner size, font type, font size
  header = tags$head(
    tags$style(HTML("
      /* ---- Hide tab list since there's only have one page now ---- */
      .navbar-nav { display: none !important; }

      /* ---- Customize banner font, size, and padding ---- */
      .navbar-brand {
        font-family: 'Merriweather', Georgia, serif !important;
        font-size: 1.15rem !important;
        font-weight: 400 !important;
        padding-top: 4px !important;
        padding-bottom: 4px !important;
      }
      
      /* ---- Compact value box typography so all 5 fit cleanly in one row ---- */
      .bslib-value-box .card-body {
        padding: 0.45rem 1.75rem !important; /*Equal 0.45rem top/bottom padding and 1.75rem side padding*/
      }
      .value-box-title { font-size: 0.8rem !important; }
      .value-box-value { font-size: 0.95rem !important; margin-bottom: 0 !important; }
      
      /* ---- Auto-size Leaflet legend width to fit text without wrapping ---- */
      .leaflet-control.info.legend{
        width: max-content !important;
        max-height: 365px !important;
        white-space: nowrap !important; /* Keep color and taxon name on the same line */
        font-size: 0.78rem !important;
        line-height: 1.4 !important;
        padding: 8px 14px 8px 10px !important;
      }
    "))
  ),
  
  # Single navigation panel housing the main dashboard
  nav_panel(
    title = "Map", #Required panel title (hidden visually via CSS above)
    
    layout_sidebar(    #Create a two-column layout with a collapsible left sidebar
      fillable = TRUE, #Stretch layout to fill available viewport height
      
      # Left sidebar containing project context and interactive controls
      sidebar = sidebar(
        width = 310,    #Set sidebar width to 310 pixels
        open  = "open", #Keep sidebar expanded by default when the app loads
        
        # Introductory paragraphs describing the study and dashboard
        p(
          "The 2025 Sherwood et al. paper studied the systematics and taxonomy",
          "of ", strong(em("Codium")), " (Bryopsidales, Chlorophyta) in the Hawaiian",
          " Islands, with description of six new species, making it one of the",
          " most thoroughly collected and studied marine algal genera from Hawaiʻi."
        ),
        p(
          "Explore the diversity of ", strong(em("Codium")), " in Hawaiʻi from",
          " both shallow and mesophotic habitats across the Main Hawaiian Islands", 
          " and the Papahānaumokuākea National Marine Sanctuary (PNMS)."
        ),
        p(
          "Use the filters below to explore specimens collected in this study by",
          "taxon, islands and atolls, collection date, depth, and specimen type.",
          "Click any marker on the map for collection details."
        ),
        
        # Collapsible accordion grouping sidebar controls into three sections
        accordion(
          open = c("Map Display", "Filter Specimens", "Search & Export"), #Keep all three panels open on startup
          
          # Panel 1: Toggle map marker coloring mode
          accordion_panel(
            title = "Map Display",               #Panel header label
            icon  = fa("map", height = "0.9em"), #Map icon next to panel title
            radioButtons(
              inputId  = "color_by",        #Server input ID
              label    = "Color Points By", #Control label
              choices  = c(                 #Display label
                "Taxon (Default)"      = "taxon",
                "Collection Depth (m)" = "depth"
              ),
              selected = "taxon" #Default to coloring markers by species
            )
          ),
          
          # Panel 2: Multi-criteria specimen filters
          accordion_panel(
            title = "Filter Specimens",             #Panel header label
            icon  = fa("filter", height = "0.9em"), #Filter funnel icon
            
            # 1. Taxon multi-select dropdown 
            # Italicizes scientific names while keeping "cf." and "geppiorum4" upright
            selectizeInput(
              inputId  = "taxon_filter",    #Server input ID
              label    = "Species (Taxon)", #Dropdown label
              choices  = taxon_choices,     #Pre-computed alphabetical species list
              selected = NULL,              #Start empty (shows all species)
              multiple = TRUE,              #Allow selecting multiple species simultaneously
              options  = list(
                placeholder = "All species",         #Text displayed when no species are selected
                plugins     = list("remove_button"), #Add an "x" button to each selected tag
                render      = I("{
                /* ---- Format dropdown options and selected tags (italicizing scientific names) ---- */
                  option: function(item, escape) {
                    var label = escape(item.label)
                      .replace(' cf. ', '</i> cf. <i>')
                      .replace('&quot;geppiorum4&quot;', '</i>&quot;geppiorum4&quot;');
                    return '<div><i>' + label + '</i></div>';
                  },
                  item: function(item, escape) {
                    var label = escape(item.label)
                      .replace(' cf. ', '</i> cf. <i>')
                      .replace('&quot;geppiorum4&quot;', '</i>&quot;geppiorum4&quot;');
                    return '<div><i>' + label + '</i></div>';
                  }
                }")
              )
            ),
            
            # 2. Islands & Atolls multi-select dropdown
            selectizeInput(
              inputId  = "island_filter",    #Server input ID
              label    = "Islands & Atolls", #Dropdown label
              choices  = island_choices,     #Pre-computed alphabetical island/atoll list
              selected = NULL,               #Start empty (shows all islands)
              multiple = TRUE,               #Allow selecting multiple islands/atolls
              options  = list(
                placeholder = "All islands & atolls", #Text shown when empty
                plugins     = list("remove_button")   #Add remove "x" button on selected tags
              )
            ),
            
            # 3. Collection Month multi-select dropdown
            selectizeInput(
              inputId  = "month_filter",     #Server input ID
              label    = "Collection Month", #Dropdown label
              choices  = month_choices,      #Calendar-ordered months (Jan -> Dec)
              selected = NULL,               #Start empty (shows all islands)
              multiple = TRUE,               #Allow selecting multiple months
              options  = list(
                placeholder = "All months",         #Text shown when empty
                plugins     = list("remove_button") #Add remove "x" button on selected tags
              )
            ),
            
            # 4. Collection Year multi-select dropdown
            selectizeInput(
              inputId  = "year_filter",     #Server input ID
              label    = "Collection Year", #Dropdown label
              choices  = year_choices,      #Chronological years present in dataset
              selected = NULL,              #Start empty (shows all years)
              multiple = TRUE,              #Allow selecting multiple years
              options  = list(
                placeholder = "All years",          #Text shown when empty
                plugins     = list("remove_button") #Add remove "x" button on selected tags
              )
            ),
            
            # 5. Collection Depth range slider
            sliderInput(
              inputId = "depth_filter",          #Server input ID
              label   = "Collection Depth (m)",  #Slider label
              min     = min_depth,               #Minimum depth (0 m)
              max     = max_depth,               #Maximum depth (114 m)
              value   = c(min_depth, max_depth), #Default two-sided range spanning min to max
              step    = 1,                       #Increment slider by 1 meter
              post    = " m"                     #Append " m" unit label to slider numbers
            ),
            
            # 6. Type Specimen checkboxes (Holotype / Topotype)
            checkboxGroupInput(
              inputId  = "type_filter",             #Server input ID
              label    = "Type Specimens",          #Checkbox group label
              choices  = c("Holotype", "Topotype"), #Available type categories
              selected = NULL,                      #Start unchecked (shows all specimens)
              inline   = TRUE                       #Display checkboxes side-by-side horizontally
            ),
            
            # 7.  Button to clear all active filters and reset search
            actionButton(
              inputId = "reset_filters",                                                     #Server input ID
              label   = tagList(fa("rotate-left", height = "0.85em"), " Reset All Filters"), #Button label & icon
              class   = "btn-sm btn-outline-secondary w-100 mt-2"                            #Compact full-width outline
            )
          ),
          
          # Panel 3: Direct specimen ID lookup and CSV export
          accordion_panel(
            title = "Search & Export",                                             #Panel header label
            icon  = fa("magnifying-glass", height = "0.9em"),                      #Magnifying glass icon
            tags$label("Find Specimen by ID", class = "fw-semibold mb-1 d-block"), #Bold input label
            textInput(
              inputId     = "voucher_search",                 #Server input ID
              label       = NULL,                             #Omit default label (using custom label above)
              placeholder = "Sherwood, BISH, Field, GenBank", #Hint showing searchable ID columns
              width       = "100%"                            #Stretch input box to full sidebar width
            ),
            tags$div( #Horizontal flex container for Search and Clear buttons
              class = "d-flex gap-2 mb-2",
              actionButton(
                inputId = "search_btn",                  #Server input ID
                label   = "Search",                      #Button label
                class   = "btn-sm btn-primary flex-fill" #Green primary button filling available width
              ),
              actionButton(
                inputId = "clear_btn",                   #Server input ID
                label   = "Clear",                       #Button label
                class   = "btn-sm btn-outline-secondary" #Secondary outline button
              )
            ),
            uiOutput("search_result_msg"), #Dynamic feedback message (found vs. not found)
            downloadButton(
              outputId = "download_csv",                         #Server output ID
              label    = "Download Specimens (CSV)",             #Download button label
              class    = "btn-sm btn-outline-primary w-100 mt-2" #Full-width outline download button
            )
          )
        )
      ),
      
      # Five Summary Value Boxes arranged in a single 5-column row
      layout_column_wrap(
        width = 1/5,           #Force all 5 boxes into one equal-width row (20% each)
        fill  = FALSE,         #Prevent boxes from stretching vertically to fill the page
        heights_equal = "row", #Ensure all 5 boxes match the exact same height
        make_summary_box("Number of Specimens", "seedling", "n_specimens", "#1B5E20"), #Box 1: Deep kelp green specimen count
        make_summary_box("Number of Taxon", "sitemap", "n_taxon", "#388E3C"), #Box 2: Medium Codium green taxon count
        make_summary_box("Islands & Atolls", "map-location-dot", "n_islands", "#00838F"), #Box 3: Shallow reef teal island count
        make_summary_box("Collection Date", "calendar-days", "collection_date", "#0277BD"), #Box 4: Pelagic blue collection date range
        make_summary_box("Depth Range", "anchor", "depth_range", "#01579B") #Box 5: Deep mesophotic navy depth range
      ),
      
      # Interactive Leaflet Map Card
      card(
        id = "map-card",    #ID for targeting map card styles if needed
        full_screen = TRUE, #Add expand-to-fullscreen toggle icon in bottom-right corner
        card_header("Hawaiian ", tags$em("Codium"), " Specimen Collection Locations"), #Card title with italicized genus
        div(
          style = "position: relative;",          #Relative wrapper so empty-state overlay sits directly on top of map
          leafletOutput("map", height = "520px"), #Render Leaflet map widget at 520px height
          uiOutput("empty_map_overlay")           #Conditional overlay message when filters return 0 specimens
        )
      ),
      
      # Citation Banner
      citation_banner, #Render the pre-built citation container defined in the beginning
      #Did it this way for final project, will have a second navigational panel with this citation banner too
      
      # Code Acknowledgement
      tags$div(
        class = "text-end mt-n2",     #Right-align text using Bootstrap utility class
        style = "margin-top: -20px;", #Pull acknowledgment closer to the citation banner above it
        tags$small( #Render small muted footer text
          class = "text-muted",
          "Dashboard layout and code adapted from ",
          tags$a( #Hyperlink to Dr. Nyssa Silbiger's GitHub repository
            href   = "https://github.com/njsilbiger/whats_in_your_water", #Full repository URL
            target = "_blank",                                            #Open repository in a new browser tab
            class  = "text-muted",
            tags$i("What\u2019s In Your Water? \u2014 Hawai\u02BBi Ocean Sampling") #Italicized dashboard title
          ),
          " by Dr. Nyssa Silbiger (2026)."
        )
      )
    )
  )
)

### Server
server <- function(input, output, session) {
  
  # Search and Export
  # 1. Multi-Column Specimen ID Search
  voucher_query <- reactiveVal("") #Reactive value storing the active uppercase search string
  
  observeEvent(input$search_btn, {                       #Triggered when the user clicks the "Search" button
    voucher_query(toupper(trimws(input$voucher_search))) #Trim whitespace, convert to uppercase, and store query
  })
  
  observeEvent(input$clear_btn, {                          #Triggered when the user clicks the "Clear" button
    voucher_query("")                                      #Reset the stored search query to an empty string
    updateTextInput(session, "voucher_search", value = "") #Clear the visible text inside the search input box
  })
  
  # Reactive subset matching the search query across all 5 ID columns
  search_matches <- reactive({
    q <- voucher_query()         #Retrieve current search query
    if (!nzchar(q)) return(NULL) #Return NULL if the search query is empty
    
    data |>
      filter(                                  #Match case-insensitively against any of the 5 ID fields
        toupper(coalesce(Sherwood, "")) == q | #Match Sherwood accession number (coalesce replaces NA with "")
        toupper(coalesce(BISH, ""))     == q | #Match Bishop Museum herbarium accession number
        toupper(coalesce(Field, ""))    == q | #Match original field collection code
        toupper(coalesce(rbcL, ""))     == q | #Match GenBank rbcL accession number
        toupper(coalesce(tufA, ""))     == q   #Match GenBank tufA accession number
      )
  })
  
  # Render feedback message below the Search/Clear buttons
  output$search_result_msg <- renderUI({
    q <- voucher_query()         #Retrieve current search query
    if (!nzchar(q)) return(NULL) #Display nothing when no search is active
    matches <- search_matches()  #Retrieve matching rows
    
    if (nrow(matches) == 0) { #Show red error text if no specimens matched
      tags$small(class = "text-danger d-block mb-1", paste0("No specimen found for \u201c", q, "\u201d."))
    } else {                  #Show green confirmation with matching Sherwood ID(s)
      tags$small(
        class = "text-success d-block mb-1",
        fa("circle-check", height = "0.85em"),
        paste0(" Found ", nrow(matches), " specimen(s): ", paste(matches$Sherwood, collapse = ", "))
      )
    }
  })
  
  # 2. Filtered Dataset (Combines ID Search + Sidebar Filters)
  df_filtered <- reactive({
    q <- voucher_query()                   #Check if an ID search is currently active
    
    # If a specific ID search is active, bypass sidebar filters and isolate matching specimen(s)
    if (nzchar(q)) {
      return(search_matches())
    }
    
    df <- data                             #Start with the full specimen dataset
    
    # Filter 1: Species (Taxon)
    if (length(input$taxon_filter) > 0) {  #Only filter if one or more species are selected
      df <- df |> filter(Taxon %in% input$taxon_filter)
    }
    
    # Filter 2: Islands & Atolls
    if (length(input$island_filter) > 0) { #Only filter if one or more islands/atolls are selected
      df <- df |> filter(Island %in% input$island_filter)
    }
    
    # Filter 3: Collection Month
    if (length(input$month_filter) > 0) {  #Only filter if one or more months are selected
      df <- df |> filter(as.character(Month) %in% input$month_filter)
    }
    
    # Filter 4: Collection Year
    if (length(input$year_filter) > 0) {   #Only filter if one or more years are selected
      df <- df |> filter(Year %in% as.numeric(input$year_filter))
    }
    
    # Filter 5: Collection Depth Slider
    # Only filter out rows outside the slider range if user actually moved the slider;
    # this keeps the 5 specimens with Depth == NA visible until a depth range is actively chosen
    if (!is.null(input$depth_filter) &&
        (input$depth_filter[1] > min_depth || input$depth_filter[2] < max_depth)) {
      df <- df |> filter(
        !is.na(Depth),                     #Exclude specimens missing depth measurements
        Depth >= input$depth_filter[1],    #Keep depths >= lower slider handle
        Depth <= input$depth_filter[2]     #Keep depths <= upper slider handle
      )
    }
    
    # Filter 6: Type Specimens (Holotype / Topotype)
    if (length(input$type_filter) > 0) {   #Only filter if Holotype and/or Topotype is checked
      df <- df |> filter(Type %in% input$type_filter)
    }
    
    df                                     #Return the final filtered dataframe
  })
  
  # Reset All Filters & Search when "Reset All Filters" button is clicked
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "taxon_filter",  selected = character(0))         #Clear species dropdown
    updateSelectizeInput(session, "island_filter", selected = character(0))         #Clear islands dropdown
    updateSelectizeInput(session, "month_filter",  selected = character(0))         #Clear months dropdown
    updateSelectizeInput(session, "year_filter",   selected = character(0))         #Clear years dropdown
    updateSliderInput(   session, "depth_filter",  value = c(min_depth, max_depth)) #Reset slider to full range
    updateCheckboxGroupInput(session, "type_filter", selected = character(0))       #Uncheck Holotype/Topotype boxes
    
    # Also clear any active ID search
    voucher_query("")                                      #Reset reactive search query
    updateTextInput(session, "voucher_search", value = "") #Clear search text input box
  })
  
  # 3. CSV Download Handler
  output$download_csv <- downloadHandler(
    filename = function() {                #Generate dynamic filename with today's date
      paste0("Hawaiian_Codium_Specimens_", Sys.Date(), ".csv")
    },
    content = function(file) {             #Write the currently filtered rows to CSV
      df_filtered() |>
        select(-any_of(c("marker_color", "Legend_color"))) |> #Drop internal map and legend color column before exporting
        readr::write_csv(file)             #Save CSV preserving Hawaiian diacritics
    }
  )
  
  # Five Summary Value Boxes
  # 1. Number of specimens ("X of 118")
  output$n_specimens <- renderText({
    paste(nrow(df_filtered()), "of", nrow(data))               #Compare filtered row count to total dataset rows
  })
  
  # 2. Number of taxon ("X of 18")
  output$n_taxon <- renderText({
    paste(n_distinct(df_filtered()$Taxon, na.rm = TRUE),       #Count unique taxa in filtered data
          "of",
          n_distinct(data$Taxon, na.rm = TRUE))                #Compare against total unique taxa in dataset
  })
  
  # 3. Number of islands and atolls ("X of 16")
  output$n_islands <- renderText({
    paste(n_distinct(df_filtered()$Island, na.rm = TRUE),      #Count unique islands/atolls in filtered data
          "of",
          n_distinct(data$Island, na.rm = TRUE))               #Compare against total unique islands/atolls
  })
  
  # 4. Collection date (Month and Year only)
  output$collection_date <- renderText({
    dates <- df_filtered()$Date[!is.na(df_filtered()$Date)]    #Extract non-NA dates from filtered data
    if (length(dates) == 0) return ("-")                       #Display "-" if no valid dates remain
    min_d <- format(min(dates), "%b %Y")                       #Format earliest date as "Mon YYYY"
    max_d <- format(max(dates), "%b %Y")                       #Format latest date as "Mon YYYY"
    if (min_d == max_d) min_d else paste(min_d, "-", max_d)    #Show single date if min == max, else show range
  })
  
  # 5. Depth range (in meters)
  output$depth_range <- renderText({
    depths <- df_filtered()$Depth[!is.na(df_filtered()$Depth)] #Extract non-NA depths from filtered data
    if (length(depths) == 0) return("-")                       #Display "-" if no depth data remain
    if (min(depths) == max(depths)) {                          #Show single depth if min == max
      paste0(min(depths), "m")
    } else {                                                   #Otherwise show "min-max m" range
      paste0(min(depths), "-", max(depths), "m")
    }
  })
  
  # Empty Map Overlay (shown when filters return 0 rows)
  output$empty_map_overlay <- renderUI({
    req(nrow(df_filtered()) == 0) #Only render overlay when filtered dataframe is empty
    div(                          #Semi-transparent white backdrop covering the map
      style = paste(
        "position: absolute; top: 0; left: 0; right: 0; bottom: 0;",
        "display: flex; flex-direction: column;",
        "align-items: center; justify-content: center;",
        "background: rgba(255,255,255,0.82); z-index: 500; pointer-events: none;"
      ),
      tags$div(                   #Centered message box inside overlay
        style = "text-align: center; padding: 24px;",
        fa("map-location-dot", height = "2.5em", fill = "#adb5bd"),                  #Muted grey map icon
        tags$h5("No specimens match these filters", class = "mt-3 mb-1 text-muted"), #Main empty-state heading
        tags$p(                   #Helpful instruction to reset filters
          class = "text-muted small mb-0",
          "Try adjusting your filters or click ",
          tags$strong("Reset All Filters"),
          " to start over."
        )
      )
    )
  })
  
  # 1. Base Leaflet Map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.25)) |> #Allow fractional zoom increments (0.25 steps)
      addProviderTiles(providers$Esri.WorldImagery) |>    #Add Esri satellite imagery base layer
      setView(lng = -166.7, lat = 24.0, zoom = 5.25) |>   #Center view over full Hawaiian Archipelago & PMNM
      addEasyButton(                                      #Button 1: reset zoom & pan to default view
        easyButton(
          icon = "ion-earth",                             #Globe icon from Ionicons
          title = "Reset Map View",                       #Hover tooltip text
          onClick = JS("function(btn, map){ map.setView([24.0, -166.7], 5.25); }") #Reset to [lat, lng], zoom
        )
      ) |> 
      addEasyButton(                                      #Button 2: toggle legend visibility on/off
        easyButton(
          icon = "fa-list",                               #List icon from FontAwesome
          title = "Show / Hide Legend",                   #Hover tooltip text
          onClick = JS("function(btn, map){
                       document.querySelectorAll('.info.legend').forEach(el => el.classList.toggle('d-none'));
                       }")
        )
      )
  })
  
  # 2. Reactive Marker Updates via LeafletProxy()
  observe({
    df <- df_filtered()             #Re-run whenever filtered dataset or color mode changes
    
    proxy <- leafletProxy("map") |> #Target existing map instance without redrawing tiles
      clearMarkers() |>             #Remove previous circle or awesome markers
      clearMarkerClusters() |>      #Remove previous marker cluster groups
      clearControls()               #Remove depth legend when switching back to Taxon mode
    
    if (nrow(df) == 0) return()     #Exit early if no specimens match current filters
    
    if (input$color_by == "depth") {
      # Filter to rows that have depth measurements
      df_depth <- df |> filter(!is.na(Depth)) #Keep only specimens with recorded depth values
      if (nrow(df_depth) == 0) return()       #Exit early if none of the filtered rows have depth
      
      # Calculate 7 evenly spaced tick marks across the full dataset's depth range (0 to 114 m)
      depth_breaks <- round(seq(min(data$Depth, na.rm = TRUE),
                                max(data$Depth, na.rm = TRUE),
                                length.out = 7))
      
      # Reverse viridis so shallow = bright yellow-green, deep = dark purple-blue
      pal <- colorNumeric(
        palette = "viridis",
        domain = range(data$Depth, na.rm = TRUE),
        reverse = TRUE
      )
      
      # Draw depth-colored circle markers and add continuous color legend
      proxy |> 
        addCircleMarkers(
          data = df_depth,
          lng = ~Long,
          lat = ~Lat,
          radius = 7,                  #Marker circle radius in pixels
          color = "white",             #White outer border for contrast against satellite imagery
          fillColor = ~pal(Depth),     #Map depth value to viridis color
          fillOpacity = 0.85,          #Slight transparency for overlapping points
          weight = 1.5,                #Border stroke thickness
          label = ~lapply(paste0(format_taxon(Taxon), " (", Depth, " m)"), htmltools::HTML), #Hover label
          popup = make_popup(df_depth) #Standardized popup with clickable GenBank links
        ) |> 
        addLegend(
          position = "topright",        #Place legend in upper-right corner of map
          pal = pal,                    #Color palette function
          values = na.omit(data$Depth), #Full depth distribution for legend gradient
          bins = depth_breaks,          #Custom 7-step tick marks reaching 114 m
          title = "Depth (m)",          #Legend header
          opacity = 0.9                 #Legend background opacity
        )
    } else {
      # Default: custom 18-taxon Ionicons Leaf Markers
      filtered_icons <- awesomeIcons(
        icon        = "ion-leaf",     #Leaf-shaped icon from Ionicons library
        library     = "ion",          #Specify Ionicons icon set
        iconColor   = "white",        #White leaf symbol inside pin
        markerColor = df$marker_color #Pre-assigned species color from assign_taxon_color()
      )
      
      # Build sorted legend table for the taxa currently visible on the map
      legend_df <- df |> 
        distinct(Taxon, legend_color) |> #Get unique visible taxa and their assigned legend color
        filter(!is.na(Taxon)) |>         #Exclude any NA taxa
        arrange(Taxon)                   #Sort alphabetically
      
      proxy |>
        addAwesomeMarkers(
          data  = df,
          lng   = ~Long,
          lat   = ~Lat,
          icon  = filtered_icons,
          label = ~lapply(format_taxon(Taxon), htmltools::HTML), #Italicized scientific name on hover
          popup = make_popup(df),                                #Full specimen details on click
          clusterOptions = markerClusterOptions( #Cluster nearby pins to prevent overlap at low zoom
            showCoverageOnHover     = TRUE,      #Highlight cluster polygon boundary on hover
            maxClusterRadius        = 25,        #Cluster pins within 25 pixels of each other
            disableClusteringAtZoom = 15         #Uncluster into individual pins at zoom level 15+
          )
        ) |> 
        addLegend(
          position = "topright",           #Place taxon legend in upper-right corner
          colors = legend_df$legend_color, #Legend colors matching the pins
          labels = lapply(format_taxon(legend_df$Taxon), htmltools::HTML), #Italicized species names
          title = "Taxon",                 #Legend header
          opacity = 0.9                    #Legend background opacity
        )
    }
  })
}

### Launch Shiny App!
shinyApp(ui, server)
