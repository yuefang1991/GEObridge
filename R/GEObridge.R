


#' This function has two goals. First, prepare the database locally for downstream searching.
#' Second, print the detailed columns in subTable (e.g. gse).
#'
#' @param destdir The destination directory of the downloaded file.
#' @param destfile The filename of the downloaded file. This filename should end in ".gz" as the unzipping assumes that is the case.
#' @param type Type of GEOmetadb.sqlite to download, if it is 'normal', a full database will be downloaded, otherwise a demo database will be downloaded, which is 25MB..
#' @return The local filename for use later..
update_getSQLiteFile <- function(destdir=getwd(),destfile='GEOmetadb.sqlite.gz',type='full') {

    localfile <- file.path(destdir,destfile)
    if(type == 'full') {
      url_geo = "https://gbnci.cancer.gov/geo/GEOmetadb.sqlite.gz"
    } else {
      url_geo = "https://gbnci.cancer.gov/geo/GEOmetadb_demo.sqlite.gz"
    }

    download.file(url_geo, destfile=localfile,mode='wb')
    cat('Unzipping...\n')
    gunzip(localfile,overwrite=TRUE)
    unzippedlocalfile <- gsub('[.]gz$','',localfile)
    con <- dbConnect(SQLite(),unzippedlocalfile)
    dat <- dbGetQuery(con,'select * from metaInfo')
    dbDisconnect(con)
    cat("Metadata associate with downloaded file:\n")
    print(dat)
    return(unzippedlocalfile)
}



#' This function has two goals. First, prepare the database locally for downstream searching.
#' Second, print the detailed columns in subTable (e.g. gse).
#'
#' @param printAllTable logical variable. Print all table if TRUE, do not print all table if FALSE.
#' @param subTable Character variable. It is the sub-table name which stores different types of metadata. It should be one of followings. ('gds', 'gds_subset', 'geoConvert','geodb_column_desc','gpl','gse','gse_gpl','gse_gsm','gsm','metaInfo','sMatrix')
#' @return The connection variable which contains all metadata.
prepare_table <- function(printAllTable=TRUE, subTable='gse'){

  # use updated version update_getSQLiteFile func to download new version database
  if(!file.exists("GEOmetadb.sqlite")){
    update_getSQLiteFile(destdir = getwd(), destfile = "GEOmetadb.sqlite.gz", type = 'demo')
  }

  # build connection
  con = dbConnect(SQLite(), "GEOmetadb.sqlite")
  TotalTable <- dbListTables(con)
  if(printAllTable == T){
    print('-----ALl tables stored in the database-----')
    print(TotalTable)
    #print('------------')
  }

  # print your target table
  print('------Print columns for your inputted sub table-----')
  subT = dbListFields(con, subTable)
  print(subT)
  #print('------------')

  return(con)
}



#' The main function of mining the latent relationship among variables.
#'
#' @param key1 The first keyword to search. For example we want to search "breast cancer", we can set key1 is the first word, breast.
#' @param key2 The second keyword to search. For example we want to search "breast cancer", we can set key2 is the second word, cancer
#' @param limitN An integer variable to choose largest number query from the database.
#' @param verbose A logical variable. It will print some disgnostic information if TRUE.
#' @param db The connection variable which contains all metadata.
#' @param subTable Character variable. It is the sub-table name which stores different types of metadata.
#' @return The returned result is GSE ID of the dataset which satisfy the search rules.
keyword_search <- function(key1='breast', key2='cancer', limitN=2000, verbose=TRUE, db=connection, subTable='gse'){
  search_statement = paste('select * from', subTable, 'limit', limitN, sep = ' ')
  gse_database0 = dbGetQuery(db, search_statement)
  # gse_database = gse_database0[,c('title', 'gse', 'summary', 'type', 'overall_design')]
  gse_database = gse_database0
  if(verbose==T) {
    print('Total dataset size:')
    print(dim(gse_database))
  }

  counts = apply(gse_database , MARGIN=1, function(x)(grep(key1,tolower(x))))
  data_search1 = gse_database[which(sapply(counts, length)>0),]
  if(verbose==T){
    print('First keyword searched dataset size:')
    print(length(data_search1$gse))
  }


  counts = apply(gse_database , MARGIN=1, function(x)(grep(key2,tolower(x))))
  data_search2 = gse_database[which(sapply(counts, length)>0),]
  if(verbose==T){
    print('Second keyword searched dataset size:')
    print(length(data_search2$gse))
  }

  data_search_bind = unique(intersect(data_search1$gse, data_search2$gse))
  return(data_search_bind)
}



#' The function to print the most frequent platform.
#'
#' @param limitN An integer variable to choose largest number query from the database.
#' @param topN Due to too much platform collected in the database, it is better to only print N most frequent platforms.
#' @param verbose A logical variable. It will print some disgnostic information if TRUE.
#' @param db The connection variable which contains all metadata.
#' @param subTable Character variable. It is the sub-table name which stores different types of metadata. .
#' @return A variable contains Top N platform information.
print_top_N_platform <- function(limitN=2000, topN=10, verbose=TRUE, db=con, subTable='gse'){
  search_statement = paste('select * from', subTable, 'limit', limitN, sep = ' ')
  gse_database0 = dbGetQuery(db, search_statement)
  #gse_database = gse_database0[,c('title', 'gse', 'summary', 'type', 'overall_design')]
  gse_database = gse_database0
  if(verbose==T) dim(gse_database)

  length(sort(table(gse_database$type),decreasing=T))
  top10platform = sort(table(gse_database$type),decreasing=T)[1:topN]
  print(top10platform)

  return(top10platform)
}



#' The function to show the barplot of the most N frequent platforms in current dataset.
#'
#' @param top10platform A variable returned from function of print_top_N_platform.
#' @return A barplot figure.
barplot_top_N_platform <- function(top10platform){
  df_plot = data.frame(platform = names(top10platform), Freq = as.numeric(top10platform))

  p<-ggplot(df_plot, aes(x=platform, y=Freq, fill=platform)) +
    geom_bar(stat="identity")+
    ggtitle('Compare the Platform across the Downloaded Datasets') +
    theme(axis.text.x=element_blank())
  print(p)
}


#' The interactive plot function for plotting the interested platform of user selected
#'
#' @param top10platform A variable returned from function of print_top_N_platform.
# Gadget example from https://shiny.rstudio.com/articles/gadgets.html
barplot_interact <- function(top10platform) {
  # prepare data from for interactive ggplot
  allPlatform = names(top10platform)
  df_plot = data.frame(platform = names(top10platform), Freq = as.numeric(top10platform))
  rownames(df_plot) = df_plot$platform

  # UI part
  ui = fluidPage(
    titlePanel("GEObridge interactive plot function"),
    sidebarLayout(
      # Inputs
      sidebarPanel(
        awesomeCheckboxGroup(inputId = 'platform', label = 'Select platforms you want to compare', choices =allPlatform, selected = allPlatform[3])
      ),
      # Outputs
      mainPanel(
        # textOutput("selected_var1"),
        # textOutput("selected_var2"),
        plotOutput('bars'),
        # br(), hr(), br(),
      )
    )
  )

  # SERVER part
  server = function(input, output) {
    dat <- reactive({
      df_plot[input$platform, ]
      #mydata[which(names(mydata) == input$platform)]
    })
    # output$selected_var1 <- renderText({
    #   paste0(input$platform, sep='------')
    #   #paste0(mydata[loca], sep='------')
    # })
    # output$selected_var2 <- renderText({
    #   #paste0(input$platform, sep='------')
    #   paste0(dat(), sep='------')
    # })
    output$bars <- renderPlot({
      ggplot(dat(), aes(x=platform, y=Freq, fill=platform)) +
        geom_bar(stat="identity")+
        ggtitle('Compare the Platform across the Downloaded Datasets') +
        theme(axis.text.x=element_blank(), plot.title = element_text(size=20))
    })
  }

  viewer <- paneViewer(300)
  runGadget(ui, server, viewer = viewer)
}
# test function
#barplot_interact(top10)





