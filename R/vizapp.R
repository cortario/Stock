vizapp <-
function(){
  
  header = dashboardHeader(title = "VizApp")
  sidebar = dashboardSidebar(disable = T)
  body = dashboardBody(
    fluidPage(
      fluidRow(  box(width=12,
                     fluidRow(
                       column(width = 4,
                              dateRangeInput('dateRange',
                                             label = 'Date range input: yyyy-mm-dd',
                                             start = "2020-01-01", end = Sys.Date() 
                              ),
                              pickerInput(inputId = "Pick",label = "Select Stocks", choices = sort(c(
                                "AAPL",  "MSFT",  "AMZN",  "FB",    "GOOGL", "GOOG" , "BRK.B", "JPM" ,  "TSLA",  "JNJ",   "NVDA",  "UNH",  
                                "V"  ,   "HD"  ,  "PG" ,   "DIS" ,  "BAC" ,  "MA" ,   "PYPL" , "CMCSA" ,"XOM",   "ADBE" , "VZ" ,   "INTC",
                                "CSCO" , "NFLX" , "CRM" ,  "PFE" ,  "KO" ,   "T" ,    "ABT"  , "PEP" ,  "CVX" ,  "ABBV" , "WMT" ,  "WFC" , 
                                "AVGO" , "MRK"  , "TMO" ,  "ACN"  , "TXN" ,  "MCD" ,  "NKE" ,  "MDT",   "COST",  "C"   ,  "DHR" ,  "HON",  
                                "LLY" ,  "LIN"  , "UPS"  , "QCOM" , "UNP"  , "PM" ,   "BMY" ,  "ORCL" , "NEE" ,  "LOW"  , "AMGN",  "MS" ,  
                                "BA"  ,  "RTX"  , "SBUX" , "CAT"  , "IBM" ,  "GS" ,   "AMAT",  "BLK"  , "GE"  ,  "INTU" , "MMM" ,  "AMT",  
                                "TGT" ,  "CVS" ,  "DE"  , "SCHW"  ,"AXP"   ,"ISRG",  "CHTR" , "ANTM"  ,"AMD"  , "BKNG"  ,"LMT"  , "MU"  , 
                                "NOW" ,  "LRCX" , "FIS"  , "MO"   , "SPGI"  ,"CI" ,   "MDLZ",  "PLD"  , "ZTS" ,  "ADP"  , "SYK" ,  "TFC",  
                                "USB" ,  "GILD",  "TMUS" , "PNC"  , "CCI" ,  "TJX",   "CME" ,  "DUK"  , "FDX" ,  "CB"   , "CSX" ,  "COP",  
                                "ATVI" , "GM"  ,  "COF" ,  "CL"   , "NSC"  , "MMC",   "BDX" ,  "EL"   , "SHW" ,  "FISV" , "SO"  ,  "ITW",  
                                "APD" , "EQIX" , "ICE"  , "ADSK" , "FCX"  , "D"   ,  "ADI"  , "BSX"   ,"EW"   , "ILMN"  ,"NEM"  , "NXPI", 
                                "PGR"  , "ETN" ,  "EMR" ,  "AON" ,  "GPN"  , "HCA",   "F"   ,  "NOC"  , "HUM" ,  "WM"   , "VRTX" , "MCO",  
                                "ECL"  , "REGN" , "DOW"  , "DG"  ,  "MET" ,  "KLAC",  "JCI" ,  "IDXX" , "ROP" ,  "EOG"  , "TWTR",  "IQV",  
                                "AIG" ,  "DD"  ,  "ROST" , "TEL" ,  "LHX"  , "TT" ,   "KMB" ,  "EXC"  , "GD"  ,  "SLB"  , "TROW",  "PSA" , 
                                "CNC" ,  "AEP" ,  "PPG"  , "DLR" ,  "ALGN"  ,"PRU",   "MCHP",  "SPG"  , "A"   ,  "BK"   , "BAX" ,  "ALL"  ,
                                "SYY" ,  "EA"  ,  "SRE" ,  "STZ" ,  "BIIB"  ,"APTV",  "APH" ,  "TRV"  , "MPC" ,  "PH"   , "EBAY",  "INFO", 
                                "ALXN" , "SNPS" , "MSCI" , "CMG" ,  "GIS"   ,"MAR",   "CMI" ,  "CTSH" , "WBA" ,  "ORLY" , "CARR",  "XEL",  
                                "ADM"  , "PSX"  , "HPQ"  , "LUV"  , "AFL"   ,"YUM" ,  "DFS" ,  "KMI"  , "DXCM",  "TDG"  , "CDNS",  "IFF",  
                                "ZBH" ,  "SWK" ,  "MNST" , "HLT"  , "MSI"   ,"CTVA",  "WLTW",  "GLW"  , "FRC" ,  "VLO"  , "SBAC",  "PAYX", 
                                "WMB" ,  "OTIS" , "PCAR" , "DHI"  , "PEG"   ,"XLNX",  "WELL",  "AZO"  , "CTAS",  "AME"   ,"PXD" ,  "ROK",  
                                "STT"  , "MCK"  , "NUE"  , "FAST" , "MTD"   ,"DAL",   "AMP" ,  "SIVB" , "FITB",  "RMD"   ,"WEC" ,  "FTNT", 
                                "CBRE" , "ANSS" , "LYB"  , "AVB"  , "EFX"   ,"WY"  ,  "AJG" ,  "KHC"  , "KR"  ,  "VRSK" , "AWK"  , "SWKS", 
                                "ES"   , "MXIM" , "LEN"  , "KSU"  , "BLL"   ,"CPRT",  "LH"  ,  "BBY"  , "DTE" ,  "ZBRA" , "EQR" ,  "KEYS", 
                                "ED"  ,  "SYF" ,  "WST"  , "VFC" ,  "O"     ,"ODFL" , "HSY" ,  "NTRS" , "VIAC",  "IP"   , "FTV" ,  "VMC",  
                                "EXPE" , "URI" ,  "CERN" , "CCL"  , "OKE"   ,"TSN"  , "HIG" ,  "CDW"  , "WDC" ,  "RSG"  , "DLTR",  "HES",  
                                "FLT" ,  "ARE" ,  "MLM" ,  "RF"   , "PPL"   ,"CZR"  , "KEY" ,  "CLX"  , "MKC" ,  "TER"  , "VRSN",  "OXY",  
                                "DOV" ,  "TTWO" , "XYL"  , "EXPD" , "GRMN",  "CFG"  , "EIX" ,  "TSCO" , "ETR" ,  "CHD"  , "AEE" ,  "VTR",  
                                "QRVO" , "HPE" ,  "GNRC",  "MTB" ,  "FE"  ,  "IT"   , "ETSY",  "RCL"  , "GWW" ,  "WAT"  , "HAL" ,  "TRMB", 
                                "EXR" ,  "ALB"  , "TDY"  , "ULTA",  "COO" ,  "NDAQ" , "STX" ,  "ESS"  , "LVS" ,  "GPC"  , "CE"  ,  "TFX",  
                                "KMX"  , "DRI" ,  "AKAM" , "CAG" ,  "ANET",  "UAL"  , "IR"  ,  "J"    , "BR"  ,  "AMCR" , "VTRS",  "MAA",  
                                "AVY"  , "CINF" , "CMS"  , "ENPH" , "PEAK",  "BKR"  , "MKTX",  "OMC"  , "MGM" ,  "DGX"  , "POOL",  "CTLT", 
                                "DRE" ,  "NTAP" , "EMN"  , "AES"  , "NVR" ,  "ABC"  , "IEX" ,  "CRL"  , "BXP" ,  "K"    , "PFG" ,  "DPZ",  
                                "CAH" ,  "TYL" ,  "LB"  ,  "STE"  , "RJF" ,  "PKI"  , "HOLX",  "PAYC" , "HBAN",  "NLOK" , "DVN" ,  "INCY", 
                                "TXT" ,  "MAS" ,  "PHM" ,  "WRK"  , "FMC" ,  "AAL"  , "WHR" ,  "WAB"  , "BF.B",  "XRAY" , "SJM" ,  "MPWR", 
                                "FANG" , "JBHT" , "FBHS" , "LNT"  , "LKQ" ,  "CTXS" , "UDR" ,  "PTC"  , "PKG" ,  "EVRG" , "CNP" ,  "WYNN", 
                                "HWM"  , "SNA"  , "LDOS",  "LUMN" , "HRL" ,  "L"    , "PWR" ,  "IPG"  , "CHRW",  "BIO"  , "LYV" ,  "ABMD", 
                                "ALLE" , "LNC" ,  "IRM" ,  "FOXA" , "ATO" ,  "TPR"  , "MOS" ,  "MHK"  , "AAP"  , "UHS"  , "HAS" ,  "BWA",  
                                "PENN" , "HST" ,  "LW"  ,  "CBOE" , "JKHY",  "NCLH" , "PNR" ,  "FFIV" , "CF"  ,  "TAP"  , "WRB" ,  "CMA",  
                                "NWL" ,  "HSIC",  "DISH" , "IVZ"  , "NWSA",  "RE"   , "REG" ,  "WU"   , "RHI" ,  "GL"   , "CPB" ,  "NI" ,  
                                "NLSN" , "DXC"  , "AOS" ,  "MRO"  , "PNW" ,  "ZION" , "BEN" ,  "AIZ"  , "KIM" ,  "SEE"  , "DISCK", "HII",  
                                "DVA"  , "JNPR" , "ALK" ,  "PVH"  , "PBCT",  "FRT"  , "NRG" ,  "APA"  , "ROL" ,  "IPGP" , "VNO" ,  "LEG" , 
                                "GPS"  , "HBI" ,  "COG"  , "UNM"  , "NOV" ,  "PRGO" , "RL"  ,  "FOX"  , "DISCA", "HFC"  , "UAA" ,  "UA" ,  
                                "NWS"  )),
                                          options = list(`selected-text-format` = "count > 3"), 
                                          multiple = TRUE
                              ),
                              actionButton("getData","Get Fin. Data")
                       ),
                       
                       column(width = 8,
                              tabBox(width= 12, title ="Data",
                                     tabPanel("Prices",dataTableOutput("Prices")),
                                     tabPanel("Returns",dataTableOutput("Returns"))
                              ),
                              br(),
                              "Donwload Prices: ",downloadLink("downloadP","Download"), br(),
                              "Donwload Returns: ",downloadLink("downloadR","Download"), br()
                       )
                     )    
      )),
      fluidRow(
        box(width = 12,
            column( width = 4,
                    
                    pickerInput(inputId = "Sec",label = "Stock to Plot", choices = "",
                                options = list(`selected-text-format` = "count > 3"), 
                                multiple = FALSE
                    ),
                    selectInput("type", "Choose Plot type", choices = c("auto", "candlesticks", "matchsticks", "bars","line"),selected = "candlesticks"),
                    
                    checkboxInput("ta_vol", label = "Volume", value = FALSE),
                    checkboxInput("ta_sma", label = "Simple Moving Average",  value = FALSE),
                    checkboxInput("ta_bb", label = "Bolinger Bands", value = FALSE),
                    checkboxInput("ta_momentum", label = "Momentum", value = FALSE),
                    checkboxInput("ta_volatility", label = "Volatility", value = FALSE),
                    actionButton("chart_act", "Add Plot")),
            column( width = 8,
                    conditionalPanel(condition = " input.ta_sma == 1",
                                     box(
                                       numericInput("sma_n","SMA Period",value = 5,min = 2,step = 1))
                    ),
                    conditionalPanel(condition = "input.ta_bb == 1",
                                     box(
                                       numericInput("bb_n","BBands Period",value = 10,min = 2,step = 1),
                                       numericInput("bb_sd","BBands sd",value = 2,min = 0),
                                       selectInput("bb_type","Type of Average", choices = c("SMA","EMA","WMA")))
                    ),
                    conditionalPanel(condition = "input.ta_momentum == 1",
                                     box(
                                       numericInput("mom_n","Momentum",value = 1,min = 1,step = 1))
                    ),
                    
                    conditionalPanel(condition = " input.ta_volatility == 1",
                                     box(
                                       numericInput("volatility_n","Volatility Period",value = 5,min = 2,step = 1))
                    ))
        )
      ),
      fluidRow(
        box(width = 12,
            plotOutput("plot"))
      )
    )
  )
  ui <- dashboardPage( skin = "blue", header, sidebar, body)
  server <- function(input, output){
    
    # Generals -----
    # Download Prices Returns Volumes
    output$downloadP <- downloadHandler(
      filename = "Prices.csv",
      content = function(file) {
        write.csv(Prices, file)
      }
    )
    
    output$downloadR <- downloadHandler(
      filename = "Returns.csv",
      content = function(file) {
        write.csv(Returns, file)
      }
    )
    
    # Update Choices P2Box1 & P4Box1
    ch = reactive({
      D = c()
      if(!is.null(input$Pick)){
        D = input$Pick
      }else{
        D = NULL
      }
    })
    
    
    observe({
      updatePickerInput(session = getDefaultReactiveDomain(), "Sec",
                        choices = ch()
      )
    })
    
    
    # Server -----
    observeEvent(input$getData,{
      
      Ti = input$Pick
      
      from = input$dateRange[1]
      to = input$dateRange[2]
      
      Res = data_download(Ti,from ,to )
      Prices <<- Res[["Prices"]]
      Returns <<- Res[["Returns"]]
      Names <<- Res[["Downloaded_Stock"]]
      for(i in 1:length(Ti)){
        
        
        if(grepl("_", Ti[i], fixed = TRUE)){
          N2 = gsub("\\_","-",Ti[i])
          assign(paste0(Ti[i]),quantmod::getSymbols(N2, src = "yahoo", from = from, to = to, auto.assign = FALSE),envir = globalenv())
        }else{
          assign(paste0(Ti[i]),quantmod::getSymbols(Ti[i], src = "yahoo", from = from, to = to, auto.assign = FALSE),envir = globalenv())
        }
        
      }
      
      output$Prices = renderDataTable({
        datatable(Prices, 
                  options=list(searching = F)
        )
      })
      
      output$Returns = renderDataTable({
        datatable(round(Returns,4),
                  options=list(searching = F)
        )
      })
      
      
    })
    
    TAInput <- reactive({
      if (input$chart_act == 0)
        return("NULL")
      
      tas <- isolate({c(input$ta_vol, input$ta_sma, 
                        input$ta_bb, input$ta_momentum,
                        input$ta_volatility)
      })
      
      funcs <- c(addVo(), 
                 paste0("addSMA(n =", input$sma_n,")"), 
                 paste0("addBBands(n =", input$bb_n,",sd =",input$bb_sd,",maType = ",input$bb_type ,")"),    
                 paste0("addMomentum(n =", input$mom_n,")"),
                 paste0("addVolatility(n =", input$volatility_n,")")
      )
      
      
      
      if (any(tas)) funcs[tas]
      else "NULL"
    })
    
    observeEvent(input$chart_act,{
      
      output$plot = renderPlot({
        chartSeries(eval(parse(text=input$Sec)),
                    name = input$Sec,
                    type = input$type,
                    theme = "white",
                    TA = TAInput()
        ) 
      })
    })
    
    
  }
  shinyApp(ui = ui, server = server)
}
