# This is a Shiny web application. You can run the application by clicking
# We work with two public api due to limited credits for each api
# API: CoinMarketCap
# API: Coinmaker
# API: Reddit


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
# data manipulation and plot
library(tidyverse)
library(ggthemes)
library(ggforce)
library(plotly)
# api package
library(coinmarketcapr)
library(cryptowatchR)
library(RedditExtractoR)
# connect to API tool
library(httr)
# library(jsonlite)
# work with Epoch time to UTC
library(lubridate)
# for word cloud
library(tm)
library(wordcloud)
library(memoise)
library(reshape2)
## convert plots to gtable objects
library(gtable)
library(grid) # low-level grid functions are required





# Word Cloud function

# The list of valid books
subreddits <<- list("BITCOINBEGINNERS", "CRYPTOCURRENCIES", "CRYPTOMARKETS")

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(subreddit) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(subreddit %in% subreddits)) {
    stop("Unknown subreddit")
  }


  # allComments_df <- (sprintf("./%s.txt.gz", subreddit),encoding="UTF-8")

  allComments_df <- find_thread_urls(subreddit = subreddit, sort_by = "top") %>%
    as_tibble() %>%
    select(text) %>%
    unique()

  commentCorpus <- Corpus(VectorSource(allComments_df))
  # We pipe the corpus through several tm_map() methods
  commentCorpus <- commentCorpus %>%
    tm_map(removePunctuation) %>% ## eliminate punctuation
    tm_map(removeNumbers) %>% # no numbers
    tm_map(stripWhitespace) %>% # white spaces
    tm_map(tolower) %>% ## make all words lowercase
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(removeWords, stopwords("SMART")) %>%
    tm_map(removeWords, c("ive", "dont"))

  # convert the corpus to a matrix to facilitate fursther analysis
  commentCorpus_mat <- as.matrix(TermDocumentMatrix(commentCorpus))
  commentCorpus_wordFreq <-
    sort(rowSums(commentCorpus_mat), decreasing = TRUE)
})


# plot for the analysis page
topcryptodata <-
  read_csv("topcryptodata.csv", col_types = cols(...1 = col_skip()))

plot.currencies <- function(data, slugs) {
  data <- data[data$currency_slug %in% slugs, ]
  data$volatility.30d <-
    Reduce(c, sapply(
      unique(data$currency_slug),
      FUN = function(x) {
        sapply(
          1:length(data[data$currency_slug == x, ]$logreturn),
          FUN = function(i) {
            sd(data[data$currency_slug == x, ]$logreturn[(max(i - 30, 0):i)])
          }
        )
      }
    )) * sqrt(365)
  p1 <-
    ggplot(data, aes(datetime, cap, color = factor(currency_slug))) +
    geom_line() +
    labs(
      x = "Date",
      y = "Market cap",
      title = paste(slugs, collapse = ", ")
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.title = element_blank()
    )
  p2 <-
    ggplot(data, aes(datetime, logreturn, color = factor(currency_slug))) +
    geom_line() +
    labs(x = "Date", y = "Log return") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.title = element_blank()
    )
  p3 <-
    ggplot(data, aes(datetime, volatility.30d, color = factor(currency_slug))) +
    geom_line() +
    labs(x = "Date", y = "Annualized volatility")
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  g <- rbind(g1, g2, g3, size = "first") # stack the plots
  g$widths <-
    unit.pmax(g1$widths, g2$widths, g3$widths) # use the largest widths
  # center the legend vertically
  g$layout[grepl("guide", g$layout$name), c("t", "b")] <- c(1, nrow(g))
  grid.newpage()
  grid.draw(g)
}

plot.return.vs.return <- function(currency1, currency2, data) {
  currency1_logreturn <-
    data %>%
    filter(currency_slug == currency1) %>%
    select(logreturn)
  currency2_logreturn <-
    data %>%
    filter(currency_slug == currency2) %>%
    select(logreturn)

  data_new <- cbind(currency1_logreturn, currency2_logreturn)
  names(data_new)[1] <- paste("logreturn_", currency1, sep = "")
  names(data_new)[2] <- paste("logreturn_", currency2, sep = "")
  data_new <- data_new %>% as_tibble()
  cor_ <- cor(currency1_logreturn, currency2_logreturn)
  p <-
    ggplot(data_new, aes_string(
      x = paste("logreturn_", currency1, sep = ""),
      y = paste("logreturn_", currency2, sep = "")
    ))
  p + geom_point() + geom_hline(yintercept = 0) + geom_vline(
    xintercept =
      0
  ) + geom_abline(color = "blue") +
    labs(
      title = paste(
        "Returns: ",
        currency1,
        " vs ",
        currency2,
        " (cor = ",
        round(cor_, digits = 4),
        ")",
        sep = ""
      ),
      x = paste(currency1, "Return"),
      y = paste(currency2, "Return")
    ) +
    theme(legend.title = element_blank())
}

# sidebar def
sidebar <- dashboardSidebar(
  sidebarMenu(
    selectInput(
      inputId = "CryptoType",
      label = " Cryptocurrencies",
      choices = c("Bitcoin"),
      selected = "Bitcoin"
    ),
    selectInput(
      inputId = "Currency",
      label = "Currencies",
      choices = c("USD"),
      selected = "USD"
    ),
    # side bar name and icon
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard", lib = "font-awesome")
    ),
    menuItem("Market Analysis", tabName = "Market", icon = icon("th")),
    menuItem(
      "Top Cryptocurrency Analysis",
      tabName = "Top",
      icon = icon("signal")
    ),
    menuItem("Community", tabName = "Community", icon = icon("comment")),
    menuItem(
      "About",
      tabName = "About",
      icon = icon("pushpin", lib = "glyphicon")
    )
  )
)



body <- dashboardBody(tabItems(
  tabItem(
    tabName = "dashboard",
    h2(strong("Cryptocurrency Dashborad"), align = "center"),
    br(),
    fluidRow(
      # Dynamic infoBoxes
      infoBoxOutput("CurrencyNameBox"),
      infoBoxOutput("PriceBox"),
      infoBoxOutput("approvalBox")
    ),
    fluidRow(
      infoBoxOutput("TotalSupplyBox2"),
      infoBoxOutput("MarketCapBox2"),
      infoBoxOutput("VolumeBox2")
    ),
    # 24 hr plot of currency change
    plotOutput("LinePlot_24hr", click = "plot_click"),

    # main content
    h2("Introduction"),
    p(
      "With the recent heat in cryptocurrency, many people have started to consider it as a real investment opportunity. Cryptocurrencies, and related areas of interest, have a wide-ranging business, economic, environmental, legal, political, and regulatory implications. Many pension funds even started to put cryptocurrency in their portfolio. If there is a catastrophic collapse of cryptocurrencies such as Bitcoin, how will the world economy be affected and impacted by the collapse of a trillion-dollar bubble? We want to look at the overall market, dive deeper into individual cryptocurrencies, analyze their performance, and help others understand the mystery of the decentralized market."
    ),
    h2("Research Questions"),
    p("Some Question"),
    h2("Data Source"),
    p("We use multiple API to obtain the newest data of a variety of cryptocurrencies. We also utilized Reddit API to scrape community discussion in three different subreddit. Here is the API we used:"),
    tags$ul(tags$li(
      tags$a(href = "https://coinmarketcap.com/api/", "CoinMarketCap")
    ),
    tags$li(
      tags$a(href = "https://docs.cryptowat.ch/rest-api", "Cryptowatch")
    ),
    tags$li(tags$a(href = "https://www.reddit.com/dev/api/", "Reddit")))
  ), 
  tabItem(
    tabName = "Market",
    h2("Market Capitalization Analysis"),
    # top currency
    plotOutput("Top5MarketCap"),
    plotOutput("TreeMarketCap", width = "100%", height = "1000")
  ),
  tabItem(
    tabName = "Top",
    h2("Top 3 Crypotpcurrency Analysis"),
    p("After we look at the market cap of all the cryptocurrencies, we can clearly see that the top three cryptocurrencies take up more than 60%
      of the market capitalization. We want to dive deeper into the three and perform some basic analysis. Bitcoin and Ethereum are decentralized
      digital currencies traded and stored in cryptocurrency wallets. They both use the distributed ledger technology known as Blockchain. On the
      other hand, Tether (USDT) is a popular stablecoin, the value of which is pegged to the US Dollar in this case, that crypto enthusiasts have
      used for years to leverage their cryptocurrency trades. We will use Tether as the baseline to show the price change of the other two cryptocurrencies."),
    fluidRow(
      column(
        4,
        dateRangeInput(
          "dates",
          label = h3("Date range"),
          start = "2013-10-09",
          end   = "2022-06-24",
          min =  "2013-10-09",
          max = "2022-06-24"
        )
      ),
      column(
        4,
        checkboxGroupInput(
          "checkGroup",
          label = h3("Select Currency to Comapare"),
          width = "100%",
          choices = list(
            "BTC" = "BTC",
            "ETH" = "ETH",
            "USDT" = "USDT"
          ),
          selected = c("BTC", "ETH", "USDT")
        )
      )
    ),
    # compare top three currency
    plotOutput("cryptocompare"),
    h3("Is Bitcoin An \"Uncorrelated Asset\"?"),
    tags$div(tags$ul(
      tags$li(
        "We investigated the correlation between the returns of some Cryptocurrencies, gold and big stock indices (S&P 500 and Dow Jones)"
      ),
      tags$li(
        "The Pearson correlation coefficient shows the extend to which two data sets (in this case: daily returns) are related."
      ),
      tags$li("It takes values between -1 and 1:"),
      tags$ul(
        tags$li("1 means a strong positive correlation"),
        tags$li("0 means no correlation"),
        tags$li("-1 means a strong negative correlation")
      ),
    )),
    img(src = "CorrlationAllAsset.png", height = 600, width = 1000),
    h3("What We Learn?"),
    tags$div(tags$ul(
      tags$li("Gold is not correlated with any of the other data sets including Bitcoin"),
      tags$li("Bitcoin and the S&P 500 showed no correlation until the Coronacrisis hit both stocks and Bitcoin hard, resulting in a growing correlation. With a coefficient of about 0.4 over the last 180 days, it is still very weak."),
      tags$li("Cryptocurrencies amongst each other are positively correlated")
    )),
    h3("Here is an Example of the Correlation between the Top 2 Cryptocurrencies' Returns"),
    plotOutput("corrlationplot"),
    tags$div("You can learn more using",
        tags$a(href="https://www.blockchaincenter.net/en/cryptocurrency-correlation-study/?timeframe=180days&asset1=SP500&asset2=BTC#correlationtable", "Cryptocurrency Correlation Tool"))
    
  ),
  tabItem(
    tabName = "Community",
    fluidPage(
      # Application title
      titlePanel("Reddit Top Comment Word Cloud"),
      sidebarLayout(
        # Sidebar with a slider and selection inputs
        sidebarPanel(
          selectInput("selection", "Choose a Subreddit:",
            choices = subreddits
          ),
          actionButton("update", "Change"),
          hr(),
          sliderInput(
            "freq",
            "Minimum Frequency:",
            min = 1,
            max = 50,
            value = 15
          ),
          sliderInput(
            "max",
            "Maximum Number of Words:",
            min = 1,
            max = 300,
            value = 100
          )
        ),

        # Show Word Cloud
        mainPanel(plotOutput("wordcloudplot"))
      )
    )
  ),
  # member table
  tabItem(
    tabName = "About",
    fluidRow(column(
      width = 6,
      h2("Team Members")
    )),
    fluidRow(
      userBox(
        title = userDescription(
          title = "Zhuojue Wang",
          subtitle = "lead Developer",
          type = 2,
          image = "https://media-exp2.licdn.com/dms/image/C5603AQEKOqUz_wc8ag/profile-displayphoto-shrink_800_800/0/1541652423442?e=1660176000&v=beta&t=7GV52KgatnGfBTuBLSIFu_sqobhWZokAW4OqTJ7SRMg",
          backgroundImage = "https://love2dev.com/img/html-space-640x426.jpg"
        ),
        # status = "maroon",
        p(
          "Zhuojue is a Master of Business Analytics Risk Management Candidate at Johns Hopkins Carey Business School. He received his bachelor degree from UC Davis with a double major in Statistics and Economics. He is passionate in machine learning applications in business analytics with causal analysis and on his way to become a researcher in the field."
        )
      ),
      userBox(
        title = userDescription(
          title = "Xuanyu Chen",
          subtitle = "lead Developer",
          type = 2,
          image = "https://media-exp2.licdn.com/dms/image/C5603AQGoVZTASX00hA/profile-displayphoto-shrink_400_400/0/1654536542969?e=1660176000&v=beta&t=hnYpWoYtwOxflCnPS5sVqzXhZfXJXVMbOUzf4GeNMTg",
          backgroundImage = "https://www.freecodecamp.org/news/content/images/size/w2000/2021/06/w-qjCHPZbeXCQ-unsplash.jpg"
        ),
        # status = "warning",
        p(
          "Xuanyu Chen is currently a student in Business Analytics and Risk Management major at Johns Hopkins University. She graduated from Northeastern University with Finance major. She has experience in private equity, project management, and business strategy. "
        )
      )
    ),
    fluidRow(
      userBox(
        title = userDescription(
          title = "Xin Kang",
          subtitle = "lead Developer",
          type = 2,
          image = "https://media-exp2.licdn.com/dms/image/C4E03AQHqkeN8PT-W0g/profile-displayphoto-shrink_400_400/0/1627371502253?e=1660176000&v=beta&t=w9-TpI7oZVcZfucwW5QRSv9xAu_yoETz0aQabgyy7RE",
          backgroundImage = "https://www.freecodecamp.org/news/content/images/size/w2000/2021/06/w-qjCHPZbeXCQ-unsplash.jpg"
        ),
        # status = "warning",
        p(
          "Xuanyu Chen is currently a student in Business Analytics and Risk Management major at Johns Hopkins University. She graduated from Northeastern University with Finance major. She has experience in private equity, project management, and business strategy. "
        )
      ),
      userBox(
        title = userDescription(
          title = "Bowen Tan",
          subtitle = "lead Developer",
          type = 2,
          image = "https://media-exp2.licdn.com/dms/image/C5603AQF1J_c3-GzCZQ/profile-displayphoto-shrink_400_400/0/1609620802392?e=1660176000&v=beta&t=daJtmVYNWtUuECVCXs3WApYx2gRzJaezvER44NlatjI",
          backgroundImage = "https://www.freecodecamp.org/news/content/images/size/w2000/2021/06/w-qjCHPZbeXCQ-unsplash.jpg"
        ),
        # status = "warning",
        p(
          "Xuanyu Chen is currently a student in Business Analytics and Risk Management major at Johns Hopkins University. She graduated from Northeastern University with Finance major. She has experience in private equity, project management, and business strategy. "
        )
      )
    ),
    fluidRow(userBox(
      title = userDescription(
        title = "Yitong Fu",
        subtitle = "lead Developer",
        type = 2,
        image = "https://media-exp2.licdn.com/dms/image/C4E03AQE_TGsVDLzI4Q/profile-displayphoto-shrink_400_400/0/1638992793893?e=1660176000&v=beta&t=qXG--KYg68dze5QG8FR-Oqky20CqG3R3VZiyYuPuLeY",
        backgroundImage = "https://www.freecodecamp.org/news/content/images/size/w2000/2021/06/w-qjCHPZbeXCQ-unsplash.jpg"
      ),
      # status = "warning",
      p(
        "Xuanyu Chen is currently a student in Business Analytics and Risk Management major at Johns Hopkins University. She graduated from Northeastern University with Finance major. She has experience in private equity, project management, and business strategy. "
      )
    ))
  )


  # tabItem(tabName = "About",
  #         box( title = h1("Members"),status = "warning",solidHeader = TRUE, width = 12,collapsible = TRUE,
  #           box(width = 12,
  #             fluidRow(column(width = 2, align = "center" ,img(src = "ZhuojueWang.jpg", height = 150, width = 100)),
  #                      column(width = 10,
  #                             h3("Zhuojue Wang"),
  #                             p("Zhuojue is a Master of Business Analytics Risk Management Candidate at Johns Hopkins Carey Business School. He received his bachelor degree from UC Davis with a double major in Statistics and Economics. He is passionate in machine learning applications in business analytics with causal analysis and on his way to become a researcher in the field.")
  #                       )
  #
  #             )
  #           ),
  #           box(
  #             h3("Xuanyu Chen"), br(), "he is abcde", width = 12
  #           ),
  #           box(
  #             h3("Xin Kang"), br(), "he is abcde", width = 12
  #           ),
  #           box(
  #             h3("Bowen Tan"), br(), "he is abcde", width = 12
  #           ),
  #           box(
  #             h3("Yitong Fu"), br(), "he is abcde", width = 12
  #           )
  #         )
  # )
))



# Define UI for
ui <- dashboardPage(dashboardHeader(title = span(
  tagList(icon("bitcoin"), " Cryptocurrency Dashboard")
), titleWidth = 350),
# side bar menu
sidebar, body,
skin = c("red")
)



server <- function(input, output, session) {
  # connected to server
  key <- "33c270b5-e528-44fd-8443-0b4493f76019"
  coinmarketcapr::setup(key)

  # list of not supported crypto
  list_Supported_Cryptocurrency <-
    c(
      "BNB",
      "Binance USD",
      "UNUS SED LEO",
      "Cronos",
      "NEAR Protocol",
      "FTX Token",
      "VeChain",
      "Hedera",
      "KuCoin Token",
      "Elrond",
      "TrueUSD",
      "Theta Network",
      "Helium",
      "Huobi Token",
      "Klaytn",
      "eCash",
      "BitTorrent-New",
      "IOTA",
      "Pax Dollar",
      "Neo",
      "Neutrino USD",
      "PancakeSwap",
      "Stacks",
      "USDD",
      "Nexo",
      "OKB",
      "Zilliqa",
      "Celo",
      "Decred",
      "Harmony",
      "Amp",
      "Arweave",
      "NEM",
      "XDC Network",
      "Holo",
      "Gate Token",
      "Fei USD",
      "Bitcoin Gold",
      "Kadena"
    )
  # fill the sidebar Input CryptoType
  observeEvent(
    input$CryptoType,
    updateSelectInput(
      session,
      "CryptoType",
      label = "Cryptocurrency Name",
      choices = coinmarketcapr::get_crypto_listings() %>% filter(!name %in% list_Supported_Cryptocurrency) %>% select(name) %>% as.list() %>% unlist(
        use.names =
          FALSE
      ),
      selected = input$CryptoType
    )
  )
  # fill the sidebar Input Currencies
  observeEvent(
    input$Currency,
    updateSelectInput(
      session,
      "Currency",
      label = "Currency Name",
      choices = c(coinmarketcapr::get_valid_currencies()),
      selected = input$Currency
    )
  )

  # get data for all Box
  dataInputBox <- reactive({
    get_crypto_listings(currency = input$Currency) %>% filter(name == input$CryptoType)
  })
  # Dashboard top box summary
  output$CurrencyNameBox <- renderInfoBox({
    infoBox(
      "Currency Name",
      h3(input$CryptoType),
      icon = icon("btc"),
      color = "blue"
    )
  })
  output$PriceBox <- renderInfoBox({
    infoBox(
      "Price",
      dataInputBox() %>% select(paste0(input$Currency, "_price")) %>% format(big.mark = ",", scientific = FALSE) %>% h3(),
      icon = icon("usd", lib = "glyphicon"),
      color = "purple"
    )
  })

  # observe the change and change the icon direction accordingly
  obs_percent_change_24h <-
    reactive({
      dataInputBox() %>% select(paste0(input$Currency, "_percent_change_24h"))
    })

  output$approvalBox <- renderInfoBox({
    if (obs_percent_change_24h() > 0) {
      infoBox(
        "Percent Change(24HR)",
        paste0(
          dataInputBox() %>% select(paste0(
            input$Currency, "_percent_change_24h"
          )) %>% format(big.mark = ",", scientific = FALSE),
          "%"
        ) %>% h3(),
        icon = icon("arrow-up"),
        color = "yellow"
      )
    } else {
      infoBox(
        "Percent Change(24HR)",
        paste0(
          dataInputBox() %>% select(paste0(
            input$Currency, "_percent_change_24h"
          )) %>% format(big.mark = ",", scientific = FALSE),
          "%"
        ) %>% h3(),
        icon = icon("arrow-down"),
        color = "yellow"
      )
    }
  })


  # Second line of Box but with fill=TRUE
  output$TotalSupplyBox2 <- renderInfoBox({
    infoBox(
      "Total Supply",
      dataInputBox() %>% select(total_supply) %>% format(big.mark = ",", scientific = FALSE) %>% h3(),
      icon = icon("piggy-bank"),
      color = "blue",
      fill = TRUE
    )
  })
  output$MarketCapBox2 <- renderInfoBox({
    infoBox(
      "Market Cap",
      dataInputBox() %>% select(paste0(input$Currency, "_market_cap")) %>% format(big.mark = ",", scientific = FALSE) %>% h3(),
      icon = icon("tags", lib = "glyphicon"),
      color = "purple",
      fill = TRUE
    )
  })
  output$VolumeBox2 <- renderInfoBox({
    infoBox(
      "Volume(24hr)",
      dataInputBox() %>% select(paste0(input$Currency, "_volume_24h")) %>% format(big.mark = ",", scientific = FALSE) %>% h3(),
      icon = icon("list-alt", lib = "glyphicon"),
      color = "yellow",
      fill = TRUE
    )
  })


  # Because the two api doesn't support the same currency, we have to eliminated the issues
  # validate function
  not_supported_currency <- function(input) {
    list_Supported_Currency <-
      c("AUD", "CAD", "CHF", "EUR", "GBP", "JPY", "USD")
    if (!(input %in% list_Supported_Currency)) {
      "The choosen Currency is not support for the graph, Please choose another Currency."
    } else {
      NULL
    }
  }



  # get the three letter name of crytocurrcy
  selected_symbol <-
    reactive({
      get_crypto_listings() %>%
        filter(name == input$CryptoType) %>%
        select(symbol)
    })
  # get data for the plot from crytowatch API
  crytowatch_data <- reactive({
    validate(not_supported_currency(input$Currency))


    # endpoint <- str_glue("https://api.cryptowat.ch/markets/kraken/{cryptocurrency}{currency}/ohlc",
    #                      cryptocurrency = "btc",
    #                      currency = "usd")

    # cryptocurrency = get_crypto_listings() %>% filter(name==input$CryptoType) %>% select(symbol) %>% tolower(),
    # currency = tolower(input$Currency))
    # endpoint <-("https://api.cryptowat.ch/pairs")
    # w <- GET(endpoint)
    # fromJSON(content(w, as = "text", encoding = "UTF-8"), flatten = TRUE)

    # Settings
    exchange <- "kraken"
    pair <-
      paste0(selected_symbol() %>% tolower(), tolower(input$Currency))
    route <- "ohlc"
    api_key <- "6NO31ET00O220TWYIMDR"

    # Daily prices for longest possible time period
    get_markets(route,
      pair,
      exchange,
      api_key = api_key,
      allowance = TRUE
    )
  })


  output$LinePlot_24hr <- renderPlot({
    # select last 24 hr data with 3 mins inteval
    closeprice_24 <-
      crytowatch_data()$result$`180` %>%
      as_tibble() %>%
      tail(480) %>%
      rename(
        CloseTime = V1,
        OpenPrice = V2,
        HighPrice = V3,
        LowPrice = V4,
        ClosePrice = V5,
        Volume = V6,
        QuoteVolume = V7
      )

    closeprice_24$CloseTime <-
      closeprice_24$CloseTime %>% as_datetime()

    # need to change the date in english form since my computer is in Chinese :(
    Sys.setlocale("LC_TIME", "C")

    ggplot(
      data = closeprice_24,
      aes(x = CloseTime, y = ClosePrice)
    ) +
      xlab("Date Time (UTC)") +
      ylab("Price") +
      ggtitle(
        paste("Price Change Over Last 24 Hours -", selected_symbol()),
        subtitle = paste(
          "Most recent data collected on:",
          closeprice_24$CloseTime %>% tail(1),
          "(UTC)"
        )
      ) +
      geom_line() +
      stat_smooth(formula = y ~ x, method = "loess") +
      theme_economist() +
      # labal max price
      geom_mark_ellipse(
        aes(
          filter = ClosePrice == max(ClosePrice),
          label = CloseTime,
          description = paste0("Price spike to $", ClosePrice)
        ),
        con.colour = "red"
      ) +
      # Now the same to circle the minimum price:
      geom_mark_ellipse(
        aes(
          filter = ClosePrice == min(ClosePrice),
          label = CloseTime,
          description = paste0("Price drop to $", ClosePrice)
        ),
        con.colour = "red"
      )
  })

  # second page
  output$Top5MarketCap <- renderPlot(
    {
      plot_top_currencies(input$Currency) + theme_economist()
    },
    res = 96
  )

  output$TreeMarketCap <- renderPlot({
    market_today <- get_crypto_listings()

    df1 <- na.omit(market_today[, c("name", "USD_market_cap")])
    df1$USD_market_cap <- as.numeric(df1$USD_market_cap)
    df1$formatted_market_cap <-
      paste0(
        df1$name,
        "\n",
        "$",
        format(
          df1$USD_market_cap,
          big.mark = ",",
          scientific = F,
          trim = T
        )
      )
    # plot the market share of coins
    treemap::treemap(
      df1,
      index = "formatted_market_cap",
      vSize = "USD_market_cap",
      title = "Cryptocurrency Market Cap",
      fontsize.labels = c(12, 8),
      palette = "RdYlBu",
      asp = 2
    )
  })


  # analysis page
  output$cryptocompare <- renderPlot({
    topcryptodata <- topcryptodata %>% filter(datetime > input$dates[1] & datetime < input$dates[2])
    plot.currencies(topcryptodata, input$checkGroup)
  })

  output$corrlationplot <- renderPlot({
    plot.return.vs.return("BTC", "ETH", topcryptodata[topcryptodata$datetime >
      as.Date("2017-12-31 UTC"), ])
  })



  # community page
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })

  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)

  output$wordcloudplot <- renderPlot({
    v <- terms()
    wordcloud_rep(
      names(v),
      v,
      scale = c(4, 0.5),
      min.freq = input$freq,
      max.words = input$max,
      colors = brewer.pal(8, "Dark2")
    )
  })

  # last page
}



# Run the application
shinyApp(ui = ui, server = server)
