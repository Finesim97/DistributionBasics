#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
types = list("Normal" = 1, "LogNormal" = 2,"Binomial"=3,"Hypergeom."=4,
             "Eruptions" = 5)
theme_set(theme_linedraw())

extract_help <- function(pkg, fn = NULL, to = c("txt", "html", "latex", "ex")){
  to <- match.arg(to)
  rdbfile <- file.path(find.package(pkg), "help", pkg)
  rdb <- tools:::fetchRdDB(rdbfile, key = fn)
  convertor <- switch(to, 
                      txt   = tools::Rd2txt, 
                      html  = tools::Rd2HTML, 
                      latex = tools::Rd2latex, 
                      ex    = tools::Rd2ex
  )
  f <- function(x) capture.output(convertor(x))
  if(is.null(fn)) lapply(rdb, f) else f(rdb)
}
helptext <- extract_help("stats","quantile","txt")
pos <- sapply(c("_\\\bT_\\\by_\\\bp_\\\be_\\\bs:","_\\\bA_\\\bu_\\\bt_\\\bh_\\\bo_\\\br\\(_\\\bs\\):"),grep,helptext)
helptext <- paste0(helptext[(pos[1]+1):(pos[2]-1)],collapse = "\n")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    datavals <- reactive({
        set.seed(input$seed)
        samples <- min(c(input$samples,10^6))
        if(input$disttype==types["Eruptions"]){
            return(sample(faithful$eruptions,size = samples,replace = TRUE))
        }else if(input$disttype %in% c(types["Normal"],types["LogNormal"])){
            mean <- input$mean
            sd <- sqrt(input$var)
            if (input$disttype == types["Normal"]) {
                return(rnorm(samples,mean,sd))
            }else{
                return(rlnorm(samples,mean,sd))
            }
            return(fun(samples,mean,sd))
        }else if(input$disttype %in% c(types["Binomial"],types["Hypergeom."])){
            black <- input$hits
            white <- input$misses
            draws <- input$draws
            if (input$disttype == types["Binomial"]) {
                return(rbinom(samples, draws, prob = black/(white+black)))
            }else{
                return(rhyper(samples,white,black,draws))
            }
        }
    })
    
    output$boxplots <- renderPlot({
        quantiletypes <- 1:9
        df <- cbind(type=as.character(quantiletypes),do.call(rbind,lapply(quantiletypes, function(type)as.data.frame(t(quantile(datavals(),type=type))))))
        colnames(df) <- c("type","min","low","mid","top","max")
        ggplot(df, aes(x=type, ymin = min, lower = low, middle = mid, upper = top, ymax = max, group=type)) +
            geom_boxplot(stat = "identity") + labs(title="Quantile Method Comparisons") + xlab("Method") + ylab("Value")
    })
    
    output$hist <- renderPlot({
        dat <- data.frame(Value=datavals())
        ggplot(dat)+ aes(x=Value) +
            geom_histogram(bins=nclass.Sturges(dat$Value),color="black",fill="white") + labs(title="Histogram", subtitle = "Number of bins according to Sturges") + xlab("Value") + ylab("Count")
    })

  output$helptext <- renderText({ 
    helptext
  })

    output$combi <- renderPlot({
        dat <- data.frame(Value=datavals())
        plotbase <- ggplot(dat) + aes(Value) 
        cowplot::plot_grid(plotbase + geom_boxplot() + ylab ("Boxplot") +
                               theme(axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.title.x = element_blank()), 
                           plotbase + geom_density() + ylab("Density") +
                               theme(axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.title.x = element_blank()), 
                           plotbase + stat_ecdf(geom = "step") + xlab("Value") + ylab("Cumulative Frequency"),
                           ncol = 1,
                           align = "h",
                           axis = "bt")
        
    })

})
