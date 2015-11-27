library(dplyr)
library(swiRcharts)
library(swiTheme)

############################################################################################
###		Load data
############################################################################################

data <- read.csv("output/historicalEmissions.csv", stringsAsFactors = F, check.names = F)

trad <- read.csv("output/historicalEmissionsTranslations.csv",
  check.names = F, stringsAsFactors = F, row.names = 1)

## TMP code to generate the trad file
# write.csv(data.frame(code = paste0("country.", gsub("( *|\\(|\\))", "", unique(data$Country))), en =  unique(data$Country)),
#   file = "output/historicalEmissionsTranslations.csv", row.names = F)

############################################################################################
###		Settings chart
############################################################################################

chartHeight <- 550

############################################################################################
###		chart cumulative
############################################################################################

i <- 1

for (i in 1:ncol(trad)) {

  lang <- colnames(trad)[i]
  output.html <- paste("output/02_CO2emissions_stackedArea_", lang, ".html", sep ="")

  dd <- data

  #country translations
  countries <- structure(trad[grepl("^country", rownames(trad)), lang],
    names = paste0("country.", gsub("( *|\\(|\\))", "", unique(data$Country))))

  idx <- match(paste0("country.", gsub("( *|\\(|\\))", "", dd$Country)), names(countries))
  if(any(is.na(idx))) stop("Some country translations cannot be matched!", "\n")
  dd$Country <- countries[idx]

  ## create fancy tooltip as html table
  dd$name <- paste0(
    '<table cellpadding="1" style="line-height:1.2">',
    '<tr><td><div style="font-size:0.9em"><b>', dd$Year, '</b></td></div>',
    '<td></td><td></td></tr>',
    '<tr><td align="left" colspan="2"><div style="font-size:0.9em">', dd$Country,'</td><td></td></tr>',
    '<tr><td colspan="3"><div style="font-size:0.85em"><b>',
      round(dd$`Total CO2 Emissions Excluding Land-Use Change and Forestry (MtCO2)`), "</b> ",
    trad['tp.mtco2',lang], '</div></td></tr>',
    '</table>')

  ## CHART
  a <- Highcharts$new()
  a$chart(zoomType = "xy", type = 'area', height = chartHeight, spacing = 5)
  hSeries <- hSeries2(data.frame(
    x = dd$Year,
    y = dd$`Total CO2 Emissions Excluding Land-Use Change and Forestry (MtCO2)`,
    name = dd$name,
    series = dd$Country), "series")
  a$series(hSeries)

  a$colors(swi_pal)
  a$plotOptions(area = list(stacking = "normal",
    lineWidth = 0, marker = list(enabled = FALSE, symbol = "circle", radius = 1)),
    series = list(fillOpacity = 1, trackByArea = TRUE))

  a$legend(borderWidth= 0, itemMarginTop = 3, itemMarginBottom = 5, itemHoverStyle = list(color = '#996666'),
    itemStyle = list(fontWeight = "normal", fontSize = "0.8em"),
    title = list(style = list(fontWeight ='normal'),
    text = paste0(trad['legend.country',lang], ' <span style="font-size: 9px; color: #666; font-weight: normal">',
    trad['legend.descr',lang], '</span><br>')), style = list(fontStyle = 'italic'))

  a$xAxis(title = list(text = ""), max = max(dd$Year), min = min(dd$Year))

  a$lang( numericSymbols= NULL)
  a$yAxis(title = list(text = trad['y.title',lang]),
    gridLineColor = "#EFEFEF",
    labels = list(formatter = "#! function () {return this.value / 1000;} !#"))

  a$tooltip(formatter = "#! function() { return this.point.name; } !#", useHTML = T ,
    borderWidth = 2, style = list(padding = 4))


  hChart.html <- tempfile("hChart_area")
  a$save(hChart.html)
  # Convert highcharts-rCharts html chart into a responsive one
  hChart2responsiveHTML(hChart.html, output.html = output.html,
    h2 = trad['title',lang], descr = trad['descr',lang],
    source = paste0(trad["source",lang], ": ",
      htmlLink("http://www.wri.org/resources/data-sets/cait-historical-emissions-data-countries-us-states-unfccc",
      trad["sourceName",lang])),
    h3 = "",
    author = paste0("Duc-Quang Nguyen | ",
      htmlLink("http://www.swissinfo.ch", "swissinfo.ch")))

}









