require(shiny)
require(DT)

month = 1:12
names(month) = month.abb

shinyUI(navbarPage("Water Demand Forecast", fluid = FALSE, inverse = TRUE,  header = includeCSS("www/style.css"),
	#### IMPORT ####
	tabPanel("Import", icon = icon('file-upload'), sidebarLayout(
		sidebarPanel(
			fileInput("file", "Choose a CSV or Excel File", accept = c("text/csv", ".csv", "text/plain",
				"application/vnd.ms-excel",
				"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
			),
			conditionalPanel("!output.filetype", helpText("Or, instead, use the example data.")),
			conditionalPanel("output.filetype == 'csv'", ## CSV OPTIONS
				checkboxInput('csv_header', 'Use first row as Header', TRUE),
				selectInput('csv_separator', 'Separator', c(", (Comma)" = ",",
															"; (Semicolon)" = ";",
															"Space" = " ",
															"Tabulation" = "\t")
				),
				fluidRow(
					column(6, selectInput('csv_decimal', 'Decimal', c(". (Period)" = ".", ", (Comma)" = ","))),
					column(6, selectInput('csv_quote', 'Quote', c("\" (Double)" = "\"", "' (Single)" = "'")))
				),

			),
			conditionalPanel("output.filetype == 'xls' || output.filetype == 'xlsx'", ## EXCEL OPTIONS
				numericInput('excel_sheet', "Sheet", 1, 1)
			)
		),
		mainPanel(dataTableOutput('table'))
	)),
	#### OPTIONS ####
	tabPanel('Options', icon = icon('sliders-h'), sidebarLayout(
		sidebarPanel(
			selectInput('data_column', "Column", c('Example Data' = 'demand')),
			conditionalPanel("input.data_frequency == -1", numericInput('data_frequency_custom', "Frequency", 12)),
			fluidRow(
				column(6, numericInput('data_year', 'Start Date', 2013)),
				column(6, selectInput('data_period', 'Month', month))
			),
			fluidRow(
				column(6,checkboxInput('data_validation_cb', 'Use last observations as validation data', T)),
					   column(6,conditionalPanel("input.data_validation_cb == true",
				numericInput('data_validation', 'Amount of Observations', 6, 1, 12))
			))
			,
			textInput('data_label', 'y-axis label', 'Water Demand (m³)')
		),
		mainPanel(
			plotOutput('option_plot')
		)
	)),
	#### PRE ANALYSIS ####
	tabPanel('Pre-Analysis', icon = icon('chart-bar'), fluidPage(
		splitLayout(
			uiOutput('analy_summary'),
			plotOutput('analy_qqplot', height = '300px')
		),
		splitLayout(
			plotOutput('analy_acf', height = '300px'),
			plotOutput('analy_pacf', height = '300px')
		),
		plotOutput('analy_stl', height = '450px')
	)),
	#### MODEL SELECTION ####
	tabPanel('Model', icon = icon('calculator'), tabsetPanel(type = 'pills',
		tabPanel('ETS',
			splitLayout(
				uiOutput('model_ets_summary')
			),
			splitLayout(
				plotOutput('model_ets_qqplot', height = '300px'),
				plotOutput('model_ets_acf', height = '300px')
			)
		),
		tabPanel('ARIMA',
			splitLayout(uiOutput('model_arima_summary')),
			splitLayout(
				plotOutput('model_arima_qqplot', height = '300px'),
				plotOutput('model_arima_acf', height = '300px')
			)
		)
	)),
	#### FORECAST ####
	tabPanel('Forecast', icon = icon('chart-line'), tabsetPanel(type = 'pills',
		tabPanel('ETS',
			plotOutput('fore_ets'),
			tableOutput('fore_ets_table'),
			conditionalPanel("input.data_validation_cb == true",
							 splitLayout(
							 	uiOutput('fore_ets_val_summary'),
							 	plotOutput('fore_ets_val', height = '300px')
							 )
			)
		),
		tabPanel('ARIMA',
			plotOutput('fore_arima'),
			tableOutput('fore_arima_table'),
			conditionalPanel("input.data_validation_cb == true",
							 splitLayout(
							 	uiOutput('fore_arima_val_summary'),
							 	plotOutput('fore_arima_val', height = '300px')
							 )
			)
		)
	)),
	#### ABOUT ####
	tabPanel('About', icon = icon('info-circle'),fluidPage(
		fluidRow(
			column(8, offset = 2, wellPanel(
				p(strong('References:')),
				p(HTML("Hyndman, R. J., & Athanasopoulos, G. (2020). <i>Forecasting: principles and practice</i>. OTexts. <a href='https://otexts.com/fpp2/'>https://otexts.com/fpp3/</a>.")),
				p(HTML("Hyndman, R. J., Athanasopoulos, G., Bergmeir, C., Caceres, G., Chhay, L.,
					   O'Hara-Wild, M., Petropoulos, F., Razbash, S., Wang, E., Yasmeen, F. (2020).
					   <i>forecast: Forecasting functions for time series and linear models</i>.
					   R package version 8.12, <a href='http://pkg.robjhyndman.com/forecas'>http://pkg.robjhyndman.com/forecast</a>.")),
				p(HTML("The source-code for this application is available at:
					   <a href='https://github.com/cesarep/water-demand-forecast'>GitHub.</a>"))
			))
		)
	)),
	#### FOOTER ####
	footer = tags$footer(
		hr(),
		flowLayout(id = "cabecario",
			p(strong("Acknowledgments"), br(),  img(src="FAPESC.png", id='fapesc', alt="FAPESC - Fundo de Amparo à Pesquisa e inovação do Estado de Santa Catarina"), # grant 2019TR594
												img(src="CNPQ.png", id='cnpq', alt="Conselho Nacional de Desenvolvimento Científico e Tecnológico")), # grant 421062/2018-5
			p(strong("Authors"), br(), "PETERSEN, César E.", br(),
										"MANFRIN, Danielle", br(),
										"HENNING, Elisa", br(),
										"KALBUSCH, Andreza")
		)
	)
))
