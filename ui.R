library(shiny)
library(DT)

shinyUI(navbarPage("Water Demand Forecast", fluid = FALSE, inverse = TRUE,  header = includeCSS("www/style.css"),
	#### IMPORT ####
	tabPanel("Import", icon = icon('file-upload'), sidebarLayout(
		sidebarPanel(
			fileInput("file", "Choose a CSV or Excel File", accept = c("text/csv", ".csv", "text/plain",
				"application/vnd.ms-excel",
				"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
			),
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
			helpText("Incluir texto explicativo?"),
			fluidRow(
				column(6, selectInput('data_column', "Column", c('Import a file first' = 'V1'))),
				column(6, selectInput('data_frequency', 'Frequency',  c("Monthly" = 12,
																		"Quarterly" = 4,
																		"Semiannual" = 2,
																		"Annual" = 1,
																		"Daily" = 365,
																		"Custom" = -1)
				))
			),
			conditionalPanel("input.data_frequency == -1", numericInput('data_frequency_custom', "Frequency", 12)),
			fluidRow(
				column(6, numericInput('data_year', 'Start Date', 1)),
				column(6, numericInput('data_period', 'Period Offset', 1))
			),
			checkboxInput('data_validation', 'Use last observations as validation data'),
			conditionalPanel("input.data_validation == true",
				numericInput('data_validation', '', 3, 1, 6)
			),
			textInput('data_label', 'y-axis label', '')
		),
		mainPanel(
			plotOutput('option_plot')
		)
	)),
	#### PRE ANALYSIS ####
	tabPanel('Pre-Analysis', icon = icon('chart-bar'), fluidPage(
		splitLayout(
			plotOutput('analy_acf', height = '300px'),
			plotOutput('analy_pacf', height = '300px')
		),
		plotOutput('analy_stl', height = '450px'),
		"INCLUIR DEMAIS TESTES"
	)),
	#### MODEL SELECTION ####
	tabPanel('Model', icon = icon('calculator'), tabsetPanel(type = 'pills',
		tabPanel('ETS',
			splitLayout(
				plotOutput('model_ets_qqplot', height = '300px'),
				plotOutput('model_ets_acf', height = '300px')
			),
			uiOutput('model_ets_summary')
		),
		tabPanel('ARIMA',
			splitLayout(
				plotOutput('model_arima_qqplot', height = '300px'),
				plotOutput('model_arima_acf', height = '300px')
			),
			uiOutput('model_arima_summary')
		)
	)),
	#### FORECAST ####
	tabPanel('Forecast', icon = icon('chart-line'), tabsetPanel(type = 'pills',
		tabPanel('ETS',
			plotOutput('fore_ets'),
			uiOutput('fore_ets_summary'),
			tableOutput('fore_ets_table')
		),
		tabPanel('ARIMA',
			plotOutput('fore_arima'),
			uiOutput('fore_arima_summary')
		)
	)),
	#### FOOTER ####
	footer = tags$footer(
		hr(),
		flowLayout(id = "cabecario",
			p(strong("Acknowledgments"), br(),  img(src="FAPESC.png", alt="FAPESC - Fundo de Amparo à Pesquisa e inovação do Estado de Santa Catarina"), # grant 2019TR594
												img(src="CNPQ.png", alt="Conselho Nacional de Desenvolvimento Científico e Tecnológico")), # grant 421062/2018-5
			p(strong("Authors"), br(), "MANFRIN, Danielle", br(),
										"HENNING, Elisa", br(),
										"KALBUSCH, Andreza", br(),
										"PETERSEN, César E.")
		)
	)
))
