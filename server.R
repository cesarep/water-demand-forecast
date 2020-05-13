library(shiny)
library(DT)
library(readxl)
library(forecast)
library(ggplot2)
library(plotly)

theme_update(plot.title = element_text(hjust = 0.5, size = 16))

shinyServer(function(input, output, session) {

	#### IMPORT DATA TAB ####

	## Import file
	import = reactive({
		# req(input$file)

		filetype = tolower(tail(strsplit(input$file$name, "\\.")[[1]], 1))
		file = input$file$datapath

		output$filetype <- reactive({ filetype })
		outputOptions(output, "filetype", suspendWhenHidden = FALSE)

		if(filetype == 'csv') {
			data = read.csv(file, input$csv_header, input$csv_separator, input$csv_quote, input$csv_decimal)
		}
		if(filetype == 'xls' || filetype == 'xlsx'){
			data = read_excel(file, input$excel_sheet)
		}

		updateSelectInput(session, 'data_column', choices  = colnames(data))

		return(data)
	})

	import = function() data.frame(V1 = c(2098064,2131560,2155893,2148904,2139178,2092157,2000241,1986054,2086004,2002469,2207489,2068330,2213972,2400832,2162789,2182637,2074590,2044090,2058373,2038225,2048551,2074917,2216076,2115768,2223332,2200935,2124300,2096102,2137916,2130628,2001614,2108760,2112368,2061940,2116044,2181326,2259042,2280324,2231722,2231215,2178884,2059325,2076815,2097688,2153082,2130012,2224171,2209526,2345104,2314853,2294366,2154050,2194208,2191145,2212914,2215803,2282421,2244625,2243165,2317752,2332073,2333264,2295320))

	## Display file preview
	output$table <- renderDataTable({
		datatable(import(), rownames = FALSE)
	})

	#### DATA OPTIONS TAB ####

	time_series = debounce(reactive({
		req(import())
		data = import()[[input$data_column]]
		freq = as.numeric(ifelse(input$data_frequency != '-1', input$data_frequency, input$data_frequency_custom))
		start = c(input$data_year, input$data_period)
		time_s = ts(data, start = start, frequency = freq)
		return(tsclean(time_s))
	}), 1000)

	output$option_plot <- renderPlot({
		##ggtsdisplay(time_series())
		autoplot(time_series(), xlab = "Period", ylab= input$data_column)
	})

	#### DATA ANALYSIS ####

	output$analy_acf <- renderPlot({
		ggAcf(time_series(), main='ACF')
	})

	output$analy_pacf <- renderPlot({
		ggPacf(time_series(), main='PACF')
	})

	output$analy_stl <- renderPlot({
		autoplot(stl(time_series(),s.window='periodic'))
	})

	#### MODEL ####
	## ETS ##

	ets_model <- reactive({
		#req(time_series())
		return(ets(time_series()))
	})

	output$model_ets_res <- renderPlot({
		checkresiduals(ets_model())
	})

	## ARIMA ##

	arima_model <- reactive({
		#req(time_series())
		return(auto.arima(time_series()))
	})

	output$model_arima_res <- renderPlot({
		checkresiduals(arima_model())
	})

	#### FORECAST ####
	## ETS

	ets_forecast <- reactive({
		return(forecast(ets_model() , h=3))
	})

	output$fore_ets <- renderPlot({
		autoplot(ets_forecast() ,xlab="Period") + autolayer(fitted(ets_forecast()),color="blue")
	})

	## ARIMA

	arima_forecast <- reactive({
		return(forecast(arima_model() , h=3))
	})

	output$fore_arima <- renderPlot({
		autoplot(arima_forecast() ,xlab="Period") + autolayer(fitted(arima_forecast()),color="blue")
	})

})
