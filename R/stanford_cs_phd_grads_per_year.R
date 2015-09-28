#' Returns a plot of the count of Stanford CS PhD graduates per year. 
#' 
#' Returns a ggplot2 plot of Stanford CS PhD students per year. 
#' 
#' @return String 
#' @export
   
StanfordCSPhDGradsPerYear <- function(){
    data('df_1')
    ggplot(data = df_1, aes(x = EndYear, y = NumGrads)) + geom_line() +
        theme_bw() +
            xlab("Year of graduation from Stanford") +
                ylab("Number of graduates")
}
