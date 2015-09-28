#' Returns a plot of fraction of Stanford CS PhD grads choosing various career options over time.  
#' 
#' Returns a ggplot2 plot of Stanford CS PhD grad occupational choices
#' 
#' @return String 
#' @export
   
StanfordCSPhDGradsOccChoicePerYear <- function(){
    data('df_2')

    ggplot(data = df_2, aes(x = year, y = value, linetype = type)) + geom_line() +
        theme_bw() +
            xlab("Year of graduation from Stanford") +
                ylab("Fraction fo total graduation pool")
    
}
