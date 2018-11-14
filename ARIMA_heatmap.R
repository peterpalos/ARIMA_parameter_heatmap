
# co-author: Aron Hartvig

#install.packages("xts")
#install.packages("forecast")
#install.packages("tseries")
#install.packages("dplyr")
#install.packages("ggplot2")
library( forecast)
library( tseries)
library( xts)
library( dplyr)
library( ggplot2)



### 

# ARIMA p-q estimator function
dataGenerator <- function( leng = 1000){
  RawData <- as.xts( ts( arima.sim( n = leng, list( ar = c( 0.7, -0.5), ma = c( -0.2, 0.2, 0.5)))))
  model <- auto.arima( RawData)
  result <- model$arma[1:2]
  return( result)
}

# Run the function, rep(time series length, number of estimation)
list_of_p_q <- lapply( rep( 1000, 1000 ), dataGenerator )



### Formatting the dataset

# calculate all possible p-q combination
all_possible_pq <- expand.grid( p_value = 0:5, q_value = 0:5 )

# formatting the output of the dataGenerator function to a 2 column df from a list
p_q_data <- do.call(rbind,list_of_p_q) %>%
  as.data.frame()
colnames( p_q_data) <- c( "p_value", "q_value")

# count the frequency
merged <- p_q_data %>%
  group_by( p_value, q_value) %>%
  summarize( freq = n()) %>%
  
  # right join the estimated p-q pairs and all possible pairs
  right_join( all_possible_pq, by = c( "p_value", "q_value"))



### Plotting

heatmap_p_q <- ggplot( merged, aes( p_value, q_value)) +
  labs(title = "1000 long time series p-q estimation", x = "p value", y = "q value") +
  geom_tile ( aes ( fill = freq ), colour = "grey") + 
  scale_x_continuous(breaks = round( seq( 0, 5))) +
  scale_y_continuous(breaks = round( seq( 0, 5))) +
  scale_fill_gradient(low = "white",  high = "darkolivegreen", na.value = "white")

plot( heatmap_p_q )


