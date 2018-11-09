
# co-author: Aron Hartvig

#install.packages( "plyr")
#install.packages( "xts")
#install.packages( "forecast")
#install.packages( "tseries")
#install.packages( "dplyr")
#install.packages( "ggplot2")
library( plyr)
library( forecast)
library( tseries)
library( xts)
library( dplyr)
library( ggplot2)



### 

# ARIMA p-q estimator function
dataGenerator <- function( leng = 1000 ){
  RawData <- as.xts( ts( arima.sim( n = leng, list( ar = c( 0.7, -0.5 ), ma = c( -0.2, 0.2, 0.5 ) ) ) ) )
  model <- auto.arima( RawData)
  result <- model[[7]][1:2]
  return( result )
}

# Run the function, rep(time series length, number of estimation)
list_of_p_q <- lapply( rep( 1000, 1000 ), dataGenerator )


### Formatting the dataset

# all possible p-q combination
all_possible_pq <- expand.grid( P_value = 0:5, Q_value = 0:5 )
all_possible_pq <- mutate(all_possible_pq, ID = paste(P_value, Q_value, sep = ""))

# formatting the output of the dataGenerator function
p_q_data <- as.data.frame( list_of_p_q )
p_q_data <- as.data.frame( t( p_q_data ) )

# count the frequency
p_q_data_freq <- ddply(p_q_data, .(p_q_data$V1, p_q_data$V2), nrow)
colnames( p_q_data_freq ) <- c( "P_value", "Q_value", "Dens")
p_q_data_freq <- mutate(p_q_data_freq, ID = paste(P_value, Q_value, sep = ""))

# right join the estimated p-q pairs and all possible pairs
merged <- merge(p_q_data_freq, all_possible_pq, by = "ID", all.y = T)
merged <- select(merged, 4:6)



### Plotting

heatmap_p_q <- ggplot( merged, aes( P_value.y, Q_value.y ) ) +
  labs(title = "1000 long time series p-q estimation", x = "p value", y = "q value") +
  geom_tile ( aes ( fill = Dens ), colour = "grey") + 
  scale_x_continuous(breaks = round( seq( 0, 5))) +
  scale_y_continuous(breaks = round( seq( 0, 5))) +
  scale_fill_gradient(low = "white",  high = "darkolivegreen", na.value = "white")

plot( heatmap_p_q )

