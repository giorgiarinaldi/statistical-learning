#install.packages("readxl")
#install.packages("visdat")
#install.packages("ggplot2")
#install.packages("cartogram")
#install.packages("corrplot")
#install.packages("car")
#install.packages("glmnet")
#install.packages("ggmap")
#install.packages("gridExtra")
#install.packages("cowplot")
#install.packages("leaps")
#install.packages("dplyr")
# install.packages("caret")
#install.packages("glmnet")
library(caret)
library(dplyr)
library(glmnet)
library(readxl)
library(visdat)
library(ggplot2)
library(e1071)
library(cartogram)
library(corrplot)
library(car)
library(ggmap)
library(gridExtra)
library(cowplot)
library(leaps)
library(glmnet)

# Setting working directory
#getwd()
#setwd(dir = "/Users/andreamarinelli/Desktop/Progetto\ Statistica/California\ House\ Price")
#getwd()

########################################################
#                 1. Obtaining data
########################################################

data <- read.csv("california_housing.csv")
initial_data <- data

########################################################
#           2. Cleaning and filtering data
########################################################

# Esplorazione dataset per verificare che i valori siano corretti

dim(data)
anyDuplicated(data) # Output = 0 -> non ci sono duplicati di righe
head(data)
tail(data)
str(data)
summary(data)
anyNA(data)

# separiamo colonne numeriche da categoriche:

num_col <- sapply(data, is.numeric)
num_col
df_num <- data[, num_col]
anyNA(df_num)

col_with_na <- colSums(is.na(df_num)) > 0
col_with_na

vis_miss(df_num)

rows_with_na <- is.na(data[, 5]) # Estraiamo le righe con NA

ggplot() + 
  geom_point(data=data[!rows_with_na, ] , mapping = aes(x = longitude, y = latitude), color="blue") +
  geom_point(data=data[rows_with_na, ] , mapping = aes(x = longitude, y = latitude), color="red")

data <- data[!rows_with_na, ] # Eliminiamo le righe con NA
dim(data)
anyNA(data) # abbiamo levato NA dal dataset controllando solo col_numeriche


# check colonna categorica:

cat_colums <- !num_col
cat_colums[cat_colums == TRUE] # printa nome della colonna non numerica


# check valori assunti dal vettore per vedere se NA sono scritti in maniera diversa da "na"

unique(data$ocean_proximity) # Abbiamo controllato che non ci siano valori diversi da quelli assumibili

cleaned_data <- data # Salviamo una copia del dataset


########################################################
#           3. Exploratory Data Analysis (EDA)
########################################################

# Creazione di nuove variabili

# vogliamo osservare come varia median_house_value in funzione di longitudine e latitudine
LA_long <- -118.24
LA_lat <- 34.05
SF_long <- -122.43
SF_lat <- 37.77

latitude_mc <- c(LA_lat, SF_lat)
longitude_mc<- c(LA_long, SF_long)
labels <- c("LA", "SF")
point <- as.data.frame(cbind(latitude_mc, longitude_mc))
rownames(point) = labels

ggplot() +
  geom_point(data = data, mapping = aes(x = data$longitude, y = data$latitude, color = data$median_house_value/1000))+
  geom_point(data = point , aes(x = longitude_mc, y = latitude_mc), color = "red", size = 8)+
  geom_text(data = point , aes(x = longitude_mc, y = latitude_mc, label = labels), color = "white", size = 4)+
  labs(x = "longitude", y = "latitude", title = "Median house value in each block")


# abbiamo osservato che con l'aumentare della vicinanza alle due principali citta LA e SF il valore aumentava
# quindi abbiamo creato una nuova variabile utilizzando lat e long: criterio data$distance maggiori centri
E_dist <- function(x, y, x_1, y_1) {
  sqrt(sum((x_1 - x)**2 , (y_1 - y)**2 ))
}


dist_LA <- mapply(E_dist, data$longitude, data$latitude, LA_long, LA_lat)
dist_SF <- mapply(E_dist, data$longitude, data$latitude, SF_long, SF_lat)

n <- dim(data)[1]  # Lunghezza del vettore desiderata
distance <- numeric(n)  # Creazione del vettore di lunghezza n

# Assegnazione di valori casuali al vettore
for (i in 1:n) {
  if (dist_LA[i] >= dist_SF[i]){
    distance[i] <- dist_SF[i]
  }
  else{
    distance[i] <- dist_LA[i]
  }
}

# Aggiungiamo la variabile distance al dataframe
data <- cbind(distance, data)

med_dist <- median(data$distance)
near_cities <- data$distance <= med_dist

# plot densities
dens_dist <- ggplot(data, aes(x = median_house_value / 1000, color = near_cities, fill = near_cities)) +
  geom_density(alpha = 0.5)+
  labs(x ="Median House Value (in 1000 $)", y="Density")+
  ggtitle("Median house values vs nearess from main cities")

# distance histogram
dist_histogram <- ggplot(data, aes(x = distance)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(distance)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(distance)), color="red",linetype="dashed", size=1) +
  labs(x ="distance", y="Count")+
  ggtitle("Histogram of the distance from main cities (LA-SF)")

plot_grid(dist_histogram, dens_dist, nrow = 1)


# distance boxplot
ggplot(data, aes(x = data$distance ))+
  geom_boxplot(fill = "pink")+
  geom_point(aes(x= mean(data$distance), y=0), color="blue")+
  labs(x ="distance", y = " ")+
  ggtitle("Boxplot of distance")

# Togliamo le colonne longitude e latitude dal dataframe
data <- data[, -c(2, 3)]

# Verifichiamo la correlazione tra le variabili
data2 <- data[sapply(data, is.numeric)]
corrplot(cor(data2), method = "shade", type = "upper", addCoef.col = "white", tl.col="black", tl.srt=45, diag = FALSE)

# households ha cor elevata con rooms, bedrooms, pop.: creo interazioni fra  households e rooms e bedrooms
bedrooms_ph <- data$total_bedrooms/data$households
rooms_ph <- data$total_rooms/data$households

# Aggiorniamo il dataframe
data <- cbind(distance, bedrooms_ph, rooms_ph, data[, -c(1,3,4,6)])
rm(distance)

data3 <- data[sapply(data, is.numeric)]
corrplot(cor(data3), method = "shade", type = "upper", addCoef.col = "black", tl.col="black", tl.srt=45, diag = FALSE)



# plot median_house_values histogram
mhv_histogram <- ggplot(data, aes(x = median_house_value/1000)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(median_house_value/1000)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(median_house_value/1000)), color="red",linetype="dashed", size=1) +
  labs(x ="Median House Value (in 1000 $)", y="Count")+
  ggtitle("Histogram of Median house values")

# median_house_values boxplot
mhv_boxplot <- ggplot(data, aes(x = median_house_value/1000)) +
  geom_boxplot(fill = "pink") +
  geom_point(aes(x= mean(median_house_value)/1000, y=0), color="blue")+
  labs(x ="Median House Value (in 1000 $)", y = " ")+
  ggtitle("Boxplot of Median house values")

plot_grid(mhv_histogram, mhv_boxplot, nrow = 1)


# plot housing_median_age histogram
hma_histogram <- ggplot(data, aes(x = housing_median_age)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(housing_median_age)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(housing_median_age)), color="red",linetype="dashed", size=1) +
  labs(x ="housing_median_age", y="Count")+
  ggtitle("Histogram of Housing median age")

# housing_median_age boxplot
hma_boxplot <- ggplot(data, aes(x = housing_median_age)) +
  geom_boxplot(fill = "pink") +
  labs(x ="housing_median_age", y = " ")+
  geom_point(aes(x= mean(housing_median_age), y=0), color="blue")+
  ggtitle("Boxplot of Housing median age")

plot_grid(hma_histogram, hma_boxplot, nrow = 1)



# rooms_ph histogram
rph_histogram <- ggplot(data, aes(x = rooms_ph)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(rooms_ph)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(rooms_ph)), color="red",linetype="dashed", size=1) +
  labs(x ="rooms_ph", y="Count")+
  ggtitle("Histogram of Rooms per household")

# rooms_ph boxplot
rph_boxplot <- ggplot(data, aes(x = rooms_ph)) +
  geom_boxplot(fill = "pink") +
  geom_point(aes(x= mean(rooms_ph), y=0), color="blue")+
  labs(x ="rooms_ph", y = " ")+
  ggtitle("Boxplot of Rooms per household")

plot_grid(rph_histogram, rph_boxplot, nrow = 1)



# bedrooms_ph histogram
bph_histogram <- ggplot(data, aes(x = bedrooms_ph)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(bedrooms_ph)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(bedrooms_ph)), color="red",linetype="dashed", size=1) +
  labs(x ="bedrooms_ph", y="Count")+
  ggtitle("Histogram of Bedrooms per household")

# bedrooms_ph boxplot
bph_boxplot <- ggplot(data, aes(x = bedrooms_ph)) +
  geom_boxplot(fill = "pink") +
  geom_point(aes(x= mean(bedrooms_ph), y=0), color="blue")+
  labs(x ="bedrooms_ph", y = " ")+
  ggtitle("Boxplot of Bedrooms per household")

plot_grid(bph_histogram, bph_boxplot, nrow = 1)



# plot population histogram
pop_histogram <- ggplot(data, aes(x = population)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(population)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(population)), color="red",linetype="dashed", size=1) +
  labs(x ="population", y="Count")+
  ggtitle("Histogram of Population")

# population boxplot
pop_boxplot <- ggplot(data, aes(x = population)) +
  geom_boxplot(fill = "pink") +
  geom_point(aes(x= mean(population), y=0), color="blue")+
  labs(x ="population", y = " ")+
  ggtitle("Boxplot of Population")

plot_grid(pop_histogram, pop_boxplot, nrow = 1)



# plot median_income
mi_histogram <- ggplot(data, aes(x = median_income)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(median_income)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(median_income)), color="red",linetype="dashed", size=1) +
  labs(x ="median_income", y="Count")+
  ggtitle("Histogram of Median income")

# boxplot median_income
mi_boxplot <- ggplot(data, aes(x = median_income)) +
  geom_boxplot(fill = "pink") +
  geom_point(aes(x= mean(median_income), y=0), color="blue")+
  labs(x ="median_income", y = " ")+
  ggtitle("Boxplot of Median income")

plot_grid(mi_histogram, mi_boxplot, nrow = 1)



# Vediamo le aree di ocean proximity

# plot coordinate
ggplot() + 
  geom_point(data=cleaned_data[cleaned_data$ocean_proximity == "<1H OCEAN", ] , mapping = aes(x = longitude, y = latitude), color="red") +
  geom_point(data=cleaned_data[cleaned_data$ocean_proximity == "NEAR OCEAN", ] , mapping = aes(x = longitude, y = latitude), color="blue") + 
  geom_point(data=cleaned_data[cleaned_data$ocean_proximity == "NEAR BAY", ] , mapping = aes(x = longitude, y = latitude), color="green") + 
  geom_point(data=cleaned_data[cleaned_data$ocean_proximity == "INLAND", ] , mapping = aes(x = longitude, y = latitude), color="orange") + 
  geom_point(data=cleaned_data[cleaned_data$ocean_proximity == "ISLAND", ] , mapping = aes(x = longitude, y = latitude), color="cyan")+
  ggtitle("Ocean proximity area")

# barplot ocean_proximity
bar_op <- ggplot(data, aes(x = ocean_proximity)) +
  geom_bar(color="black", fill="pink")+
  ggtitle("Ocean Proximity")

# plot densities
dens_op <- ggplot(data, aes(x = median_house_value / 1000, color = ocean_proximity, fill = ocean_proximity)) +
  geom_density(alpha = 0.5)+
  labs(x ="median_house_value (in 1000$)", y = "density")+
  ggtitle("Median house value by proximity to the sea")

plot_grid(bar_op, dens_op, nrow = 1)


########################################################
#                Dealing with Outliers
########################################################

# Outliers in bedrooms_ph
outliers_b <- data$bedrooms_ph > IQR(data$bedrooms_ph) * 1.5 + quantile(bedrooms_ph, 0.75) | data$bedrooms_ph < quantile(bedrooms_ph, 0.25) - IQR(data$bedrooms_ph) * 1.5

# Outliers in rooms_ph
outliers_r <- data$rooms_ph > IQR(data$rooms_ph) * 1.5 + quantile(data$rooms_ph, 0.75) | data$rooms_ph < quantile(data$rooms_ph, 0.25) - IQR(data$rooms_ph) * 1.5

# Outliers in data$housing_median_age
outliers_hma <- data$housing_median_age > IQR(data$housing_median_age) * 1.5 + quantile(data$housing_median_age, 0.75) | data$housing_median_age < quantile(data$housing_median_age, 0.25) - IQR(data$housing_median_age) * 1.5

# Outliers in population
outliers_p <- data$population > IQR(data$population) * 1.5 + quantile(data$population, 0.75) | data$population < quantile(data$population, 0.25) - IQR(data$population) * 1.5

# Outliers in median_income
outliers_mi <- data$median_income > IQR(data$median_income) * 1.5 + quantile(data$median_income, 0.75) | data$median_income < quantile(data$median_income, 0.25) - IQR(data$median_income) * 1.5

# Outliers in median_house_value
outliers_mhv <- data$median_house_value > IQR(data$median_house_value) * 1.5 + quantile(data$median_house_value, 0.75) | data$median_house_value < quantile(data$median_house_value, 0.25) - IQR(data$median_house_value) * 1.5

# Rimuovo gli outliers e costruisco il dataset finale
data <- data[!(outliers_b | outliers_r | outliers_hma | outliers_p | outliers_mi | outliers_mhv), ]

# Salvo una copia del dataset finale
final_data <- data 

anyNA(data)
summary(data)

data2 <- data[sapply(data, is.numeric)]
class(data2)
cor(data2)
corrplot(cor(data2), method = "shade", type = "upper", addCoef.col = "black", tl.col="black", tl.srt=45, diag = FALSE)



# plot median_house_values histogram
mhv_histogram <- ggplot(data, aes(x = median_house_value/1000)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(median_house_value/1000)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(median_house_value/1000)), color="red",linetype="dashed", size=1) +
  labs(x ="Median House Value (in 1000 $)", y="Count")+
  ggtitle("Histogram of Median house values")

# median_house_values boxplot
mhv_boxplot <- ggplot(data, aes(x = median_house_value/1000)) +
  geom_boxplot(fill = "pink") +
  geom_point(aes(x= mean(median_house_value)/1000, y=0), color="blue")+
  labs(x ="median_house_values", y = " ")+
  ggtitle("Boxplot of Median house values")

plot_grid(mhv_histogram, mhv_boxplot, nrow = 1)



# plot housing_median_age histogram
hma_histogram <- ggplot(data, aes(x = housing_median_age)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(housing_median_age)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(housing_median_age)), color="red",linetype="dashed", size=1) +
  labs(x ="housing median age", y="Count")+
  ggtitle("Histogram of House median age")

# housing_median_age boxplot
hma_boxplot <- ggplot(data, aes(x = housing_median_age)) +
  geom_boxplot(fill = "pink") +
  geom_point(aes(x= mean(housing_median_age), y=0), color="blue")+
  labs(x ="housing_median_age", y = " ")+
  ggtitle("Boxplot of House median age")

plot_grid(hma_histogram, hma_boxplot, nrow = 1)



# plot rooms_ph histogram
rph_histogram <- ggplot(data, aes(x = rooms_ph)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(rooms_ph)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(rooms_ph)), color="red",linetype="dashed", size=1) +
  labs(x ="rooms_ph", y="Count")+
  ggtitle("Histogram of Rooms per household")

# rooms_ph boxplot
rph_boxplot <- ggplot(data, aes(x = rooms_ph)) +
  geom_boxplot(fill = "pink") +
  geom_point(aes(x= mean(rooms_ph), y=0), color="blue")+
  labs(x ="rooms_ph", y = " ")+
  ggtitle("Boxplot of Rooms per household")

plot_grid(rph_histogram, rph_boxplot, nrow = 1)



# plot bedrooms_ph histogram
bph_histogram <- ggplot(data, aes(x = bedrooms_ph)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(bedrooms_ph)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(bedrooms_ph)), color="red",linetype="dashed", size=1) +
  labs(x ="bedrooms_ph", y="Count")+
  ggtitle("Histogram of Bedrooms per household")

# bedrooms_ph boxplot
bph_boxplot <- ggplot(data, aes(x = bedrooms_ph)) +
  geom_boxplot(fill = "pink") +
  geom_point(aes(x= mean(bedrooms_ph), y=0), color="blue")+
  labs(x ="bedrooms_ph", y = " ")+
  ggtitle("Boxplot of Bedrooms per household")

plot_grid(bph_histogram, bph_boxplot, nrow = 1)



# plot population histogram
pop_histogram <- ggplot(data, aes(x = population)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(population)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(population)), color="red",linetype="dashed", size=1) +
  labs(x ="population", y="Count")+
  ggtitle("Histogram of Population")

# population boxplot
pop_boxplot <- ggplot(data, aes(x = population)) +
  geom_boxplot(fill = "pink") +
  geom_point(aes(x= mean(population), y=0), color="blue")+
  labs(x ="population", y = " ")+
  ggtitle("Boxplot of Population")

plot_grid(pop_histogram, pop_boxplot, nrow = 1)



# plot median_income
mi_histogram <- ggplot(data, aes(x = median_income)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(median_income)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(median_income)), color="red",linetype="dashed", size=1) +
  labs(x ="median_income", y="Count")+
  ggtitle("Histogram of Median income")

# boxplot median_income
mi_boxplot <- ggplot(data, aes(x = median_income)) +
  geom_boxplot(fill = "pink") +
  geom_point(aes(x= mean(median_income), y=0), color="blue")+
  labs(x ="median_income", y = " ")+
  ggtitle("Boxplot of Median income")

plot_grid(mi_histogram, mi_boxplot, nrow = 1)



# Dividiamo il dataframe in due gruppi in base alla variabile distanza
# Usando come threshold la distanza mediana
med_dist <- median(data$distance)
near_cities <- data$distance <= med_dist

# plot densities
dens_dist <- ggplot(data, aes(x = median_house_value / 1000, color = near_cities, fill = near_cities)) +
  geom_density(alpha = 0.5) +
  labs(x ="distance", y = "density")+
  ggtitle("Median house values vs nearess from main cities")

# distance histogram
dist_histogram <- ggplot(data, aes(x = distance)) +
  geom_histogram(aes(y =..density..), fill="pink", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "blue", size =0.8)+
  geom_vline(aes(xintercept = mean(distance)), color="blue",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(distance)), color="red",linetype="dashed", size=1) +
  labs(x ="distance", y="Count")+
  ggtitle("Histogram of the distance from main cities (LA-SF)")

plot_grid(dist_histogram, dens_dist, nrow = 1)



# boxplot ocean_proximity (without outliers)
box_op <- ggplot(data, aes(x = ocean_proximity, y = median_house_value / 1000)) +
  geom_boxplot(fill="pink") + 
  stat_summary(fun=mean, geom="point", color="blue")+
  labs(x ="ocean_proximity", y = "median_house_values (in 1000$)")+
  ggtitle("Boxplot of Median house value by proximity to the sea")

# plot densities
dens_op <- ggplot(data, aes(x = median_house_value / 1000, color = ocean_proximity, fill = ocean_proximity)) +
  geom_density(alpha = 0.5)+
  labs(x ="median_house_values (in 1000$)", y = "density")+
  ggtitle("Median house value by proximity to the sea")

plot_grid(box_op, dens_op, nrow = 1)



g1 <- ggplot(data, aes(x = median_income, y = median_house_value / 1000)) + 
  geom_jitter() +
  labs(y ="median_house_values (in 1000$)")+
  geom_smooth(method = lm, formula = y~x)

g2 <- ggplot(data, aes(x = bedrooms_ph, y = median_house_value / 1000)) + 
  geom_jitter() +
  labs(y ="median_house_values (in 1000$)")+
  geom_smooth(method = lm, formula = y~x)

g3 <- ggplot(data, aes(x = rooms_ph, y = median_house_value / 1000)) + 
  geom_jitter() +
  labs(y ="median_house_values (in 1000$)")+
  geom_smooth(method = lm, formula = y~x)

g4 <- ggplot(data, aes(x = housing_median_age, y = median_house_value / 1000)) + 
  geom_jitter() +
  labs(y ="median_house_values (in 1000$)")+
  geom_smooth(method = lm, formula = y~x)

g5 <- ggplot(data, aes(x = population, y = median_house_value / 1000)) + 
  geom_jitter() +
  labs(y ="median_house_values (in 1000$)")+
  geom_smooth(method = lm, formula = y~x)

g6 <- ggplot(data, aes(x = distance, y = median_house_value / 1000)) + 
  geom_jitter() +
  labs(y ="median_house_values (in 1000$)")+
  geom_smooth(method = lm, formula = y~x)

plot_grid(g1, g2, g3, g4, g5, g6, nrow = 2)


########################################################
#                4. Regression Model
########################################################

data$ocean_proximity[data$ocean_proximity == "ISLAND"] <- "NEAR OCEAN"

# Divido per 1000 la variabile median_house_value e sostituisco nel dataset
data$median_house_value <- data$median_house_value/1000

# Tratto le variabili categoriche creando una dummy variable for each categori of ocean_proximity
unique(data$ocean_proximity)

NEAR_BAY <- ifelse(data$ocean_proximity == 'NEAR BAY', 1, 0)
Min_1H_OCEAN <- ifelse(data$ocean_proximity == '<1H OCEAN', 1, 0)
INLAND <- ifelse(data$ocean_proximity == 'INLAND', 1, 0)
NEAR_OCEAN <- ifelse(data$ocean_proximity == 'NEAR OCEAN', 1, 0)

dataset_with_dummy <- cbind(data[,-c(7)], INLAND, Min_1H_OCEAN, NEAR_OCEAN)

# Provo il modello completo
model <- lm(median_house_value ~ ., data = dataset_with_dummy)

summary(model)

beta_hat <- coefficients(model)
beta_hat

R2 <- summary(model)$r.squared
R2

adjusted_R2 <- summary(model)$adj.r.squared
adjusted_R2

RSE <- summary(model)$sigma
RSE

vif(model) 

confint(model)

par(mfrow=c(2, 2))
plot(model) #osservo che ho problemi nel QQ-plot
par(mfrow=c(1, 1)) 

# Subset selection
regfit.full <- regsubsets(median_house_value~. ,data=dataset_with_dummy, nvmax=9)
summary(regfit.full)

# Uso AIC per vedere le migliori variabili da utilizzare
library(MASS)
AIC <- stepAIC(model, direction ='backward')
AIC
summary(AIC)
BIC <- step(model, direction = "backward", k = log(dim(dataset_with_dummy)[1]))
BIC
summary(BIC)
AIC(model)



# Entrambe mi suggeriscono che il modello con tutte le variabili è il migliore possibile, 
# ma vediamo che ci sono problemi nei grafici, perciò a trasformare la y

# osservo le relazioni tra le features e log(y)
g1_log <- ggplot(data, aes(x = median_income, y = log(median_house_value))) + 
  geom_jitter() +
  labs(y ="median_house_values (in 1000$)")+
  geom_smooth(method = lm, formula = y~x)

g2_log <- ggplot(data, aes(x = bedrooms_ph, y =log(median_house_value))) + 
  geom_jitter() +
  labs(y ="median_house_values (in 1000$)")+
  geom_smooth(method = lm, formula = y~x)

g3_log <- ggplot(data, aes(x = rooms_ph, y = log(median_house_value))) + 
  geom_jitter() +
  labs(y ="median_house_values (in 1000$)")+
  geom_smooth(method = lm, formula = y~x)

g4_log <- ggplot(data, aes(x = housing_median_age, y =log(median_house_value))) + 
  geom_jitter() +
  labs(y ="median_house_values (in 1000$)")+
  geom_smooth(method = lm, formula = y~x)

g5_log <- ggplot(data, aes(x = population, y =log(median_house_value))) + 
  geom_jitter() +
  labs(y ="median_house_values (in 1000$)")+
  geom_smooth(method = lm, formula = y~x)

g6_log <- ggplot(data, aes(x = distance, y = log(median_house_value))) + 
  geom_jitter() +
  labs(y ="median_house_values (in 1000$)")+
  geom_smooth(method = lm, formula = y~x)

plot_grid(g1_log, g2_log, g3_log, g4_log, g5_log, g6_log, nrow = 2)



# Provo il modello completo ma su log(median_house_value) senza la dummy variable problematica NEAR_OCEAN
model_log <- lm(log(median_house_value) ~ . , data = dataset_with_dummy)

summary(model_log)

beta_hat_log <- coefficients(model_log)
beta_hat_log

R2_log <- summary(model_log)$r.squared
R2_log

adjusted_R2_log <- summary(model_log)$adj.r.squared
adjusted_R2_log

RSE_log <- summary(model_log)$sigma
RSE_log

vif(model_log)

confint(model_log)

par(mfrow=c(2, 2))
plot(model_log) 
par(mfrow=c(1, 1)) 

# Subset selection
regfit.full_log <- regsubsets(log(median_house_value)~. ,data = dataset_with_dummy, nvmax=9)
summary(regfit.full_log)
regfit.full_log

# Uso AIC per vedere se togliere una variabile o meno
AIC_log <- stepAIC(model_log, direction ='backward')
AIC_log
summary(AIC_log) #noto che devo togliere le variabili housing_median_age e Min_1H_OCEAN   
BIC_log <- step(model_log, direction = "backward", k = log(dim(dataset_with_dummy)[1]))
BIC_log
summary(BIC_log)
AIC(model_log)
# Sia AIC che BIC mi suggeriscono di togliere Min_1H_OCEAN e housing_median_age.

# le tolgo entrambe

# aggiorno il modello senza housing_median_age e Min_1H_OCEAN
model_log_2 <- lm(log(median_house_value) ~ . -Min_1H_OCEAN -housing_median_age, data = dataset_with_dummy)

summary(model_log_2)

beta_hat_log_2 <- coefficients(model_log_2)
beta_hat_log_2

R2_log_2 <- summary(model_log_2)$r.squared
R2_log_2

adjusted_R2_log_2 <- summary(model_log_2)$adj.r.squared
adjusted_R2_log_2

RSE_log_2 <- summary(model_log_2)$sigma
RSE_log_2

vif(model_log_2)

confint(model_log_2)

par(mfrow=c(2, 2))
plot(model_log_2) 
par(mfrow=c(1, 1))

# Subset selection
regfit.full_log_2 <- regsubsets(log(median_house_value)~ . -Min_1H_OCEAN -housing_median_age ,data=dataset_with_dummy, nvmax=7)
summary(regfit.full_log_2)
regfit.full_log_2

# Uso AIC per vedere se togliere una variabile o meno
AIC_log_2 <- stepAIC(model_log_2, direction ='backward')
AIC_log_2
summary(AIC_log_2) 
BIC_log_2 <- step(model_log_2, direction = "backward", k = log(dim(dataset_with_dummy)[1]))
BIC_log_2
summary(BIC_log_2)
AIC(model_log_2)

# Entrambi mi dicono che sono arrivato al modello migliore possibile per quella y

# Compute ANOVA to confrontare i due modelli 
anova(model_log, model_log_2)


# cerco ora High Leverage Points

# In R, il termine "hatvalues" si riferisce alla funzione "hatvalues()" che viene
# utilizzata nell'analisi dei dati per calcolare i valori di "hat" o i valori cappello. 
# Questa funzione viene spesso utilizzata in analisi della regressione per valutare l'influenza
# di ciascuna osservazione sui risultati del modello.

# I valori di "hat" rappresentano la stima della distanza di ciascuna osservazione dal centro del 
# dataset. In altre parole, misurano quanto ciascuna osservazione influisce sui risultati del modello.
# I valori cappello sono utili per identificare le osservazioni che hanno un'influenza elevata o anomala
# sul modello di regressione.

# La funzione "hatvalues()" restituisce un vettore di valori cappello corrispondenti a ciascuna 
# osservazione nel dataset. I valori cappello sono compresi tra 0 e 1, dove un valore elevato indica
# un'influenza maggiore dell'osservazione sul modello.

hat_values <- hatvalues(model_log_2)
plot(hat_values, rstandard(model_log_2), xlab="Leverage", ylab = "Standardized Residuals")
p <- dim(dataset_with_dummy)[2]-1
p
n <- dim(dataset_with_dummy)[1]
n
abline(v=(p+1)/n, col = "red")
abline(v=3*(p+1)/n, col = "blue")
high_leverage_points <- names(hat_values[hat_values>3*(p+1)/n])
high_leverage_points
dataset_with_dummy[high_leverage_points, c(1,2,3,4,5,6,7,8,9,10)]
number_high_leverage_points <-length(high_leverage_points)
number_high_leverage_points

data_w_hlp <- dataset_with_dummy[!(rownames(dataset_with_dummy) %in% high_leverage_points),]


# costruisco ora il modello without high leverage points

model_w_hlp <- lm(log(median_house_value)~. -housing_median_age -Min_1H_OCEAN, data = data_w_hlp)
summary(model_w_hlp)

beta_hat_w_hlp <- coefficients(model_w_hlp)
beta_hat_w_hlp

R2_w_hlp <- summary(model_w_hlp)$r.squared
R2_w_hlp

adjusted_R2_w_hlp <- summary(model_w_hlp)$adj.r.squared
adjusted_R2_w_hlp

RSE_w_hlp <- summary(model_w_hlp)$sigma
RSE_w_hlp

vif(model_w_hlp)

confint(model_w_hlp)


par(mfrow=c(2, 2))
plot(model_w_hlp) 
par(mfrow=c(1, 1)) 

# Subset selection
regfit.full_w_hlp <- regsubsets(log(median_house_value)~.-housing_median_age -Min_1H_OCEAN, data = data_w_hlp, nvmax=7)
summary(regfit.full_w_hlp)
regfit.full_w_hlp

# Uso AIC per vedere se togliere una variabile o meno
AIC_b_w_hlp <- stepAIC(model_w_hlp, direction ='backward')
AIC_b_w_hlp
summary(AIC_b_w_hlp) #noto che non devo togliere alcuna variabile 
BIC_b_w_hlp <- step(model_w_hlp, direction = "backward", k = log(dim(data_w_hlp)[1]))
BIC_b_w_hlp
summary(BIC_b_w_hlp)
AIC(AIC_b_w_hlp)

# entrambe mi dicono che vanno bene tutte le variabili fin qui selezionate,
# quindi decido che questo è il mio modello finale

model_final <- lm(log(median_house_value)~.-housing_median_age -Min_1H_OCEAN, data = data_w_hlp)
summary(model_final)

# Compute ANOVA 
aov(model_w_hlp)
aov(model_log_2)

# definisco il dataset finale senza high Leverage Points e dummy variables (cioè con ocean_proximity)
dataset_last <- data[!(rownames(dataset_with_dummy) %in% high_leverage_points),]


########################################################
#                Model Evaluation                  
#########################################################

# Split the data into train and test and see their error(MSE)

# setting seed to generate a reproducible random sampling
set.seed(569)

# Define training subset
n_train = floor(dim(data_w_hlp)[1]*0.8) # 13243 samples for test(80 % of the data)
i_train <- sample( 1:n, size = n_train, replace = FALSE) # indexes of training samples

# linear model with training data
model_train <- lm(log(median_house_value) ~ . -Min_1H_OCEAN -housing_median_age, data = data_w_hlp , subset = i_train)

# Predict the demand of the test data based on fitted model
y_pred <- predict(model_train, newdata = data_w_hlp[-i_train, ])

# mean squared error on test data
MSE <- mean((log(data_w_hlp$median_house_value[-i_train])-y_pred)^2)
MSE

# MSE su log(y) che implica MSE=exp(MSE) su median_house_value/1000 
MSE_c = exp(MSE) * 1000
MSE_c


########################################################
#                k-Fold Cross-Validation                  
########################################################

# Using k-Fold Cross-Validation to assess the goodness of the model

# Define the value of k
k=10

# Split the subsets of data_w_hlp
folds_10 <- sample(1:k, nrow(data_w_hlp), replace=TRUE)
table(folds_10)

# Make the error's vector
cv.errors_10 <- matrix(NA,k,1)
colnames(cv.errors_10) <- "Errors"

# Compute 10-fold Cross Validation and compute the error
for(j in 1:k){
  best.fit <- lm(log(median_house_value)~.-housing_median_age -Min_1H_OCEAN, data=data_w_hlp[folds_10!=j,])
  test.mat <- model.matrix(log(median_house_value)~.-housing_median_age -Min_1H_OCEAN, data = data_w_hlp[folds_10==j,])
  y_pred_k <- predict(model_train, newdata = data_w_hlp[folds_10==j,])
  cv.errors_10[j,1] <- mean((log(data_w_hlp$median_house_value[folds_10==j]) - y_pred_k)^2)
}
mean.cv.errors_10 <- apply(cv.errors_10, 2, mean)
names(mean.cv.errors_10) <- c("Error")
mean.cv.errors_10

error_10 = exp(mean.cv.errors_10) * 1000
error_10


# Define the value of k
k=20

# Split the subsets of data_w_hlp
folds_20 <- sample(1:k, nrow(data_w_hlp), replace=TRUE)
table(folds_20)

# Make the error's vector
cv.errors_20 <- matrix(NA,k,1)
colnames(cv.errors_20) <- "Errors"

# Compute 20-fold Cross Validation and compute the error
for(j in 1:k){
  best.fit <- lm(log(median_house_value)~.-housing_median_age -Min_1H_OCEAN, data=data_w_hlp[folds_20!=j,])
  test.mat <- model.matrix(log(median_house_value)~.-housing_median_age -Min_1H_OCEAN, data = data_w_hlp[folds_20==j,])
  y_pred_k <- predict(model_train, newdata = data_w_hlp[folds_20==j,])
  cv.errors_20[j,1] <- mean((log(data_w_hlp$median_house_value[folds_20==j]) - y_pred_k)^2)
}
mean.cv.errors_20 <- apply(cv.errors_20, 2, mean)
names(mean.cv.errors_20) <- c("Error")
mean.cv.errors_20

# mean.cv.errors_20 e mean.cv.errors_10 errori su log(y) 
error_20 = exp(mean.cv.errors_20) * 1000
error_20

########################################################
#   RIDGE regression with k-Fold Cross-Validation                 
########################################################

# design matrix 
X <- model.matrix(log(data_w_hlp$median_house_value) ~. -housing_median_age -Min_1H_OCEAN, data=data_w_hlp)

# remove the first column relative to the intercept 
X <- X[,-1]

# vector of responses
y <- log(data_w_hlp$median_house_value)

# In order to determine the best value of the regularization parameter we perform 
# a 10-fold cross-validation on  values

train <- sample(1:nrow(X), nrow(X)*0.8)
y_test <- y[-train]

ridge_model <- cv.glmnet(X[train, ], y[train], alpha = 0, nfold=10) # alpha = 0 regressione RIDGE
lambda_min <- ridge_model$lambda.min
lambda_min
coefficient_ridge <- coef(ridge_model, s = "lambda.min")
coefficient_ridge


# Utilizza il lambda minimo selezionato dalla cross-validation
predictions <- predict(ridge_model, newx = X[-train, ], s = "lambda.min")  
predictions_exp <- exp(predictions)

y_exp <- exp(y_test)

n_R <- dim(X[-train, ])[1]
n_R
p_R <- dim(X)[2]
p_R

# Residual Sum of Squares
RSS <- sum((y_test - predictions)^2)
RSS

# Explained Sum of Squares
ESS <- sum((predictions - mean(y_test))^2)
ESS

# Total Sum of Squares
TSS <- ESS + RSS 
TSS

# Residual Standard Error
RSE <- sqrt(RSS/(n_R - p_R - 1))
RSE

# R Squared statistic
R2_R <- 1 - RSS/TSS
R2_R

# adjusted R square
adjR2_R <- 1 - (1-R2_R)*((n_R-1)/(n_R-p_R-1)) 
adjR2_R

#calcolo il MSE (utilizzando RSS o la sua definizione)
MSE_2 <- RSS/n_R
MSE_2
MSE_R <- mean((predictions - y_test)^2)
MSE_R 
# vedo che sono uguali

MSE_R_c = exp(MSE_R) * 1000
MSE_R_c

# questo era quello della model evaluation sulla stessa proporzione di dati
MSE_c

# MSE su log(y) che implica MSE=exp(MSE) su median_house_value/1000 




# grande ridge (che non è quello di BEAUTIFUL) hai fatto un ottimo lavoro hai una cena pagata


########################################################
#                ANOVA                  
########################################################

ocean_proximity <- as.factor(dataset_last$ocean_proximity)
contrasts(ocean_proximity)


# verifico l'ipotesi di equivaranzia tra le variabili categoriche
bartlett.test(log(median_house_value) ~ ocean_proximity , data = dataset_last) 

# rejected since alpha = 0.05 >= p-value







