fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}


# new function ------------------------------------------------------------


data_storm <- read.csv("storms.csv")
summary(data_storm)

# this create a new subset, just hurricanes
hurricane <- data_storm[data_storm$status == "hurricane",]
head(hurricane)

# select specific columns
hurricane <- hurricane[, c("name", "year", "category", "pressure", "wind")]
head(hurricane)

# create a function to classify wind velocity 
classify_wind <- function(wind) {
  if(wind<80){
    "Low"
  }else if (wind<110){
    "moderate"
  }else {
    "high"
  } 
}

hurricane$windclass <- sapply(
  hurricane$wind,
  classify_wind
)

head(hurricane, 20)
table(hurricane$windclass)

head(starwars)
starwars %>%
  filter(species == "Droid")

only_droids <- starwars |> 
  filter(species == "Droid")
only_droids


starwars %>% 
  filter(skin_color == "light", eye_color == "brown")

starwars |> 
  arrange(desc(height))

starwars %>% slice(5:10)

# Select columns by name
col <- starwars %>% 
  select(hair_color,
         skin_color, 
         eye_color)
col

starwars %>% select(hair_color:eye_color)

starwars %>% select(!(hair_color:eye_color))

another <- starwars |> 
  select(-height)
another

starwars %>% select(contains('c'))

new_s <- starwars |> 
  mutate(mass_new = mass * 1000)
new_s |> 
  select(name, mass, mass_new)
new_s


# Load dataset
data(DNase)
# Step 1: Inspect data
head(DNase, 3)
# Step 2: Calculate mean density per concentration
mean_density <- aggregate(density ~ conc, data = DNase, FUN = mean)
# Step 3: Create barplot
barplot(
  height = mean_density$density, # height is a vector of numeric values that determines the height of each bar
  names.arg = mean_density$conc, # names.arg is a vector of names of each bar
  col = "lightblue",
  border = "gray30", 
  main = "Mean DNase I Activity by Concentration", # Main title of the plot
  xlab = "Concentration (mg/mL)", # Label for the X-axis
  ylab = "Mean Optical Density", # Label for the Y-axis
  las = 2,          # rotate labels for clarity
  cex.names = 0.8  # adjust label size
) # Don't forget to close the parenthesis


# Step 3: Create barplot
barplot(
  height = mean_density$density,
  names.arg = mean_density$conc,
  col = "lightblue",
  border = "gray30",
  main = "Mean DNase I Activity by Concentration",
  xlab = "Concentration (mg/mL)",
  ylab = "Mean Optical Density",
  las = 2,          
  cex.names = 0.8,   
  horiz = TRUE      # make bars horizontal
)


library(readxl)
coronary <- read_excel("coronary.xlsx")
head(coronary)

plot(coronary$dbp ~ coronary$chol,
     col = "blue",
     xlim = c(4,10), 
     ylim = c(40,140),
     xlab = "Total Cholesterol (mmol/L)",
     ylab = "Diastolic Blood Pressure (mmHg/L)"
)

abline(lm(dbp ~ chol, data = coronary), 
       col = "red", lwd = 2,lty = 1)

spearman_result <- cor.test(
  coronary$chol,
  coronary$dbp,
  method = "spearman",
  exact = FALSE # avoids warnings with tied ranks
)
spearman_result


shapiro.test(coronary$chol)
shapiro.test(coronary$dbp)

hist(coronary$chol)
hist(coronary$dbp)


# Asegúrate de que los datos estén ordenados por edad
coronary <- coronary[order(coronary$age), ]



# Gráfico de líneas básico
plot(coronary$age, coronary$chol,
     type = "l", # "l" = line plot
     col = "blue",
     lwd = 2,
     xlab = "Age (years)",
     ylab = "Cholesterol (mmol/L)",
     main = "Cholesterol vs Age")




library(ggplot2)
tyre <- read.csv("tyre.csv")
head(tyre)

summary(tyre)

plot <- ggplot(tyre, 
       aes(Brands, Mileage)) +
  geom_boxplot(aes(fill = Brands)) +
 # geom_violin() +
  labs(title= "Marcas de llantas y cuanto aguantan",
       x= "Marcas caras",
       y= "Millas"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
    
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12
  )
  
plot 
ggsave(
  filename = "Mileage_by_Brand.png",
  plot = plot,
  width = 8, height = 6, dpi = 300)

getwd()
plot
# Statistical analysis brands
summary(tyre)
mod <- aov(Mileage ~Brands, data = tyre)
summary(mod)

resid_anova <- residuals(mod)
shapiro.test(resid_anova)

TukeyHSD(mod)
plot(TukeyHSD(mod))






# Read the TSV file
duck_data <- read.delim(
  "duck_otu_relative_abundance_20.tsv",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Write to CSV
write.csv(
  duck_data,
  "duck_otu_relative_abundance_20.csv",
  row.names = FALSE
)

# Optional: verify the conversion
cat("Rows:", nrow(duck_data), "\n")
cat("Columns:", ncol(duck_data), "\n")
cat("First few column names:\n")
print(head(colnames(duck_data), 10))


install.packages("lessR")
library(lessR)
fish <- read.csv("fish_data.csv")
head(fish)

boxplot(fish$Length)

pivot(fish, c(IQR, skew, kurtosis,max, min, mean, sd, var), Length)

q25 <- quantile(fish$Length,probs=(0.25))
q75 <- quantile(fish$Length,probs=(0.75))

iqr <- q75 - q25
lower <- q25 - (1.5*iqr)
upper <- q75 + (1.5*iqr)
  
lower
upper

shapiro.test(fish$Weight)
