### BUSINESS ANALYTICS ###
### Exercise Sheet 7


## Exercise 7.1

library(tidyverse)
library(lubridate) # for d)
library(GGally) # for h)

# a) Load raw_data.csv and rename all attributes to match the "description" column in Table 1.
df <- read_delim('raw_data.csv', delim = ";")
glimpse(df)

# The names of the columns could be changed using the rename function as follow:
df <- df %>% rename(
  order_date = od,
  delivery_date = dd,
  salutation = a6,
  date_of_birth = a7,
  state = a8,
  return_shipment = a9
)
glimpse(df)

# An alternative is to use names function to set column names
names(df) <- c("ID", "order_date", "delivery_date", "size", "price", "tax", "salutation", "date_of_birth", "state", "return_shipment")
glimpse(df)

# b) Correct the data types for all nominal attributes and assign the corresponding labels from the "comment" column in Table 1.

# *salutation*, *state* and *return_shipment* are nominal attributes. Using the mutate function, the columns could be changes.
df <- df %>% mutate(
  salutation = factor(salutation, labels = c("Company", "Mr.", "Mrs.")),
  state = factor(state, labels = c("BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV", "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH")),
  return_shipment = factor(return_shipment, labels = c("no", "yes"))
)

# c) Correct the data type for the ordinal attribute size and assign the corresponding lables from the "comment" column in Table 1.
table(df$size)
# Since *size* attribute has case incongruity, all values are converted to uppercase.
df <- df %>% mutate(size = toupper(size))
table(df$size)

# *size* can be converted to an ordered factor given the ordinal nature as suggested in the table description.
df <- df %>% mutate(size = factor(size, ordered = TRUE, levels = c("S", "M", "L", "XL", "XXL", "XXXL")))
table(df$size)
# **NOTE**: Nowadays it is no longer best practice to store string columns as factors, because it makes it much harder to work with the values. (Unless your data is very big and you run into memory issues). Instead, you would just transfer the columns into strings. If a package/model absolutely *needs* a factor column, the data should be only converted to factor at the very end when calling that package. Nevertheless, as the data is given as integer-keys here, we will just use factors, and using as.factor would be helpful in creating the string cols.

# d) Correct the data types for all date attributes. Create separate attributes for weekday, year, month, day, and quarter of "order date".

# *order date*, *delivery_date* and *date_of_birth* should be dates but only *order date* was parsed correctly, because the others contain '?'. __lubridate::as_date provides__ better date parsing than R's standard __as.date__. __mutate__ can be used with __across__ function to perform similar mutations across multiple columns.
df <- df %>% mutate(across(.cols = c(order_date, delivery_date, date_of_birth), .fns = as_date))
# The reason for the warning is when converting columns with ? in place of the date, the transformation has inserted NA. Looking at the data, the desired columns have data as their type.

# The NAs can be counted by using the __summarise__ and __across__ function.
df %>% summarise(across(where(is.Date), ~sum(is.na(.))))

# __wday__, __month__, __day__ and __quarter__ are lubridate functions that can be used to get the required attributes from the order date.
df <- df %>% mutate(across(order_date, .fns = c(weekday = wday, year = year, month = month, day = day, quarter = quarter), .names = "{.fn}"))

# e) Find missing values (only NA), fill missing prices/tax with averages or remove the instances.

# The missing values (represented by NA) can be counted using by summarising across all of the columns.
df %>% summarise(across(.cols = everything(), .fns = ~sum(is.na(.))))

# The NA values inside *price* and *tax* are filled by the mean of the available values.
df <- df %>% mutate(
  tax = if_else(is.na(tax), mean(tax, na.rm = T), tax),
  price = if_else(is.na(price), mean(price, na.rm = T), price)
)

# f) Calculate a new attribute "delivery time" as the difference of order and delivery date in days.
#    Inspect the values for errors and set the value to "NA" for corresponding instances.

# To get the delivery time, the *delivery_date* is subtracted from the *order_date*. This returns a __lubridate::duration__ object which needs to be converted to numeric.
# Since the delivery time could not be negative, the invalid entries are set to NA.
df <- df %>%
  mutate(delivery_time = delivery_date - order_date) %>%
  mutate(delivery_time = as.numeric(delivery_time, unit = 'days')) %>%
  mutate(delivery_time= if_else(delivery_time < 0.0, NA_real_, delivery_time))

df %>% count(delivery_time, sort = T)

# g) Plot a histogram for the new "delivery time" column. Then discretize ("bin") it to levels "NA", "<= 5d", and "> 5d" in a new attribute "delivery_time_discrete" and plot a bar chart for it.
hist(df$delivery_time)
# Plotting the histogram shows that most of the values lie around 0 to 10 days. Thus, it makes sense to "bin" the data as proposed above.

# The data can be binned as described in the question using __case_when__ method.
df <- df %>%
  mutate(dt_binned = case_when(
    is.na(delivery_time) ~ "NA",
    delivery_time <= 5 ~ "<= 5d",
    TRUE ~ "> 5d")
  )
barplot(table(df$dt_binned))

# alternatively, using ggplot
ggplot(df, aes(x=dt_binned)) + stat_count()

# h) Compute the correlation matrix for the numerical attributes only. Plot the matrix of the scatterplots.
#    Plot the heat map of the correlation matrix.

# To calculate the correlation of all numeric attributes, the numeric attributes are first selected and then passed through the correlation function.
corr <- df %>% select(where(is.numeric)) %>% cor(use = "pairwise.complete.obs")

# Similarly to create a pairwise scatter plot of all numeric attributes, we could use __pairs__ method.
df %>% select(where(is.numeric)) %>% pairs

# GGally extends ggplot2 and can be used to plot a heatmap of calculated correlations.
df %>% select(where(is.numeric)) %>% ggcorr

# i) Standardize all numerical values and again compute their correlation matrix.

# Standardization subtracts from each value the mean of the distribution and divides by the standard deviation. Thus the resulting values have zero mean and unit standard deviation. This can be achieved in R by using the __scale__ method.
df <- df %>% select(where(is.numeric)) %>% mutate(across(.cols = everything(), .fns = scale))
df %>% 
  summarise(across(everything(), .fns = c(~ mean(., na.rm = T), ~ sd(., na.rm = T))))

# As can be observed the resulting columns have zero mean and unit standard deviation. The correlation can be calculated as before.
corr <- df %>% select(where(is.numeric)) %>% cor(use = "pairwise.complete.obs")
corr %>% ggcorr


## Exercise 7.2

# a) Load fuel_generation_by_country.csv and observe the format of the data. Think about the format of the data.
#    Is it tidy? In what ways does it violate the tidy data assumptions?
country_data <- read_csv('fuel_generation_by_country.csv')
glimpse(country_data)
# The data is in wide format: The variable 'fuel' does not have it's own column, instead each fuel has its own column name. The inclusion of a 'total' column should be noted, although that doesn't directly violate tidy data assumptions.

# b) Check the consistency of the data by comparing the sum of generated power by fuel with the total fuel generation of each country.
#    Are there any inconsistencies? How would you deal with them?

# One of the ways to check the fidelity of data we first convert it to tidy format and then calculate the sum of all fuel generation methods for each country. The calculated sum can then be compared with the *Total* column for each country checked for any discrepancy.
country_data <- country_data %>% 
  pivot_longer(cols = -country, names_to = "fuel", values_to = "generation_gwh") %>% 
  mutate(generation_year = 2014)
# converting data to long format and adding the year (2014) for clarity

# calculate sum of fuel generation from all methods
cd_total_calculated <- country_data %>%
  # all except total
  filter(fuel != 'Total') %>% 
  group_by(country) %>% 
  summarize(total_generation_calculated = sum(generation_gwh))

# get given total fuel generation
cd_total_given <- country_data %>% filter(fuel == 'Total') %>% 
  select(country, total_generation_given = generation_gwh)

# The two tibbles generated above can be joined together using country as the key.
cd_total_calculated %>% 
  inner_join(cd_total_given)%>% 
  filter(total_generation_calculated != total_generation_given)

# From the above result, there exists one observation where the total is not equivalent to the sum of all the other fuel generation methods. In this case, let's assume that we trust our calculated value more than the given one, and overwrite the latter as follows:
country_data[country_data$country == 'Niger' & country_data$fuel == 'Total', ]$generation_gwh <- 690

# c) Load global_power_plants.csv and observe the format. Discuss whether the data is tidy.
power_data <- read_csv('global_power_plants.csv', 
               col_types = cols(other_fuel3 = col_character(), wepp_id = col_character(), production_gwh = col_double() ))
glimpse(power_data)

# The data is tidy: Every observation (power plant and year combination) is in a row, every observation is in a column and every value is in one cell. Note the choice of fuel columns ('primary_fuel', 'other_fuelX') - this is sometimes done to avoid using relational data when that would lead to considerable overhead.
# Note that the first entries have NAs in the `production_gwh` column, so let's look at a random subsample of the data to check whether this persists everywhere or is only a problem of a few plants:
power_data %>% sample_n(20) %>% glimpse()
# We see that many entries have NAs, but it is not a general problem throughout the dataset.

# d) For each powerplant find what percentage of its country's total power supply is attributed to the plant?

# To generate this new feature, it is required that the two tables be joined since one of the tables give us country specific information and the other table gives us power plant specific information. We will put the `generation_year` attribute into wide format to facilitate the join. As we only have country data for 2014, we will use this data as a proxy for all years when engineering the new feature.
power_data <- power_data %>% pivot_wider(names_from = "generation_year", names_prefix = "generatino_gwh_", values_from = "production_gwh")

power_data_joined <- power_data %>%
  left_join(
    #transmute drops existing columns
    country_data %>% filter(fuel == 'Total') %>% transmute(country, country_gen_total = generation_gwh),
    by = c("country_long" = "country")
  )

# we look at a random sample rather than the first rows, as the latter have many NAs in the generation data
glimpse(power_data_joined %>% sample_n(20))

# Once the tables are properly joined, the *capacity_mw* feature can be used to calculate the share of the powerplant to the countries total power supply.
power_data_joined <- power_data_joined %>%
  # convert Gwh to MW averages
  mutate(
    country_gen_total = 1000 / 24 / 365.25 * country_gen_total
  ) %>%
  mutate(cap_share_of_country_gen_total = capacity_mw / country_gen_total)

power_data_joined %>% summarise(name, cap_share_of_country_gen_total) %>% sample_n(10)