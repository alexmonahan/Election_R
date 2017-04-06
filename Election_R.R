#Read in the given election data for this project
elec_df <- read.csv("http://elections.huffingtonpost.com/pollster/2016-general-election-trump-vs-clinton-vs-johnson.csv")

#Here are the only columns we actually want to analyze 
myCols <- c("Pollster", "Population", "Start.Date", "End.Date", 
                  "Number.of.Observations",
                  "Trump", "Clinton", "Johnson", "Other", "Undecided")

#We can keep only these columns with the following line
elec_df <- elec_df[, myCols]

#Subset the dataframe again -- only want to keep the likely voters in the population
elec_df <- elec_df[elec_df$Population %in% 
                                     c("Likely Voters"), ] 

#Data formatting to get a middle date -- as explained in class
elec_df$Start.Date <- as.Date(as.character(elec_df$Start.Date))
elec_df$End.Date <- as.Date(as.character(elec_df$End.Date))
elec_df$Date <- (elec_df$Start.Date + 
                           difftime(elec_df$End.Date, elec_df$Start.Date, units = "days") / 2)

#We want to organize the data so we can see the percentage support for each
#candidate changing over time
elec_df_long <- reshape(elec_df, 
                                varying = c("Trump", "Clinton", "Johnson", "Other", "Undecided"), 
                                v.names = "Percent",
                                timevar = "Candidate", 
                                times = c("Trump", "Clinton", "Johnson", "Other", "Undecided"), 
                                direction = "long")

#Comparison of the two different dataframes
head(elec_df)

#The 5 different candidate options: Clinton, Trump, Johnson, Other, and Undecided
elec_df_long$Candidate <- factor(elec_df_long$Candidate,
                                         levels = c("Clinton", "Trump",
                                                    "Johnson", "Other", 
                                                    "Undecided"))

#Import ggplot2 to add a visualization for the election data
#Visualization for amount of support for each candidate over time
library(ggplot2)
p <- ggplot(elec_df_long, aes(x = Date, y = Percent)) + 
  geom_point(aes(color = Candidate, size = Number.of.Observations)) 
print(p)

#We want the given colors to match the colors of the political parties
repdem_Cols <- c("Trump" = "#e31a1c", "Clinton" = "#1f78b4", 
                "Johnson" = "#ff7f00", "Other" = "#969696", 
                "Undecided" = "#cccccc")
p <- p + xlim(as.Date("2016-06-01"), NA) +
  scale_color_manual(values = repdem_Cols) 
p

#Smoothing to show the support for the candidates changing over time
ggplot(elec_df_long, aes(x = Date, y = Percent)) + 
  geom_point(aes(color = Candidate)) +
  geom_smooth(aes(color = Candidate, fill = Candidate)) +
  xlim(as.Date("2016-06-01"), NA) + #ylim(0, 100) +
  scale_fill_manual(values = repdem_Cols) +
  scale_color_manual(values = repdem_Cols) +
  theme_bw()