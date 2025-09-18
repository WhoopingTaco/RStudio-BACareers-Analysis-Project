#Pre-requisite packages
install.packages(c("tidyr","readxl","dplyr","stringr"))
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

# Read dataset - split in 2 due to inconsistent format
raw_data1 <- read_xlsx("C:/Users/mudit.000/Downloads/Compressed/1738253651_Task 8_R-Data Analysis-BA Careers/dataset.xlsx", n_max = 3693)
raw_data2 <- read_xlsx("C:/Users/mudit.000/Downloads/Compressed/1738253651_Task 8_R-Data Analysis-BA Careers/dataset.xlsx", skip = 3693, col_names = FALSE)

# Rename columns for dataset 1
colnames(raw_data1) <- c("drop1", "drop2", "Job.Title", "Salary.Estimate", "Job.Description", "Rating", "Company.Name", "Location", "Headquarters", "Size", "Founded", "Type.of.ownership", "Industry", "Sector", "Revenue", "Competitors", "Easy.Apply")

# Drop index columns due to redundant information
raw_data1 <- select(raw_data1, !c(drop1,drop2))

# Rename columns for dataset 2
colnames(raw_data2) <- c("Job.Title", "Salary.Estimate", "Job.Description", "Rating", "Company.Name", "Location", "Headquarters", "Size", "Founded", "Type.of.ownership", "Industry", "Sector", "Revenue", "Competitors", "Easy.Apply")

# Merge datasets after amendments
raw_data <- rbind(raw_data1,raw_data2)

# Find missing values in each attribute
colSums(is.na(raw_data))

# Make columns for lower & upper bounds of salary estimates
raw_data <- raw_data %>%
    mutate(
        Salary.Lower.Bound = str_extract(Salary.Estimate, "\\$\\d+K") %>%
            str_remove_all("[$K]") %>%
            as.numeric() * 1000,
        
        Salary.Upper.Bound = str_extract(Salary.Estimate, "-\\s*\\$\\d+K") %>%
            str_remove_all("[-$K\\s]") %>%
            as.numeric() * 1000
    )

# Remove rating number from company name
raw_data <- raw_data %>%
    mutate(
        Company.Name = str_remove(Company.Name, "\\s+\\d+\\.\\d+$")
    )

# Categorise organisation type
raw_data$Type.of.ownership <- factor(raw_data$Type.of.ownership)

# Rename easy-apply values to Yes or No, and categorise
raw_data <- raw_data %>%
    mutate(
        Easy.Apply = recode(Easy.Apply,
                            `1` = "Yes",
                            `-1` = "No",
    ))
raw_data$Easy.Apply <- factor(raw_data$Easy.Apply)

# Rename odd values & missing sectors to their appropriate levels and categorise
raw_data <- raw_data %>%
    mutate(
        Sector = recode(Sector,
                        `-1` = "Not Known",
                        "Goldman Sachs, Commonwealth Bank of Australia, Deutsche Bank" = "Finance")
    )
raw_data$Sector <- factor(raw_data$Sector)

# Rename odd values & missing industries to their appropriate levels and categorise
raw_data <- raw_data %>%
    mutate(
        Industry = recode(Industry,
                          `-1` = "Not Known",
                          "$5 to $10 billion (USD)" = "Not Known")
    )
raw_data$Industry <- factor(raw_data$Industry)

# Rename odd values & missing revenue to their appropriate levels and categorise
raw_data <- raw_data %>%
    mutate(
        Revenue = recode(Revenue,
                         `-1` = "Unknown / Non-Applicable")
    )
raw_data$Revenue<- factor(raw_data$Revenue)

# Rename odd values & missing competitors to their appropriate levels
raw_data <- raw_data %>%
    mutate(
        Competitors = recode(Competitors,
                         `-1` = "Not Known")
    )

# Rename odd values & missing competitors to their appropriate levels & categorise
raw_data <- raw_data %>%
    mutate(
        Size = recode(Size,
                         `-1` = "Unknown",
                      'Company - Public' = 'Unknown')
    )
raw_data$Size<- factor(raw_data$Size)

# Make mean salary column
raw_data <- raw_data %>%
mutate(Mean_Salary = (Salary.Lower.Bound + Salary.Upper.Bound)/2)

# Display Job Title, Rating, Location, Industry
View(select(raw_data, Job.Title, Rating, Location, Industry))

# Top 20 Industries
top_industries <- raw_data %>%
    group_by(Industry) %>%            # group by industry
    summarise(frequency = n()) %>%        
    arrange(desc(frequency)) %>%          # sort descending
    slice_head(n = 20)                # take top 20

# Top 20 Sectors
top_sectors <- raw_data %>%
    group_by(Sector) %>%            # group by industry
    summarise(frequency = n()) %>%        
    arrange(desc(frequency)) %>%          # sort descending
    slice_head(n = 20)                # take top 20

# Top 20 Headquarters
raw_data <- raw_data %>%
    mutate(
        Headquarters = recode(Headquarters,
                         `-1` = "Unknown")
    )

top_hq <- raw_data %>%
    group_by(Headquarters) %>%            # group by industry
    summarise(frequency = n()) %>%        
    arrange(desc(frequency)) %>%          # sort descending
    slice_head(n = 20)                # take top 20

# Top 15 jobs based on Rating
top_consulting_jobs <- raw_data %>%
    filter(Industry == "Consulting") %>%       # only consulting jobs
    group_by(Job.Title) %>%                    # group by job title
    summarise(avg_rating = mean(Rating, na.rm = TRUE)) %>% 
    arrange(desc(avg_rating)) %>%              # highest first
    slice_head(n = 15)                         # take top 15

# Top 15 jobs based on Rating under Consulting industry
raw_data <- raw_data %>%
    mutate(
        Rating = case_when(
            is.na(Rating) ~ "Unknown",
            Rating == -1  ~ "Unknown",
            TRUE ~ as.character(Rating)
        )
    )

# Bottom 15 jobs based on Rating
bottom_jobs <- raw_data %>%
    filter(Rating != "Unknown") %>%     
    mutate(Rating = as.numeric(Rating)) %>%  
    select(Job.Title, Rating, Company.Name, Salary.Estimate, Location) %>%
    arrange(Rating) %>%
    slice_head(n = 15)

# Bubble plot of Top 10 Energy companies with ratings > 3, bubbled by number of job postings
ggplot(energy_companies, aes(x = Avg_Rating, y = reorder(Company.Name, Avg_Rating), size = Count)) +
geom_point(color = "steelblue", alpha = 0.7) +
labs(title = "Top 10 Energy Companies (Rating > 3)",
x = "Average Rating",
y = "Company",
size = "Job Postings") +
theme_minimal()

# Bubble plot of Top 10 Accounting companies with ratings > 3, bubbled by number of job postings
accounting_companies <- raw_data %>%
    filter(Industry == "Accounting", Rating > 3) %>%
    group_by(Company.Name) %>%
    summarise(Avg_Rating = mean(Rating, na.rm = TRUE),
              Count = n()) %>%
    arrange(desc(Avg_Rating)) %>%
    slice_head(n = 10)
ggplot(accounting_companies, aes(x = Avg_Rating, y = reorder(Company.Name, Avg_Rating), size = Count)) +
    geom_point(color = "steelblue", alpha = 0.7) +
    labs(title = "Top 10 Accounting Companies (Rating > 3)",
         x = "Average Rating",
         y = "Company",
         size = "Job Postings") +
    theme_minimal()

# Horizontal Bar chart of Revenue vs Average Rating 
raw_data %>%
    group_by(Revenue) %>%
    summarise(Avg_Rating = mean(Rating, na.rm = TRUE)) %>%
    ggplot(aes(x = Revenue, y = Avg_Rating)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "Revenue vs. Average Rating", x = "Revenue", y = "Average Rating") +
    theme_minimal()
raw_data$Rating <- as.numeric(raw_data$Rating)

# Horizontal Bar chart of Top 10 locations by job postings
raw_data %>%
    group_by(Location) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    slice_head(n = 10) %>%
    ggplot(aes(x = reorder(Location, Count), y = Count)) +
    geom_col(fill = "darkred") +
    coord_flip() +
    labs(title = "Top 10 Locations by Job Postings", x = "Location", y = "Count") +
    theme_minimal()

# Bar chart of Average Salary vs Easy Apply
raw_data %>%
group_by(Easy.Apply) %>%
summarise(Avg_Salary = mean(Mean_Salary, na.rm = TRUE)) %>%
ggplot(aes(x = Easy.Apply, y = Avg_Salary, fill = Easy.Apply)) +
geom_col() +
labs(title = "Easy Apply vs. Average Salary", x = "Easy Apply", y = "Average Salary") +
theme_minimal()

# Horizontal Bar Chart of Top 10 Industries by Average Salary
raw_data %>%
    group_by(Industry) %>%
    summarise(Avg_Salary = mean(Mean_Salary, na.rm = TRUE)) %>%
    arrange(desc(Avg_Salary)) %>%
    slice_head(n = 10) %>%
    ggplot(aes(x = reorder(Industry, Avg_Salary), y = Avg_Salary)) +
    geom_col(fill = "darkgreen") +
    coord_flip() +
    labs(title = "Top 10 Industries by Average Salary", x = "Industry", y = "Avg Salary") +
    theme_minimal()

# Bar-Boxplot Chart for the salary distribution in every industry
raw_data %>%
    group_by(Industry) %>%
    filter(n() >= 20) %>%  # keep only industries with 20+ jobs
    ungroup() %>%
    ggplot(aes(x = reorder(Industry, Salary.Upper.Bound, FUN = median),
               y = Salary.Upper.Bound)) +
    geom_boxplot(outlier.color = "red", fill = "lightblue") +
    coord_flip() +
    labs(title = "Salary Distribution by Industry (Upper Bound, ≥20 Jobs)",
         x = "Industry", y = "Upper Bound Salary ($)") +
    theme_minimal()

# Bar chart showing salary ranges in each industry, average rating indicated by colour gradient & number of job postings indicated by thickness of bar
raw_data %>%
  filter(!is.na(Rating), !is.na(Industry)) %>%
  group_by(Industry) %>%
  summarise(
    Avg_Rating = mean(as.numeric(Rating), na.rm = TRUE),
    Median_Lower = median(Salary.Lower.Bound, na.rm = TRUE),
    Median_Upper = median(Salary.Upper.Bound, na.rm = TRUE),
    Job_Count = n()
  ) %>%
  top_n(15, Median_Upper) %>%   # select top 15 by upper median salary
  ggplot(aes(x = Median_Lower, xend = Median_Upper, y = reorder(Industry, Median_Upper), yend = Industry)) +
  geom_segment(aes(color = Avg_Rating, size = Job_Count), lineend = "round") +
  scale_color_viridis_c(option = "plasma", name = "Avg Rating") +
  scale_size(range = c(1, 8), name = "Job Count") +
  labs(title = "Top 15 Industries: Salary Ranges vs Avg Rating & Job Count",
       x = "Salary Range ($)", y = "Industry") +
  theme_minimal()
