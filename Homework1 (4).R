
# Load the dataset
bonds <- read.csv("Homework1_Bonds (2).csv")

# 1. How many bonds were approved and how many were defeated?
# Count the number of approved and defeated bonds
approved_bonds <- sum(bonds$Result == "Carried")
defeated_bonds <- sum(bonds$Result == "Defeated")

# Calculate the approval rate by government type
approval_rate_by_type <- prop.table(table(bonds$Result, bonds$Type), margin = 3)

# Print results
approved_bonds
defeated_bonds
approval_rate_by_type

# 2. Calculate a new variable "Votes_Total" and find the bond with the highest voter turnout
# Create a new variable for total votes
bonds$Votes_Total <- bonds$VotesFor + bonds$VotesAgainst

# Find the row with the highest total votes
max_votes_row <- bonds[which.max(bonds$Votes_Total), ]

# Print highest voter turnout bond
max_votes_row

# 3. Subset the data for approved bonds with at least 100 total votes and create Percent_For variable
approved_bonds_subset <- subset(bonds, Result == "Carried" & Votes_Total >= 100)

# Create a new variable for the percentage of votes for the bond
approved_bonds_subset$Percent_For <- (approved_bonds_subset$VotesFor / approved_bonds_subset$Votes_Total) * 100

# Create a histogram for Percent_For
hist(approved_bonds_subset$Percent_For, 
     main="Distribution of Percentage of Votes For Approved Bonds", 
     xlab="Percentage of Votes For", 
     col="blue",        # Darker color
     breaks=20,         # Increased number of bins for better granularity
     border="black",    # Optional: Add black borders to bars
     xlim=c(50, 100),   # Set x-axis limits similar to the example
     ylim=c(0, 350),    # Set y-axis limits to match the scale of the first image
     las=1,
     font.main=1
     )             # Ensure axis labels are displayed horizontally

# Scatter plot showing relationship between Percent_For and bond Amount
plot(approved_bonds_subset$Amount, approved_bonds_subset$Percent_For, 
     main="Margin of Approval vs Bond Cost", 
     xlab="Bond Amount (in dollars)", ylab="Percentage of Votes For", 
     pch=4,     # Use "x" character for points
     col=rgb(0, 0, 1, 0.9),  # Darker blue color with higher intensity
     xlim=c(0, 3e9),  # Set x-axis limits for Bond Amount
     ylim=c(50, 100), # Set y-axis limits for Percentage of Votes For
     las=1,
     font.main=1,     # Horizontal y-axis labels
     cex=0.7,         # Adjust size of the points (optional for visual density)
     xaxt="n")        # Suppress default x-axis

# Manually add x-axis without scientific notation
axis(1, at=seq(0, 3e9, by=0.5e9), labels=c("0.0", "0.5", "1.0", "1.5", "2.0", "2.5", "3.0"))

# Optional: Add grid lines to match the style of the desired output
grid()

