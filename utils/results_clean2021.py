import pandas as pd
from pathlib import Path

# Load CSV file
REPO = Path(__file__).resolve().parents[1]
DATA = Path.joinpath(REPO, 'data')
csv_file_path = Path.joinpath(DATA, 'may2021general_clean.csv')

# Read in CSV
df = pd.read_csv(csv_file_path)

# Clean the Precinct column to only contain the 4-digit precinct number
df['Precinct'] = df['Precinct'].astype(str).str.extract(r'(\d{4})')

# Remove rows with 'TOTAL VOTE % Election' in the Candidate column
df = df[~df['Candidate'].str.contains('TOTAL VOTE % Election', na=False)]

# Define a function to split the Candidate column into separate parts
def split_candidate(row):
    if isinstance(row, str):  # Check if the row is a string
        candidate_info = row.split()
        try:
            # Find the percentage (%), which usually contains '%'
            percent_idx = next(i for i, token in enumerate(candidate_info) if '%' in token)
            percentage = candidate_info[percent_idx].replace('%', '')  # Clean the percentage
            # The total votes are before the percentage and are digits
            votes = candidate_info[percent_idx - 1] if candidate_info[percent_idx - 1].isdigit() else None
            # Candidate name is everything before the votes
            name = " ".join(candidate_info[:percent_idx - 1]) if votes else " ".join(candidate_info[:percent_idx])
        except StopIteration:  # If no percentage is found
            name, votes, percentage = row, None, None
        return pd.Series([name, votes, percentage])
    else:
        return pd.Series([None, None, None])

# Apply the function to split the Candidate column
df[['Candidate', 'Total Votes', 'Vote Percentage']] = df['Candidate'].apply(split_candidate)

# Convert Total Votes to integer and Vote Percentage to float
df['Total Votes'] = pd.to_numeric(df['Total Votes'], errors='coerce')
df['Vote Percentage'] = pd.to_numeric(df['Vote Percentage'], errors='coerce')

# Remove rows where Candidate, Total Votes, and Vote Percentage are all blank
df = df.dropna(subset=['Candidate', 'Total Votes', 'Vote Percentage'], how='all')

# Keep only the desired columns
df = df[['Precinct', 'Race', 'Candidate', 'Total Votes', 'Vote Percentage']]

# Save the cleaned CSV
cleaned_file_path = "may2021general_cleaned.csv"
df.to_csv(cleaned_file_path, index=False)

print(f"Cleaned CSV saved to: {cleaned_file_path}")
