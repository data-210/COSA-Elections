import pdfplumber
import pandas as pd
from pathlib import Path

# Path to the uploaded PDF file
REPO = Path(__file__).resolve().parents[1]
DATA = Path.joinpath(REPO, 'data')

pdf_path = Path.joinpath(DATA, 'may2021general.pdf')

# Initialize empty list to store extracted results
data = []

# Open the PDF and extract text
with pdfplumber.open(pdf_path) as pdf:
    for page_num, page in enumerate(pdf.pages, start=1):
        print(f"Processing page {page_num}...")  # Debugging
        text = page.extract_text()
        lines = text.split("\n")
        
        # Debug: Print extracted lines for a specific page
        if page_num == 1:  # Adjust this to inspect other pages
            print("\n".join(lines[:20]))  # Print the first 20 lines
        
        precinct = None
        race = None
        candidate_section = False  # Initialize candidate_section as False
        
        for i, line in enumerate(lines):
            # Extract precinct number
            if "Statistics TOTAL" in line:
                precinct = lines[i - 1].strip()
                continue  # Move to next line after capturing precinct
            
            # Capture Mayor race
            if "For Mayor City of San Antonio" in line:
                race = "For Mayor City of San Antonio"
                candidate_section = True  # We're entering a candidate section
                continue  # Move to the candidate section
            
            # Capture City Council races
            if "For Council, City of San Antonio" in line:
                race = line.strip()  # Capture the full race name
                candidate_section = True  # We're entering a candidate section
                continue  # Move to the candidate section
            
            # Extract candidate names and vote totals
            if candidate_section:
                # Stop at "Total Votes Cast" or any similar line, marking the end of candidate list
                if "Total Votes Cast" in line or "Statistics TOTAL" in line:
                    candidate_section = False
                    continue
                
                # Match candidate data using a specific pattern for numbers at the end
                parts = line.rsplit(maxsplit=3)
                if len(parts) >= 4:
                    candidate_name = " ".join(parts[:-3])  # All but last three segments
                    votes = parts[-3]  # Third-to-last segment is the vote total
                    percentage = parts[-1]  # Last segment is the percentage
                    
                    # Debugging: Print parsed candidate data
                    print(f"Precinct: {precinct}, Race: {race}, Candidate: {candidate_name}, Votes: {votes}, Percentage: {percentage}")
                    
                    data.append({
                        'Precinct': precinct,
                        'Race': race,
                        'Candidate': candidate_name,
                        'Total Votes': votes,
                        'Percentage': percentage,
                    })

# Create a pandas DataFrame from the extracted data
df = pd.DataFrame(data)

# Debugging: Print the first few rows of the DataFrame before cleaning
print("Extracted Data (before cleaning):")
print(df.head())

# Filter out rows where 'Percentage' contains invalid data
df = df[df['Percentage'].str.contains(r'^\d+(\.\d+)?%$', na=False)]  # Keep only valid percentages

# Clean and format data
df['Total Votes'] = pd.to_numeric(df['Total Votes'].str.replace(',', ''), errors='coerce')  # Remove commas and convert to numeric
df['Percentage'] = df['Percentage'].str.replace('%', '').astype(float) / 100  # Convert percentage to float

# Debugging: Print the cleaned DataFrame
print("Cleaned Data:")
print(df.head())

# Save the DataFrame to a CSV file
csv_path = 'may2021general_test2.csv'
df.to_csv(csv_path, index=False)

print(f"CSV saved to: {csv_path}")
