import pdfplumber
import pandas as pd
from pathlib import Path

# Path to the uploaded PDF file
# pdf_path = '../data/May\ 6\ 2023\ Precinct.pdf'
REPO = Path(__file__).resolve().parents[1]
DATA = Path.joinpath(REPO, 'data')

pdf_path = Path.joinpath(DATA, 'june2021runoff.pdf')


# Initialize empty list to store extracted results
data = []

# Open the PDF and extract text
with pdfplumber.open(pdf_path) as pdf:
    for page in pdf.pages:
        text = page.extract_text()
        lines = text.split("\n")
        
        precinct = None
        registered_voters = None
        ballots_cast = None
        voter_turnout = None
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
                
                # Candidates usually appear with vote numbers, we extract their name and the TOTAL column (votes)
                candidate_data = line.split()
                if len(candidate_data) > 1:  # Ensure there's more than just whitespace or invalid lines
                    candidate_name = " ".join(candidate_data[:-2])  # Name should exclude the last two items (votes, etc.)
                    votes = candidate_data[-2]  # The second-to-last item should be the total votes
                    data.append({
                        'Precinct': precinct,
                        'Race': race,
                        'Candidate': candidate_name,
                        'Total Votes': votes
                    })

# Create a pandas DataFrame from the extracted data
df = pd.DataFrame(data)

# Display the DataFrame
print(df.head())  # This will just print the first few rows to verify

# Save the DataFrame to a CSV file
csv_path = 'june2021runoff.csv'
df.to_csv(csv_path, index=False)

print(f"CSV saved to: {csv_path}")






