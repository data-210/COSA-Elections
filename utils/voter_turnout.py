import pdfplumber
import pandas as pd
from pathlib import Path

# Path to your local PDF file
REPO = Path(__file__).resolve().parents[1]
DATA = Path.joinpath(REPO, 'data')

pdf_path = Path.joinpath(DATA, 'june2021runoff.pdf')

# Initialize empty list to store extracted results
turnout_data = []

# Open the PDF and extract text
with pdfplumber.open(pdf_path) as pdf:
    for page in pdf.pages:
        text = page.extract_text()
        lines = text.split("\n")
        
        precinct = None
        stats_section = False  # Initialize as False
        
        for i, line in enumerate(lines):
            # Extract precinct number
            if "Statistics TOTAL" in line:
                precinct = lines[i - 1].strip()
                stats_section = True  # Enter the statistics section
                continue  # Move to the statistics section
            
            if stats_section:
                # Extract the statistics data
                if "Registered Voters - Total" in line:
                    registered_voters = line.split()[-1]
                if "Ballots Cast - Total" in line:
                    ballots_cast = line.split()[-1]
                if "Ballots Cast - Blank" in line:
                    ballots_blank = line.split()[-1]
                if "Voter Turnout - Total" in line:
                    voter_turnout = line.split()[-1]
                    # Append data once all relevant fields are found
                    turnout_data.append({
                        'Precinct': precinct,
                        'Registered Voters': registered_voters,
                        'Ballots Cast': ballots_cast,
                        'Ballots Cast - Blank': ballots_blank,
                        'Voter Turnout (%)': voter_turnout
                    })
                    stats_section = False  # Exit the statistics section

# Create a pandas DataFrame from the extracted data
df_turnout = pd.DataFrame(turnout_data)
df_turnout['Precinct'] = df_turnout['Precinct'].astype(str).str.extract(r'(\d{4})')

# Save the DataFrame to a new CSV file
df_turnout.to_csv('voter_turnout2021runoff.csv', index=False)

#print(f"Voter Turnout CSV saved to: {turnout_csv_path}")
