import pdfplumber
import pandas as pd

# Path to your local PDF file
pdf_path = '/Users/jackturek/Desktop/Data Data Data/Personal Projects/Public Policy Stuff/Voting, Political Science & Elections/Analzying Election and Poll Data/SanAntonio/Elections/May 6 2023 Precinct.pdf'

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

# Save the DataFrame to a new CSV file
df_turnout.to_csv('voter_turnout2023.csv', index=False)

#print(f"Voter Turnout CSV saved to: {turnout_csv_path}")
