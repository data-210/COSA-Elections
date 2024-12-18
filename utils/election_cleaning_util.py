import pandas as pd

# Load CSV file
csv_file_path = 'Bexar County & COSA Elections/Analzying Election and Poll Data/SanAntonio/satx2023_dirty.csv'

# Read in CSV
df = pd.read_csv(csv_file_path)

# Remove Vote and Total rows from Candidate column
df_cleaned = df[~df['Candidate'].isin(['Vote', 'TOTAL'])]

# Remove For and VOTE from Total Votes column
df_cleaned = df_cleaned[~df['Total Votes'].isin(['For', 'VOTE'])]

# Convert 'Total Votes' col to numeric
df_cleaned['Total Votes'] = pd.to_numeric(df_cleaned['Total Votes'], errors='coerce')
df_cleaned['Total Votes'] = df_cleaned['Total Votes'].fillna(0).astype(int)

# Modify Race column
df_cleaned['Race'] = df_cleaned['Race'].replace({
    'For Mayor City of San Antonio': 'Mayor',
    'For Council, City of San Antonio, District 1': 'District 1',
    'For Council, City of San Antonio, District 2': 'District 2',
    'For Council, City of San Antonio, District 3': 'District 3',
    'For Council, City of San Antonio, District 4': 'District 4',
    'For Council, City of San Antonio, District 5': 'District 5',
    'For Council, City of San Antonio, District 6': 'District 6',
    'For Council, City of San Antonio, District 7': 'District 7',
    'For Council, City of San Antonio, District 8': 'District 8',
    'For Council, City of San Antonio, District 9': 'District 9',
    'For Council, City of San Antonio, District 10': 'District 10',
})

# Fix Candidate column
df_cleaned['Candidate'] = df_cleaned['Candidate'].replace({
    'Adriana Rocha': 'Adriana Rocha Garcia',
    'Andrew "AJ"': 'Andrew "AJ" Luck',
    'Armando': 'Armando Dominguez',
    'Bryan R.': 'Bryan R. Martin',
    'Carla': 'Carla Walker',
    'Cesario': 'Cesario Garcia',
    'Chris': 'Chris Baecker',
    'Christopher': 'Christopher Longoria',
    'Christopher T.': 'Christopher T. Schuchardt',
    'Dan': 'Dan Rossiter',
    'David Allan': 'David Allan Lara',
    'Denise': 'Denise Gutierrez',
    'Diana Flores': 'Diana Flores Uriegas',
    'Dominique': 'Dominique Lui',
    'Edward Earl': 'Edward Earl Giles',
    'Erin Gallegos': 'Erin Gallegos Reid',
    'Ernest': 'Ernest Salinas',
    'Gary': 'Gary Allen',
    'Gregorio De La': 'Gregorio De La Paz',
    'Irina': 'Irina Rudolph',
    'Jacob B.': 'Jacob B. Chapa',
    'Jalen': 'Jalen McKee-Rodriguez',
    'James M.': 'James M. Guild',
    'Jarrett': 'Jarrett Lipman',
    'Jayden': 'Jayden Muñoz',
    'Jeremy': 'Jeremy Roberts',
    'Joel': 'Joel Solis',
    'John': 'John Courage',
    'Larry La': 'Larry La Rose',
    'Lauro': 'Lauro Bustamante',
    'Madison': 'Madison Gutierrez', 
    'Manny': 'Manny Pelaez',
    'Marc': 'Marc Whyte',
    'Margaret': 'Margaret Sherwood',
    'Marina Alderete': 'Marina Alderete Gavito',
    'Mario': 'Mario Bravo',
    'Melissa Cabello': 'Melissa Cabello Havrda',
    'Michael': 'Michael Idrogo',
    'Michael John': 'Michael John Good',
    'Patrick': 'Patrick Jones',
    'Phyllis': 'Phyllis Viagran',
    'Ray Adam': 'Ray Adam Basldua',
    'Rick': 'Rick Otley',
    'Robert': 'Robert Flores',
    'Roberto Rios': 'Roberto Rios Ortega',
    'Ron': 'Ron Nirenberg',
    'Rose Requenz': 'Rose Requenez Hill',
    'Sandragrace': 'Sandragrace Martinez',
    'Sukh': 'Sukh Kaur',
    'Wendell': 'Wendell Carson',
    'William T.': 'William T. Lamar-Boone'
})

# Calculate total votes per precinct and race
total_votes_per_race = df_cleaned.groupby(['Precinct', 'Race'])['Total Votes'].transform('sum')

# Calculate vote percentage for each candidate
df_cleaned['Vote Percentage'] = (df_cleaned['Total Votes'] / total_votes_per_race) * 100

# Round Vote Percentage to 2 decimal points
df_cleaned['Vote Percentage'] = df_cleaned['Vote Percentage'].round(2)

df_cleaned.to_csv('satx2023_generalelection_002.csv')