import pandas as pd
import numpy as np

votes = pd.read_csv("../data/_processed/_allcsv/votes.csv", delimiter=",")

#Creating a dataset containing a timeseries for votes per county
cleanedSet = pd.DataFrame(columns=['state_abbr', 'county_name', 'votes_dem', 'votes_gop', 'total_votes', 'candDem',  'candGop', 'diff', 'diff_per_point', 'population', 'elecYear'])

for index, row in votes.iterrows():
    tmp16 = row[['state_abbr', 'county_name', 'votes_dem_2016', 'votes_gop_2016', 'total_votes_2016', 'Clinton', 'Trump', 'diff_2016', 'per_point_diff_2016', 'population2014']]
    tmp16['elecYear'] = '2016'
    tmp16 = pd.DataFrame(tmp16)
    cleanedSet.loc[len(cleanedSet)] = [j for sub in tmp16.values for j in sub]

    tmp12 = row[['state_abbr', 'county_name', 'votes_dem_2012', 'votes_gop_2012', 'total_votes_2012', 'Obama', 'Romney', 'diff_2012', 'per_point_diff_2012', 'population2010']]
    tmp12['elecYear'] = '2012'
    tmp12 = pd.DataFrame(tmp12)
    cleanedSet.loc[len(cleanedSet)] = [j for sub in tmp12.values for j in sub]

print(cleanedSet)

cleanedSet.to_csv('../data/_processed/fe/timeseriesVotes.csv')
