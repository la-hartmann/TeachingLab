def race_col:
#get the count of the number or races a single row has, assign to count variable
    count=[]
    check = df[['race_1','race_2','race_3', 'race_4','race_5','race_6']]
    for i in range(len(df)):
        not_empty= check.count(axis=1).iloc[i]
        count.append(not_empty)
    df['race_count']=count
    
    #convert race columns to strings
    columns = ['race_1', 'race_2', 'race_3', 'race_4', 'race_5', 'race_6','race_7','race_7_TEXT']
    for col in columns:
        df[col] = df[col].astype(str)
    
    #create new column that is all encompassing of the racial identity data according to SY23-24 reporting guidelines
    Race = []
    for i in range(len(df)):
        if df['ethnicity'].iloc[i] == 'Hispanic or Latino':
            race = 'Hispanic/Latino'
        else:
            if df.race_count.iloc[i]>1:
                race = 'More than one race'
            
            if df.race_count.iloc[i]<=1:
              #check 'Other' reporting for H/L and >1 race
                if 'Hispanic' in df['race_7_TEXT'].iloc[i]: 
                    race = 'Hispanic/Latino'
                if 'Latin' in df['race_7_TEXT'].iloc[i]:
                    race = 'Hispanic/Latino'
                if 'Mixed' in df['race_7_TEXT'].iloc[i]: 
                    race = 'More than one race'
                if 'Multi' in df['race_7_TEXT'].iloc[i]:
                    race = 'More than one race'
                if 'Biracial' in df['race_7_TEXT'].iloc[i]:
                    race = 'More than one race' 
                if ',' in df['race_7_TEXT'].iloc[i]: 
                    race = 'More than one race'
                if 'and ' in df['race_7_TEXT'].iloc[i]:
                    race='More than one race'
                
                else:
                    if 'Asian' in df.race_1.iloc[i]:
                        race = df.race_1.iloc[i]
                    
                    if 'Black or African American' in df.race_2.iloc[i]:
                        race = df.race_2.iloc[i]
                    
                    if 'Native American or Indian' in df.race_3.iloc[i]:
                        race = df.race_3.iloc[i]
                    
                    if 'Native Hawaiian or Pacific Islander' in df.race_4.iloc[i]:
                        race = df.race_4.iloc[i]
                    
                    if 'White' in df.race_5.iloc[i]:
                        race = df.race_5.iloc[i]
                    
                    if 'Prefer not to say' in df.race_6.iloc[i]:
                        race = df.race_6.iloc[i]
                
                    if 'I prefer to self describe' in df.race_7.iloc[i]:
                        race = 'Other'
        Race.append(race)
    df['Race'] = Race
    return df
