# Pull ed survey
df = get_survey('SV_8vrKtPDtqQFbiBM')
# clean import id, subset >= 7/1/2023, include only Finished responses, reset index
df = clean(df)

### Q1 Data Cleaning ###

# subset to include < 10/1/2023
df = pd.DataFrame(df[(df['EndDate']<'10-01-2023')])
df = df.reset_index()
df = df.drop(columns=['index'])

### Grade Banding ###
df = grade_col(df)
### Racial Identity ###
df = race_col(df)


### Data Type Numeric and Reverse Coding Questions ###
columns = ['mindsets_ts_1_1','mindsets_ts_1_21','mindsets_ts_1_22','mindsets_ts_1_23',
     'mindsets_ts_1_4','mindsets_ts_1_5','mindsets_ts_1_6','mindsets_ts_1_7','mindsets_ts_1_13',
     'mindsets_ts_1_16','mindsets_ts_1_19','mindsets_ts_1_20',
          'non_ts_mindsets_1','non_ts_mindsets_2','non_ts_mindsets_3','non_ts_mindsets_4',
          'non_ts_mindsets_7','non_ts_mindsets_8','non_ts_mindsets_9','non_ts_mindsets_10','non_ts_mindsets_11',
          'non_ts_mindsets_12','non_ts_mindsets_13','non_ts_mindsets_14']

rc = ['mindsets_ts_1_1','mindsets_ts_1_21','mindsets_ts_1_22','mindsets_ts_1_23',
     'non_ts_mindsets_1','non_ts_mindsets_2','non_ts_mindsets_3','non_ts_mindsets_4']
he = ['mindsets_ts_1_4','mindsets_ts_1_5','mindsets_ts_1_6','mindsets_ts_1_7','mindsets_ts_1_13',
     'non_ts_mindsets_7','non_ts_mindsets_8','non_ts_mindsets_9','non_ts_mindsets_10','non_ts_mindsets_11']
gm = ['mindsets_ts_1_16','mindsets_ts_1_19','mindsets_ts_1_20',
     'non_ts_mindsets_12','non_ts_mindsets_13','non_ts_mindsets_14']

reverse = ['mindsets_ts_1_1','mindsets_ts_1_21','mindsets_ts_1_22','mindsets_ts_1_23','mindsets_ts_1_4','mindsets_ts_1_5',
           'mindsets_ts_1_6','mindsets_ts_1_13','mindsets_ts_1_16','mindsets_ts_1_19','mindsets_ts_1_20',
          'non_ts_mindsets_1','non_ts_mindsets_2','non_ts_mindsets_3','non_ts_mindsets_4','non_ts_mindsets_7',
          'non_ts_mindsets_8','non_ts_mindsets_9','non_ts_mindsets_11','non_ts_mindsets_12',
          'non_ts_mindsets_13','non_ts_mindsets_14']

#dictionary to reassign values
num = {'5 - Strongly agree':5, '4 - Agree':4, '3 - Neither agree nor disagree':3, '2 - Disagree':2, '1 - Strongly disagree':1 }

#change text to numeric
for col in columns:
    df = df.replace({col: num})
    df[col]=df[col].apply(pd.to_numeric)
    
#reverse score required columns
for i in reverse:  
    high = 6
    df[i]= (high-df[i]).mod(6)

### Creating Scored Dataframe ###
#ed survey creating quintile scores

scoring = {5:1, 4:0.75, 3:0.5, 2:0.25, 1:0, 0:0}

qscores = []
for i in columns:
    column = df[i]
    qscore = column.map(scoring)
    qscores.append(qscore)
qscores = pd.DataFrame(qscores).transpose()

# rename scored columns to add to df
rename = {'mindsets_ts_1_1':'rc1', 'mindsets_ts_1_21':'rc2', 'mindsets_ts_1_22':'rc3',
       'mindsets_ts_1_23':'rc4', 'mindsets_ts_1_4':'he1', 'mindsets_ts_1_5':'he2',
       'mindsets_ts_1_6':'he3', 'mindsets_ts_1_7':'he4', 'mindsets_ts_1_13':'he5',
       'mindsets_ts_1_16':'gm1', 'mindsets_ts_1_19':'gm2', 'mindsets_ts_1_20':'gm3',
       'non_ts_mindsets_1':'nrc1', 'non_ts_mindsets_2':'nrc2', 'non_ts_mindsets_3':'nrc3',
       'non_ts_mindsets_4':'nrc4', 'non_ts_mindsets_7':'nhe1', 'non_ts_mindsets_8':'nhe2',
       'non_ts_mindsets_9':'nhe3', 'non_ts_mindsets_10':'nhe4', 'non_ts_mindsets_11':'nhe5',
       'non_ts_mindsets_12':'ngm1', 'non_ts_mindsets_13':'ngm2', 'non_ts_mindsets_14':'ngm3'}

qscores.rename(columns=rename, inplace=True)

# add quintile scores to df
df = pd.concat([df,qscores], axis=1)

### Calculating Mindset Scores ###

# calculate the score for each individual overall and by construct category
RC = (100*(df[['rc1','rc2','rc3','rc4','nrc1','nrc2','nrc3','nrc4']].sum(axis=1))/4) 
GM = (100*(df[['gm1','gm2','gm3','ngm1','ngm2','ngm3']].sum(axis=1))/3)
HE = (100*(df[['he1', 'he2', 'he3','he4','he5','nhe1', 'nhe2', 'nhe3','nhe4','nhe5']].sum(axis=1))/5)
overall = ((RC+GM+HE)/3)

df['overall_score']=overall
df['rc_score']=RC
df['he_score']=HE
df['gm_score']=GM

### Disaggregated Average Scores ###

# grade band 
df.groupby(df.grade).agg({'overall_score':['mean'],'gm_score':['mean'],'he_score':['mean'],'rc_score':['mean']}).transpose()
# content area
df.groupby(df.content_area).agg({'overall_score':['mean'],'gm_score':['mean'],'he_score':['mean'],'rc_score':['mean']}).transpose()
#racial identity
df.groupby(df.Race).agg({'overall_score':['mean'],'gm_score':['mean'],'he_score':['mean'],'rc_score':['mean']}).transpose()
# role -- only report teacher
df.groupby(df.content_area).agg({'overall_score':['mean'],'gm_score':['mean'],'he_score':['mean'],'rc_score':['mean']}).transpose()

