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


### Data Type Numeric ### 
crse = ['ts_crse_after_7/30_1', 'ts_crse_after_7/30_2', 'ts_crse_after_7/30_3',
       'ts_crse_after_7/30_4', 'non_ts_crse_a_7/30_1', 'non_ts_crse_a_7/30_2',
       'non_ts_crse_a_7/30_3', 'non_ts_crse_a_7/30_4']

num = {'5- Very often':5, '4- Often':4, '3- Sometimes':3, '2- Rarely':2, '1- Never':1 }

#change text to numeric
for col in crse:
    df = df.replace({col: num})
    df[col]=df[col].apply(pd.to_numeric)


### Scoring CRSE ###
# only count 4s and 5s 
scoring = {5:1, 4:1, 3:0, 2:0, 1:0, 0:0}

qscores = []
for i in crse:
    column = df[i]
    qscore = column.map(scoring)
    qscores.append(qscore)
qscores = pd.DataFrame(qscores).transpose()

#rename columns to be added to df
rename ={'ts_crse_after_7/30_1':'crse1', 'ts_crse_after_7/30_2':'crse2', 'ts_crse_after_7/30_3':'crse3',
       'ts_crse_after_7/30_4':'crse4', 'non_ts_crse_a_7/30_1':'ncrse1', 'non_ts_crse_a_7/30_2':'ncrse2',
       'non_ts_crse_a_7/30_3':'ncrse3', 'non_ts_crse_a_7/30_4':'ncrse4'}
qscores.rename(columns=rename, inplace=True)

#add scores to df
df = pd.concat([df,qscores], axis=1)

### Calculating CRSE % Often/Very Often (4s and 5s) ###
CRSE = (100*(df[['crse1', 'crse2', 'crse3', 'crse4','ncrse1', 'ncrse2', 'ncrse3',
       'ncrse4']].sum(axis=1))/4).round(2)
crse1 = (100*(df[['crse1','ncrse1']].sum(axis=1))).round(2)
crse2 = (100*(df[['crse2','ncrse2']].sum(axis=1))).round(2)
crse3 = (100*(df[['crse3','ncrse3']].sum(axis=1))).round(2)
crse4 = (100*(df[['crse4','ncrse4']].sum(axis=1))).round(2)

df['crse_score']=CRSE
df['crse1']=crse1
df['crse2']=crse2
df['crse3']=crse3
df['crse4']=crse4

### Disaggregating CRSE Scores ###

#content area
df.groupby('content_area').agg({'crse_score':['mean'],'crse1':['mean'],'crse2':['mean'],'crse3':['mean'],'crse4':['mean']}).transpose()
#grade band
df.groupby('grade').agg({'crse_score':['mean'],'crse1':['mean'],'crse2':['mean'],'crse3':['mean'],'crse4':['mean']}).transpose()
# racial identity
df.groupby('Race').agg({'crse_score':['mean'],'crse1':['mean'],'crse2':['mean'],'crse3':['mean'],'crse4':['mean']}).transpose()
# role
df.groupby('role').agg({'crse_score':['mean'],'crse1':['mean'],'crse2':['mean'],'crse3':['mean'],'crse4':['mean']}).transpose()
