# Pull IPG survey
df = get_survey('SV_0BSnkV9TVXK1hjw')
# clean import id, subset >= 7/1/2023, include only Finished responses, reset index
df = clean(df)

### Q1 Data Cleaning ###

# subset to only use FSOT, K12 math, K12 ela
df = pd.DataFrame(df[(df.ipg_rubric=='K-12: Mathematics IPG')|(df.ipg_rubric=='K-12: ELA/Literacy IPG (please use this tool for K-2 observations that are not focused on foundational skills)')|(df.ipg_rubric=='Foundational Skills Observational Tool - FSOT')])
# subset to include < 10/1/2023
df = pd.DataFrame(df[(df['EndDate']<'10-01-2023')])
# subset to exclude 'ongoing' service observations
df = pd.DataFrame(df[df.direct_to_ts_obs != 'Ongoing'])

df = df.reset_index()
df = df.drop(columns=['index'])

### Grade Banding ###
#use the IPG grade band function

df = ipg_grade_col(df)


### Check for NaNs ###

#create a non NaN counter that counts non empty entries across rubric columns for a single observation
# this will be used to logic positive indicator and question n sizes later
columns=['k12_m_ca1a', 'k12_m_ca1b', 'k12_m_ca1c',
       'k12_m_ca2a', 'k12_m_ca2b', 'k12_m_ca2c', 'k12_m_ca2d', 'k12_m_ca3a', 'k12_m_ca3b', 'k12_m_ca3c',
       'k12_m_ca3d', 'k12_m_ca3e','k12_ela_ca1a', 'k12_ela_ca1b', 'k12_ela_ca1c',
       'k12_ela_ca2a', 'k12_ela_ca2b', 'k12_ela_ca2c', 'k12_ela_ca2d', 'k12_ela_ca3a', 
           'k12_ela_ca3b', 'k12_ela_ca3c','k12_ela_ca3d', 'k12_ela_ca3e', 'k12_ela_ca3f',
          'fsot_ac1','fsot_ac2','fsot_td1', 'fsot_td2', 'fsot_td3', 'fsot_td4',
       'fsot_sp1', 'fsot_sp2', 'fsot_sp3', 'fsot_sp4','fsot_ad1', 'fsot_ad2']
check = df[columns]
nancheck= []
for i in range(len(check)):
    nonempty = check.count(axis=1).iloc[i]
    nancheck.append(nonempty)
df['nancheck']=nancheck  

### Positive Indicators and Question Count by Tool ###

#convert rubric columns to strings
for col in columns:
    df[col]=df[col].astype(str)

# Math IPG -- If 'Yes', 3, or 4 then Pos Ind
# Math IPG -- if 'not observed'/'not applicable'/'n/a' then detract from question count

# initialize pos ind and question counters
k12m = []
k12mn=[]
for i in range(len(df)):
 # if all NaNs then score and nsize will not affect calculations
    if df.nancheck.iloc[i]==0:
        score=0
        nsize=0
  # otherwise, run the pos indicator and question counter
    else:
        score = 0
        nsize = df.nancheck.iloc[i]
        if df.ipg_rubric.iloc[i] == 'K-12: Mathematics IPG':
            if "Yes" in df.k12_m_ca1a.iloc[i]:
                score += 1
            if "Yes" in df.k12_m_ca1b.iloc[i]:
                score += 1
            if "Yes" in df.k12_m_ca1c.iloc[i]:
                score += 1
            if "3" in df.k12_m_ca2a.iloc[i]:
                score += 1
            if "4" in df.k12_m_ca2a.iloc[i]:
                score += 1
            if "observed" in df.k12_m_ca2a.iloc[i]:
                nsize -= 1
            if "3" in df.k12_m_ca2b.iloc[i]:
                score += 1
            if "4" in df.k12_m_ca2b.iloc[i]:
                score += 1
            if "observed" in df.k12_m_ca2b.iloc[i]:
                nsize -= 1
            if "3" in df.k12_m_ca2c.iloc[i]:
                score += 1
            if "4" in df.k12_m_ca2c.iloc[i]:
                score += 1
            if "observed" in df.k12_m_ca2c.iloc[i]:
                nsize -= 1
            if "3" in df.k12_m_ca2d.iloc[i]:
                score += 1
            if "4" in df.k12_m_ca2d.iloc[i]:
                score += 1
            if "observed" in df.k12_m_ca2d.iloc[i]:
                nsize -= 1
            if "3" in df.k12_m_ca3a.iloc[i]:
                score += 1
            if "4" in df.k12_m_ca3a.iloc[i]:
                score += 1
            if "observed" in df.k12_m_ca3a.iloc[i]:
                nsize -= 1
            if "3" in df.k12_m_ca3b.iloc[i]:
                score += 1
            if "4" in df.k12_m_ca3b.iloc[i]:
                score += 1
            if "observed" in df.k12_m_ca3b.iloc[i]:
                nsize -= 1
            if "3" in df.k12_m_ca3c.iloc[i]:
                score += 1
            if "4" in df.k12_m_ca3c.iloc[i]:
                score += 1
            if "observed" in df.k12_m_ca3c.iloc[i]:
                nsize -= 1
            if "3" in df.k12_m_ca3d.iloc[i]:
                score += 1
            if "4" in df.k12_m_ca3d.iloc[i]:
                score += 1
            if "observed" in df.k12_m_ca3d.iloc[i]:
                nsize -= 1
            if "3" in df.k12_m_ca3e.iloc[i]:
                score += 1
            if "4" in df.k12_m_ca3e.iloc[i]: 
                score += 1
            if "observed" in df.k12_m_ca3e.iloc[i]:
                nsize -= 1
        else:
          # non K12 math rubrics get a score and n size of 0
            nsize=0

    k12m.append(score)
    k12mn.append(nsize)


# ELA IPG -- If 'Yes', 3, or 4 then Pos Ind
# ELA IPG -- if 'not observed'/'not applicable'/'n/a' then detract from question count

# initialize pos ind and question counters

k12e = []
k12en=[]
for i in range(len(df)):
 # if all NaNs then score and nsize will not affect calculations
    if df.nancheck.iloc[i]==0:
        score=0
        nsize=0
  # otherwise, run the pos indicator and question counter
    else:
        score = 0  
        nsize = df.nancheck.iloc[i]
        if df.ipg_rubric.iloc[i] == 'K-12: ELA/Literacy IPG (please use this tool for K-2 observations that are not focused on foundational skills)':
            if "Yes" in df.k12_ela_ca1a.iloc[i]:
                score += 1
            if "Yes" in df.k12_ela_ca1b.iloc[i]:
                score += 1
            if "observed" in df.k12_ela_ca1b.iloc[i]:
                nsize -= 1
            if "Yes" in df.k12_ela_ca1c.iloc[i]:
                score += 1
            if "observed" in df.k12_ela_ca1c.iloc[i]:
                nsize -= 1
            if "3" in df.k12_ela_ca2a.iloc[i]:
                score += 1
            if "4" in df.k12_ela_ca2a.iloc[i]:
                score += 1
            if "observed" in df.k12_ela_ca2a.iloc[i]:
                nsize -= 1
            if "N/A" in df.k12_ela_ca2a.iloc[i]:
                nsize -= 1
            if "3" in df.k12_ela_ca2b.iloc[i]:
                score += 1
            if "4" in df.k12_ela_ca2b.iloc[i]:
                score += 1
            if "observed" in df.k12_ela_ca2b.iloc[i]:
                nsize -= 1
            if "N/A" in df.k12_ela_ca2b.iloc[i]:
                nsize -= 1
            if "3" in df.k12_ela_ca2c.iloc[i]:
                score += 1
            if "4" in df.k12_ela_ca2c.iloc[i]:
                score += 1
            if "observed" in df.k12_ela_ca2c.iloc[i]:
                nsize -= 1
            if "N/A" in df.k12_ela_ca2c.iloc[i]:
                nsize -= 1
            if "3" in df.k12_ela_ca2d.iloc[i]:
                score += 1
            if "4" in df.k12_ela_ca2d.iloc[i]:
                score += 1
            if "observed" in df.k12_ela_ca2d.iloc[i]:
                nsize -= 1
            if "N/A" in df.k12_ela_ca2d.iloc[i]:
                nsize -= 1
            if "3" in df.k12_ela_ca3a.iloc[i]:
                score += 1
            if "4" in df.k12_ela_ca3a.iloc[i]:
                score += 1
            if "observed" in df.k12_ela_ca3a.iloc[i]:
                nsize -= 1
            if "3" in df.k12_ela_ca3b.iloc[i]:
                score += 1
            if "4" in df.k12_ela_ca3b.iloc[i]:
                score += 1
            if "observed" in df.k12_ela_ca3b.iloc[i]:
                nsize -= 1
            if "3" in df.k12_ela_ca3c.iloc[i]:
                score += 1
            if "4" in df.k12_ela_ca3c.iloc[i]:
                score += 1
            if "observed" in df.k12_ela_ca3c.iloc[i]:
                nsize -= 1
            if "3" in df.k12_ela_ca3d.iloc[i]:
                score += 1
            if "4" in df.k12_ela_ca3d.iloc[i]:
                score += 1
            if "observed" in df.k12_ela_ca3d.iloc[i]:
                nsize -= 1
            if "3" in df.k12_ela_ca3e.iloc[i]:
                score += 1
            if "4" in df.k12_ela_ca3e.iloc[i]:
                score += 1
            if "observed" in df.k12_ela_ca3e.iloc[i]:
                nsize -= 1
            if "3" in df.k12_ela_ca3f.iloc[i]:
                score += 1
            if "4" in df.k12_ela_ca3f.iloc[i]:
                score += 1
            if "observed" in df.k12_ela_ca3f.iloc[i]:
                nsize -= 1
        else:
          # non K12 ela rubrics get a score and n size of 0
            nsize=0

    k12e.append(score)
    k12en.append(nsize)


# FSOT -- If 3, or 4 then Pos Ind (for ad1,2 if 2 or 3 then Pos Ind)
# FSOT -- if 'not observed'/'not applicable'/'n/a' then detract from question count

# initialize pos ind and question counters
fsot = []
fsotn = []
for i in range(len(df)):
  # if all NaNs then score and nsize will not affect calculations
    if df.nancheck.iloc[i]==0:
        score=0
        nsize=0
  # otherwise, run the pos indicator and question counter
    else:
        score = 0 
        nsize = df.nancheck.iloc[i]
        if df.ipg_rubric.iloc[i] == 'Foundational Skills Observational Tool - FSOT':
            if "3" in df.fsot_ac1.iloc[i]:
                score += 1
            if "4" in df.fsot_ac1.iloc[i]:
                score += 1
            if "observed" in df.fsot_ac1.iloc[i]:
                nsize -= 1
            if "applicable" in df.fsot_ac1.iloc[i]:
                nsize -= 1
            if "3" in df.fsot_ac2.iloc[i]:
                score += 1
            if "4" in df.fsot_ac2.iloc[i]:
                score += 1
            if "observed" in df.fsot_ac2.iloc[i]:
                nsize -= 1
            if "applicable" in df.fsot_ac2.iloc[i]:
                nsize -= 1
            if "3" in df.fsot_td1.iloc[i]:
                score += 1
            if "4" in df.fsot_td1.iloc[i]:
                score += 1
            if "observed" in df.fsot_td1.iloc[i]:
                nsize -= 1
            if "applicable" in df.fsot_td1.iloc[i]:
                nsize -= 1
            if "3" in df.fsot_td2.iloc[i]:
                score += 1
            if "4" in df.fsot_td2.iloc[i]:
                score += 1
            if "observed" in df.fsot_td2.iloc[i]:
                nsize -= 1
            if "applicable" in df.fsot_td2.iloc[i]:
                nsize -= 1
            if "3" in df.fsot_td3.iloc[i]:
                score += 1
            if "4" in df.fsot_td3.iloc[i]:
                score += 1
            if "observed" in df.fsot_td3.iloc[i]:
                nsize -= 1
            if "applicable" in df.fsot_td3.iloc[i]:
                nsize -= 1
            if "3" in df.fsot_td4.iloc[i]:
                score += 1
            if "4" in df.fsot_td4.iloc[i]:
                score += 1
            if "observed" in df.fsot_td4.iloc[i]:
                nsize -= 1
            if "applicable" in df.fsot_td4.iloc[i]:
                nsize -= 1
            if "3" in df.fsot_sp1.iloc[i]:
                score += 1
            if "observed" in df.fsot_sp1.iloc[i]:
                nsize -= 1
            if "applicable" in df.fsot_sp1.iloc[i]:
                nsize -= 1
            if "3" in df.fsot_sp2.iloc[i]:
                score += 1
            if "observed" in df.fsot_sp2.iloc[i]:
                nsize -= 1
            if "applicable" in df.fsot_sp2.iloc[i]:
                nsize -= 1
            if "3" in df.fsot_sp3.iloc[i]:
                score += 1
            if "observed" in df.fsot_sp3.iloc[i]:
                nsize -= 1
            if "applicable" in df.fsot_sp3.iloc[i]:
                nsize -= 1
            if "3" in df.fsot_sp4.iloc[i]:
                score += 1
            if "observed" in df.fsot_sp4.iloc[i]:
                nsize -= 1
            if "applicable" in df.fsot_sp4.iloc[i]:
                nsize -= 1
            if "4" in df.fsot_sp1.iloc[i]:
                score += 1
            if "4" in df.fsot_sp2.iloc[i]:
                score += 1
            if "4" in df.fsot_sp3.iloc[i]:
                score += 1
            if "4" in df.fsot_sp4.iloc[i]:
                score += 1
            if "3" in df.fsot_ad1.iloc[i]:
                score += 1
            if "2" in df.fsot_ad1.iloc[i]:
                score += 1
            if "3" in df.fsot_ad2.iloc[i]:
                score += 1
            if "2" in df.fsot_ad2.iloc[i]:
                score += 1
            if "observed" in df.fsot_ad1.iloc[i]:
                nsize -= 1
            if "observed" in df.fsot_ad2.iloc[i]:
                nsize -= 1
            if "applicable" in df.fsot_ad1.iloc[i]:
                nsize -= 1
            if "applicable" in df.fsot_ad2.iloc[i]:
                nsize -= 1
        else:
          #non fsot rubric will get a score and nsize of 0
            nsize=0
    fsot.append(score)
    fsotn.append(nsize)

# sum across the positive indicators and question n sizes for each individual
# note: since each observation uses 1 tool, the pos ind and q n sizes are at most 12 or 13 (tool dependent)
lists_of_lists = [k12m,k12e,fsot]
list2 = [k12mn,k12en,fsotn]
pos_ind=[sum(x) for x in zip(*lists_of_lists)]
nsize = [sum(x) for x in zip(*list2)]

# add pos_ind and nsize columns to df
df['pos_ind']=pos_ind
df['nsize']=nsize

### Disaggreating % Positive Indicators For Dataset ###
# grade band
(df['pos_ind'].groupby(df.grade).sum())*100/(df['nsize'].groupby(df.grade).sum())
# content area
(df['pos_ind'].groupby(df.content_area).sum())*100/(df['nsize'].groupby(df.content_area).sum())
