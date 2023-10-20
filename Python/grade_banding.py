#Grade Banding function in Python

### Educator Survey ###
def grade_col(df):
    
    ## FOR GRADE REPORTING -- IF SPAN ACROSS GRADE LEVELS, REPORT HIGHEST GRADE LEVEL
    ### GRADE BANDS: K-2, 3-5, 6-8, 9-12, OTHER, UNSPECIFIED
    
    
    #get the count of grade levels entered by each participant
    
    columns = ['grade_level_1', 'grade_level_2', 'grade_level_3',
       'grade_level_4', 'grade_level_5', 'grade_level_6', 'grade_level_7',
       'grade_level_8', 'grade_level_9', 'grade_level_10', 'grade_level_11',
       'grade_level_12', 'grade_level_13', 'grade_level_14']
    
    check = df[columns]
    count = []
    for i in range(len(df)):
        not_empty = check.count(axis=1).iloc[i]
        count.append(not_empty)
    df['grade_count']=count
    
    #change grade columns to string data type
    for col in columns:
        df[col]=df[col].astype(str)
        
    #create if, elif, else structure to divide out the data into grade bands
    grade = []
    for i in range(len(df)):
        if df.grade_count.iloc[i] == 0:
            grade_band = 'Unspecified'
        elif df.grade_count.iloc[i] == 1:
            if "12" in df.grade_level_13.iloc[i]:
                grade_band = '9-12'
            elif "11" in df.grade_level_12.iloc[i]:
                grade_band = '9-12'
            elif "10" in df.grade_level_11.iloc[i]:
                grade_band = '9-12'
            elif "9" in df.grade_level_10.iloc[i]:
                grade_band = '9-12'
            elif "8" in df.grade_level_9.iloc[i]:
                grade_band='6-8' 
            elif "7" in df.grade_level_8.iloc[i]:
                grade_band='6-8'
            elif "6" in df.grade_level_7.iloc[i]:
                grade_band='6-8'
            elif "5" in df.grade_level_6.iloc[i]:
                grade_band='3-5'
            elif "4" in df.grade_level_5.iloc[i]:
                grade_band='3-5'
            elif "3" in df.grade_level_4.iloc[i]:
                grade_band='3-5'
            elif "2" in df.grade_level_3.iloc[i]:
                grade_band = 'K-2'
            elif "1" in df.grade_level_2.iloc[i]:
                grade_band = 'K-2'
            elif "K" in df.grade_level_1.iloc[i]:
                grade_band = 'K-2'
            elif "Other" in df.grade_level_14.iloc[i]:
                grade_band = "Other"
        elif df.grade_count.iloc[i] >=2:
            if "12" in df.grade_level_13.iloc[i]:
                grade_band = '9-12'
            elif "11" in df.grade_level_12.iloc[i]:
                grade_band = '9-12'
            elif "10" in df.grade_level_11.iloc[i]:
                grade_band = '9-12'
            elif "9" in df.grade_level_10.iloc[i]:
                grade_band = '9-12'
            elif "8" in df.grade_level_9.iloc[i]:
                grade_band='6-8' 
            elif "7" in df.grade_level_8.iloc[i]:
                grade_band='6-8'
            elif "6" in df.grade_level_7.iloc[i]:
                grade_band='6-8'
            elif "5" in df.grade_level_6.iloc[i]:
                grade_band='3-5'
            elif "4" in df.grade_level_5.iloc[i]:
                grade_band='3-5'
            elif "3" in df.grade_level_4.iloc[i]:
                grade_band='3-5'
            elif "2" in df.grade_level_3.iloc[i]:
                grade_band = 'K-2'
            elif "1" in df.grade_level_2.iloc[i]:
                grade_band = 'K-2'
            elif "K" in df.grade_level_1.iloc[i]:
                grade_band = 'K-2'
                
        grade.append(grade_band)
    df['grade']=grade

    return df

### Classroom Observations ###
def grade_col(df):
    
    ## FOR GRADE REPORTING -- IF SPAN ACROSS GRADE LEVELS, REPORT HIGHEST GRADE LEVEL
    ### GRADE BANDS: K-2, 3-5, 6-8, 9-12, OTHER, UNSPECIFIED
    
    
    #get the count of grade levels entered by each participant
    
    columns = ['grade_level_instr_1', 'grade_level_instr_2', 'grade_level_instr_3',
       'grade_level_instr_4', 'grade_level_instr_5', 'grade_level_instr_6', 'grade_level_instr_7',
       'grade_level_instr_8', 'grade_level_instr_9', 'grade_level_instr_10', 'grade_level_instr_11',
       'grade_level_instr_12', 'grade_level_instr_13', 'grade_level_instr_14']
    
    check = df[columns]
    count = []
    for i in range(len(df)):
        not_empty = check.count(axis=1).iloc[i]
        count.append(not_empty)
    df['grade_count']=count
    
    #change grade columns to string data type
    for col in columns:
        df[col]=df[col].astype(str)
        
    #create if, elif, else structure to divide out the data into grade bands
    grade = []
    for i in range(len(df)):
        if df.grade_count.iloc[i] == 0:
            grade_band = 'Unspecified'
        elif df.grade_count.iloc[i] == 1:
            if "12" in df.grade_level_instr_14.iloc[i]:
                grade_band = '9-12'
            elif "11" in df.grade_level_instr_13.iloc[i]:
                grade_band = '9-12'
            elif "10" in df.grade_level_instr_12.iloc[i]:
                grade_band = '9-12'
            elif "9" in df.grade_level_instr_11.iloc[i]:
                 grade_band = '9-12'
            elif "8" in df.grade_level_instr_10.iloc[i]:
                 grade_band='6-8' 
            elif "7" in df.grade_level_instr_9.iloc[i]:
                grade_band='6-8' 
            elif "6" in df.grade_level_instr_8.iloc[i]:
                grade_band='6-8'
            elif "5" in df.grade_level_instr_7.iloc[i]:
                grade_band='3-5'
            elif "4" in df.grade_level_instr_6.iloc[i]:
                grade_band='3-5'
            elif "3" in df.grade_level_instr_5.iloc[i]:
                grade_band='3-5'
            elif "2" in df.grade_level_instr_4.iloc[i]:
                grade_band = 'K-2'
            elif "1" in df.grade_level_instr_3.iloc[i]:
                grade_band = 'K-2'
            elif "K" in df.grade_level_instr_2.iloc[i]:
                grade_band = 'K-2'
            elif "Pre-K" in df.grade_level_instr_1.iloc[i]:
                grade_band = 'Other'

        elif df.grade_count.iloc[i] >=2:
            if "12" in df.grade_level_instr_14.iloc[i]:
                grade_band = '9-12'
            elif "11" in df.grade_level_instr_13.iloc[i]:
                grade_band = '9-12'
            elif "10" in df.grade_level_instr_12.iloc[i]:
                grade_band = '9-12'
            elif "9" in df.grade_level_instr_11.iloc[i]:
                 grade_band = '9-12'
            elif "8" in df.grade_level_instr_10.iloc[i]:
                 grade_band='6-8' 
            elif "7" in df.grade_level_instr_9.iloc[i]:
                grade_band='6-8' 
            elif "6" in df.grade_level_instr_8.iloc[i]:
                grade_band='6-8'
            elif "5" in df.grade_level_instr_7.iloc[i]:
                grade_band='3-5'
            elif "4" in df.grade_level_instr_6.iloc[i]:
                grade_band='3-5'
            elif "3" in df.grade_level_instr_5.iloc[i]:
                grade_band='3-5'
            elif "2" in df.grade_level_instr_4.iloc[i]:
                grade_band = 'K-2'
            elif "1" in df.grade_level_instr_3.iloc[i]:
                grade_band = 'K-2'
            elif "K" in df.grade_level_instr_2.iloc[i]:
                grade_band = 'K-2'
                
        grade.append(grade_band)
    df['grade']=grade

    return df
