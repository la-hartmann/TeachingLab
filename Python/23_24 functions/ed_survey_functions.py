def survey_codes():
    codes = {'Educator Survey':'SV_8vrKtPDtqQFbiBM', 'IPG':'SV_0BSnkV9TVXK1hjw', 'D13_Caregiver':'SV_e4ByNmGiIOk8ZYq',
            'D13_Educator':'SV_bK5AlCjMPAgHghg', 'D13_Student':'SV_9LisDEvQ3oUVCGq'}
    return codes

def get_survey(qcode):
    from QualtricsAPI.Setup import Credentials
    from QualtricsAPI.Survey import Responses
    #Credentials from Teaching Lab Research account
    Credentials().qualtrics_api_credentials(token='kPvIQdFYT3zUVJJr2DVHXTRFXnvhlgVuwu3S6A3G',data_center='teachinglab.iad1')

    #Create an instance
    r = Responses()
    #creating df from survey code, without recode values
    df = r.get_survey_responses(survey= qcode, useLabels=True)
    return df

def clean(data, start, end):
    #drop the import label and question rows
    data.drop(index=data.index[:2], inplace=True)
    #change the data type to date instead of string 
    data['RecordedDate']=pd.to_datetime(data['RecordedDate'])
    #subset the data to only include Finished surveys completed after start and before end date
    df = pd.DataFrame(data[(data.RecrodedDate >= start)&(data.RecordedDate <= end)])
    df = pd.DataFrame(df[(df['Finished']==True)|(df['Finished']=='True')])
    df = df.reset_index()
    df = df.drop(columns=['index'])
    return df

def race_col(df):
    
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
    
    #create new column that is all encompassing of the racial identity data
    Race = []
    for i in range(len(df)):
        if df['ethnicity'].iloc[i] == 'Hispanic or Latino':
            race = 'Hispanic/Latino'
        else:
            if df.race_count.iloc[i]>1:
                race = 'More than one race'
            
            if df.race_count.iloc[i]<=1:
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

def race_dist(df):
    return [df['Race'].value_counts().sum(),df['Race'].value_counts(),round((df['Race'].value_counts())*100/len(df),2)]

def content_dist(df):
    return [df['content_area'].value_counts().sum(),df['content_area'].value_counts(),round(df['content_area'].value_counts()*100/(df['content_area'].value_counts().sum()),2)]

def role_dist(df):
    return [df.role.value_counts().sum(),df.role.value_counts(),round(df.role.value_counts()*100/len(df),2)]

def gender_dist(df):
    return [df.gender.value_counts().sum(),df.gender.value_counts(),round(df.gender.value_counts()*100/len(df),2)]

def tl_part(df):
    return [df.tl_pl_participation.value_counts().sum(),df.tl_pl_participation.value_counts(),round(df.tl_pl_participation.value_counts()*100/df.tl_pl_participation.value_counts().sum(),2)]

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

def grade_dist(df):
    return [df.grade.value_counts().sum(),df.grade.value_counts(),round(df.grade.value_counts()*100/len(df),2)]

def exp_dist(df):
    return [df.teaching_experience.value_counts().sum(),df.teaching_experience.value_counts(),round(df.teaching_experience.value_counts()*100/df.teaching_experience.value_counts().sum(),2)]

def lablead_dist(df):
    return [df.lab_leader.value_counts().sum(),df.lab_leader.value_counts(),round(df.lab_leader.value_counts()*100/df.lab_leader.value_counts().sum(),2)]

def materials_use(df):
    return [df[(df.materials_1=='Use often (once or twice weekly)')|(df.materials_1=='Use everyday')].materials_1.value_counts().sum(),
                  round(df[(df.materials_1=='Use often (once or twice weekly)')|(df.materials_1=='Use everyday')].materials_1.value_counts().sum()*100/df.materials_1.value_counts().sum(),2),
                   df[(df.materials_2=='Use often (once or twice weekly)')|(df.materials_2=='Use everyday')].materials_2.value_counts().sum(),
                  round(df[(df.materials_2=='Use often (once or twice weekly)')|(df.materials_2=='Use everyday')].materials_2.value_counts().sum()*100/df.materials_2.value_counts().sum(),2),
            df[(df.materials_3=='Use often (once or twice weekly)')|(df.materials_3=='Use everyday')].materials_3.value_counts().sum(),
                  round(df[(df.materials_3=='Use often (once or twice weekly)')|(df.materials_3=='Use everyday')].materials_3.value_counts().sum()*100/df.materials_3.value_counts().sum(),2),
            df[(df.materials_4=='Use often (once or twice weekly)')|(df.materials_4=='Use everyday')].materials_4.value_counts().sum(),
                  round(df[(df.materials_4=='Use often (once or twice weekly)')|(df.materials_4=='Use everyday')].materials_4.value_counts().sum()*100/df.materials_4.value_counts().sum(),2)]

def lesson_mod(df):
    return [df[(df.lesson_modifications=='with modifications to less than half of a lesson plan')|(df.lesson_modifications=='with no or few modifications')].lesson_modifications.value_counts().sum(),
        round(df[(df.lesson_modifications=='with modifications to less than half of a lesson plan')|(df.lesson_modifications=='with no or few modifications')].lesson_modifications.value_counts().sum()*100/df.lesson_modifications.value_counts().sum(),2)]

def curr_perc(df):
    return [df[(df.curriculum_sch_dist_4=='Agree')|(df.curriculum_sch_dist_4=='Strongly agree')].curriculum_sch_dist_4.value_counts().sum(),
        round(df[(df.curriculum_sch_dist_4=='Agree')|(df.curriculum_sch_dist_4=='Strongly agree')].curriculum_sch_dist_4.value_counts().sum()*100/df.curriculum_sch_dist_4.value_counts().sum(),2),
           df[(df.curriculum_sch_dist_6=='Agree')|(df.curriculum_sch_dist_6=='Strongly agree')].curriculum_sch_dist_6.value_counts().sum(),
        round(df[(df.curriculum_sch_dist_6=='Agree')|(df.curriculum_sch_dist_6=='Strongly agree')].curriculum_sch_dist_6.value_counts().sum()*100/df.curriculum_sch_dist_6.value_counts().sum(),2),
           df[(df.curriculum_sch_dist_3=='Agree')|(df.curriculum_sch_dist_3=='Strongly agree')].curriculum_sch_dist_3.value_counts().sum(),
        round(df[(df.curriculum_sch_dist_3=='Agree')|(df.curriculum_sch_dist_3=='Strongly agree')].curriculum_sch_dist_3.value_counts().sum()*100/df.curriculum_sch_dist_3.value_counts().sum(),2),
           df[(df.curriculum_sch_dist_2=='Agree')|(df.curriculum_sch_dist_2=='Strongly agree')].curriculum_sch_dist_2.value_counts().sum(),
        round(df[(df.curriculum_sch_dist_2=='Agree')|(df.curriculum_sch_dist_2=='Strongly agree')].curriculum_sch_dist_2.value_counts().sum()*100/df.curriculum_sch_dist_2.value_counts().sum(),2),
           df[(df.curriculum_sch_dist_5=='Agree')|(df.curriculum_sch_dist_5=='Strongly agree')].curriculum_sch_dist_5.value_counts().sum(),
        round(df[(df.curriculum_sch_dist_5=='Agree')|(df.curriculum_sch_dist_5=='Strongly agree')].curriculum_sch_dist_5.value_counts().sum()*100/df.curriculum_sch_dist_5.value_counts().sum(),2),
           df[(df.curriculum_sch_dist_1=='Agree')|(df.curriculum_sch_dist_1=='Strongly agree')].curriculum_sch_dist_1.value_counts().sum(),
        round(df[(df.curriculum_sch_dist_1=='Agree')|(df.curriculum_sch_dist_1=='Strongly agree')].curriculum_sch_dist_1.value_counts().sum()*100/df.curriculum_sch_dist_1.value_counts().sum(),2)]

def t_se(df):
    return [df[(df['ts_se_after_10/3_1']=='5 - Extremely confident')|(df['ts_se_after_10/3_1']=='4')]['ts_se_after_10/3_1'].value_counts().sum(),
           round(df[(df['ts_se_after_10/3_1']=='5 - Extremely confident')|(df['ts_se_after_10/3_1']=='4')]['ts_se_after_10/3_1'].value_counts().sum()*100/df['ts_se_after_10/3_1'].value_counts().sum(),2),
           df[(df['ts_se_after_10/3_3']=='5 - Extremely confident')|(df['ts_se_after_10/3_3']=='4')]['ts_se_after_10/3_3'].value_counts().sum(),
           round(df[(df['ts_se_after_10/3_3']=='5 - Extremely confident')|(df['ts_se_after_10/3_3']=='4')]['ts_se_after_10/3_3'].value_counts().sum()*100/df['ts_se_after_10/3_3'].value_counts().sum(),2),
           df[(df['ts_se_after_10/3_2']=='5 - Extremely confident')|(df['ts_se_after_10/3_2']=='4')]['ts_se_after_10/3_2'].value_counts().sum(),
           round(df[(df['ts_se_after_10/3_2']=='5 - Extremely confident')|(df['ts_se_after_10/3_2']=='4')]['ts_se_after_10/3_2'].value_counts().sum()*100/df['ts_se_after_10/3_2'].value_counts().sum(),2)]

def t_crse(df):
    return [df[(df['ts_crse_after_7/30_1']=='5- Very often')|(df['ts_crse_after_7/30_1']=='4- Often')]['ts_crse_after_7/30_1'].value_counts().sum(),
           round(df[(df['ts_crse_after_7/30_1']=='5- Very often')|(df['ts_crse_after_7/30_1']=='4- Often')]['ts_crse_after_7/30_1'].value_counts().sum()*100/df['ts_crse_after_7/30_1'].value_counts().sum(),2),
           df[(df['ts_crse_after_7/30_2']=='5- Very often')|(df['ts_crse_after_7/30_2']=='4- Often')]['ts_crse_after_7/30_2'].value_counts().sum(),
          round(df[(df['ts_crse_after_7/30_2']=='5- Very often')|(df['ts_crse_after_7/30_2']=='4- Often')]['ts_crse_after_7/30_2'].value_counts().sum()*100/df['ts_crse_after_7/30_2'].value_counts().sum(),2),
           df[(df['ts_crse_after_7/30_3']=='5- Very often')|(df['ts_crse_after_7/30_3']=='4- Often')]['ts_crse_after_7/30_3'].value_counts().sum(),
           round(df[(df['ts_crse_after_7/30_3']=='5- Very often')|(df['ts_crse_after_7/30_3']=='4- Often')]['ts_crse_after_7/30_3'].value_counts().sum()*100/df['ts_crse_after_7/30_3'].value_counts().sum(),2),
           df[(df['ts_crse_after_7/30_4']=='5- Very often')|(df['ts_crse_after_7/30_4']=='4- Often')]['ts_crse_after_7/30_4'].value_counts().sum(),
          round(df[(df['ts_crse_after_7/30_4']=='5- Very often')|(df['ts_crse_after_7/30_4']=='4- Often')]['ts_crse_after_7/30_4'].value_counts().sum()*100/df['ts_crse_after_7/30_4'].value_counts().sum(),2)]

def non_t_crse(df):
    return [df[(df['non_ts_crse_a_7/30_1']=='5- Very often')|(df['non_ts_crse_a_7/30_1']=='4- Often')]['non_ts_crse_a_7/30_1'].value_counts().sum(),
           round(df[(df['non_ts_crse_a_7/30_1']=='5- Very often')|(df['non_ts_crse_a_7/30_1']=='4- Often')]['non_ts_crse_a_7/30_1'].value_counts().sum()*100/df['non_ts_crse_a_7/30_1'].value_counts().sum(),2),
           df[(df['non_ts_crse_a_7/30_2']=='5- Very often')|(df['non_ts_crse_a_7/30_2']=='4- Often')]['non_ts_crse_a_7/30_2'].value_counts().sum(),
           round(df[(df['non_ts_crse_a_7/30_2']=='5- Very often')|(df['non_ts_crse_a_7/30_2']=='4- Often')]['non_ts_crse_a_7/30_2'].value_counts().sum()*100/df['non_ts_crse_a_7/30_2'].value_counts().sum(),2),
           df[(df['non_ts_crse_a_7/30_4']=='5- Very often')|(df['non_ts_crse_a_7/30_4']=='4- Often')]['non_ts_crse_a_7/30_4'].value_counts().sum(),
           round(df[(df['non_ts_crse_a_7/30_4']=='5- Very often')|(df['non_ts_crse_a_7/30_4']=='4- Often')]['non_ts_crse_a_7/30_4'].value_counts().sum()*100/df['non_ts_crse_a_7/30_4'].value_counts().sum(),2),
           df[(df['non_ts_crse_a_7/30_6']=='5- Very often')|(df['non_ts_crse_a_7/30_6']=='4- Often')]['non_ts_crse_a_7/30_6'].value_counts().sum(),
           round(df[(df['non_ts_crse_a_7/30_6']=='5- Very often')|(df['non_ts_crse_a_7/30_6']=='4- Often')]['non_ts_crse_a_7/30_6'].value_counts().sum()*100/df['non_ts_crse_a_7/30_6'].value_counts().sum(),2),
           df[(df['non_ts_crse_a_7/30_5']=='5- Very often')|(df['non_ts_crse_a_7/30_5']=='4- Often')]['non_ts_crse_a_7/30_5'].value_counts().sum(),
           round(df[(df['non_ts_crse_a_7/30_5']=='5- Very often')|(df['non_ts_crse_a_7/30_5']=='4- Often')]['non_ts_crse_a_7/30_5'].value_counts().sum()*100/df['non_ts_crse_a_7/30_5'].value_counts().sum(),2),
           df[(df['non_ts_crse_a_7/30_3']=='5- Very often')|(df['non_ts_crse_a_7/30_3']=='4- Often')]['non_ts_crse_a_7/30_3'].value_counts().sum(),
           round(df[(df['non_ts_crse_a_7/30_3']=='5- Very often')|(df['non_ts_crse_a_7/30_3']=='4- Often')]['non_ts_crse_a_7/30_3'].value_counts().sum()*100/df['non_ts_crse_a_7/30_3'].value_counts().sum(),2)]

def en_cond1(df):
    return [df[(df['school_environment_4']=='5 - Strongly agree')|(df['school_environment_4']=='4 - Agree')]['school_environment_4'].value_counts().sum(),
round(df[(df['school_environment_4']=='5 - Strongly agree')|(df['school_environment_4']=='4 - Agree')]['school_environment_4'].value_counts().sum()*100/df['school_environment_4'].value_counts().sum(),2),
           df[(df['school_environment_2']=='5 - Strongly agree')|(df['school_environment_2']=='4 - Agree')]['school_environment_2'].value_counts().sum(),
round(df[(df['school_environment_2']=='5 - Strongly agree')|(df['school_environment_2']=='4 - Agree')]['school_environment_2'].value_counts().sum()*100/df['school_environment_2'].value_counts().sum(),2),
           df[(df['school_environment_1']=='5 - Strongly agree')|(df['school_environment_1']=='4 - Agree')]['school_environment_1'].value_counts().sum(),
round(df[(df['school_environment_1']=='5 - Strongly agree')|(df['school_environment_1']=='4 - Agree')]['school_environment_1'].value_counts().sum()*100/df['school_environment_1'].value_counts().sum(),2)]

def en_cond2(df):
    return [df[(df['ts_perceptions_sl_4']=='All the time')|(df['ts_perceptions_sl_4']=='Often')]['ts_perceptions_sl_4'].value_counts().sum(),
round(df[(df['ts_perceptions_sl_4']=='All the time')|(df['ts_perceptions_sl_4']=='Often')]['ts_perceptions_sl_4'].value_counts().sum()*100/df['ts_perceptions_sl_4'].value_counts().sum(),2),
           df[(df['ts_perceptions_sl_2']=='All the time')|(df['ts_perceptions_sl_2']=='Often')]['ts_perceptions_sl_2'].value_counts().sum(),
round(df[(df['ts_perceptions_sl_2']=='All the time')|(df['ts_perceptions_sl_2']=='Often')]['ts_perceptions_sl_2'].value_counts().sum()*100/df['ts_perceptions_sl_2'].value_counts().sum(),2),
           df[(df['ts_perceptions_sl_3']=='All the time')|(df['ts_perceptions_sl_3']=='Often')]['ts_perceptions_sl_3'].value_counts().sum(),
round(df[(df['ts_perceptions_sl_3']=='All the time')|(df['ts_perceptions_sl_3']=='Often')]['ts_perceptions_sl_3'].value_counts().sum()*100/df['ts_perceptions_sl_3'].value_counts().sum(),2),
           df[(df['ts_perceptions_sl_5']=='All the time')|(df['ts_perceptions_sl_5']=='Often')]['ts_perceptions_sl_5'].value_counts().sum(),
round(df[(df['ts_perceptions_sl_5']=='All the time')|(df['ts_perceptions_sl_5']=='Often')]['ts_perceptions_sl_5'].value_counts().sum()*100/df['ts_perceptions_sl_5'].value_counts().sum(),2)]

def teach_obs(df):
    return [df[(df['observation_practice_1']=='5 - Almost always')|(df['observation_practice_1']=='4 - Very often')]['observation_practice_1'].value_counts().sum(),
round(df[(df['observation_practice_1']=='5 - Almost always')|(df['observation_practice_1']=='4 - Very often')]['observation_practice_1'].value_counts().sum()*100/df['observation_practice_1'].value_counts().sum(),2),
           df[(df['observation_practice_2']=='5 - Almost always')|(df['observation_practice_2']=='4 - Very often')]['observation_practice_2'].value_counts().sum(),
round(df[(df['observation_practice_2']=='5 - Almost always')|(df['observation_practice_2']=='4 - Very often')]['observation_practice_2'].value_counts().sum()*100/df['observation_practice_2'].value_counts().sum(),2),
           df[(df['observation_practice_3']=='5 - Almost always')|(df['observation_practice_3']=='4 - Very often')]['observation_practice_3'].value_counts().sum(),
round(df[(df['observation_practice_3']=='5 - Almost always')|(df['observation_practice_3']=='4 - Very often')]['observation_practice_3'].value_counts().sum()*100/df['observation_practice_3'].value_counts().sum(),2)]

def future_plan(df):
    return [df.future_plan_teaching.value_counts(),
            round(df.future_plan_teaching.value_counts()*100/df.future_plan_teaching.value_counts().sum(),2)]

def leave_plan(df):
    return [df.plan_leave_teaching.value_counts(),
        round(df.plan_leave_teaching.value_counts()*100/df.plan_leave_teaching.value_counts().sum(),2)]

def future_pl(df):
    return [df.t_pl_needs_1.value_counts(),round(df.t_pl_needs_1.value_counts()*100/len(df),2),
           df.t_pl_needs_4.value_counts(),round(df.t_pl_needs_4.value_counts()*100/len(df),2),
           df.t_pl_needs_5.value_counts(),round(df.t_pl_needs_5.value_counts()*100/len(df),2),
           df.t_pl_needs_6.value_counts(),round(df.t_pl_needs_6.value_counts()*100/len(df),2),
           df.t_pl_needs_7.value_counts(),round(df.t_pl_needs_7.value_counts()*100/len(df),2),
           df.t_pl_needs_8.value_counts(),round(df.t_pl_needs_8.value_counts()*100/len(df),2),
           df.t_pl_needs_10.value_counts(),round(df.t_pl_needs_10.value_counts()*100/len(df),2),
           df.t_pl_needs_9.value_counts(),round(df.t_pl_needs_9.value_counts()*100/len(df),2)]


def mindset_scoring(df):
    #set up columns
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
    
    #apply numeric, reverse score
    num = {'5 - Strongly agree':5, '4 - Agree':4, '3 - Neither agree nor disagree':3, '2 - Disagree':2, '1 - Strongly disagree':1 }

    #change text to numeric
    for col in columns:
        df = df.replace({col: num})
        df[col]=df[col].apply(pd.to_numeric)
    
    #reverse score required columns
    for i in reverse:  
        high = 6
        df[i]= (high-df[i]).mod(6)
        
    #ed survey creating scores

    scoring = {5:1, 4:0.75, 3:0.5, 2:0.25, 1:0, 0:0}

    qscores = []
    for i in columns:
        column = df[i]
        qscore = column.map(scoring)
        qscores.append(qscore)
    qscores = pd.DataFrame(qscores).transpose()
    
    #renaming q - scored columns
    rename = {'mindsets_ts_1_1':'rc1', 'mindsets_ts_1_21':'rc2', 'mindsets_ts_1_22':'rc3',
       'mindsets_ts_1_23':'rc4', 'mindsets_ts_1_4':'he1', 'mindsets_ts_1_5':'he2',
       'mindsets_ts_1_6':'he3', 'mindsets_ts_1_7':'he4', 'mindsets_ts_1_13':'he5',
       'mindsets_ts_1_16':'gm1', 'mindsets_ts_1_19':'gm2', 'mindsets_ts_1_20':'gm3',
       'non_ts_mindsets_1':'nrc1', 'non_ts_mindsets_2':'nrc2', 'non_ts_mindsets_3':'nrc3',
       'non_ts_mindsets_4':'nrc4', 'non_ts_mindsets_7':'nhe1', 'non_ts_mindsets_8':'nhe2',
       'non_ts_mindsets_9':'nhe3', 'non_ts_mindsets_10':'nhe4', 'non_ts_mindsets_11':'nhe5',
       'non_ts_mindsets_12':'ngm1', 'non_ts_mindsets_13':'ngm2', 'non_ts_mindsets_14':'ngm3'}

    qscores.rename(columns=rename, inplace=True)
    df = pd.concat([df,qscores], axis=1)
    
    # calculate the score for each individual overall and by construct category
    RC = (100*(df[['rc1','rc2','rc3','rc4','nrc1','nrc2','nrc3','nrc4']].sum(axis=1))/4) 
    GM = (100*(df[['gm1','gm2','gm3','ngm1','ngm2','ngm3']].sum(axis=1))/3)
    HE = (100*(df[['he1', 'he2', 'he3','he4','he5','nhe1', 'nhe2', 'nhe3','nhe4','nhe5']].sum(axis=1))/5)
    overall1 = (100*df[['rc1', 'rc2', 'rc3', 'rc4', 'he1', 'he2', 'he3', 'he4', 'he5', 'gm1',
           'gm2', 'gm3', 'nrc1', 'nrc2', 'nrc3', 'nrc4', 'nhe1', 'nhe2', 'nhe3',
           'nhe4', 'nhe5', 'ngm1', 'ngm2', 'ngm3']].sum(axis=1))/12
    overall = (GM+HE+RC)/3
    df['overall_score']=overall
    df['rc_score']=RC
    df['he_score']=HE
    df['gm_score']=GM

    return df

def non_t_mindsets(df):
    return df[df.role!='Teacher/Specialist'].agg({'overall_score':['mean'],'rc_score':['mean'], 'nrc1':['mean'],'nrc2':['mean'],
                                       'nrc3':['mean'],'nrc4':['mean'],
                                        'he_score':['mean'],'nhe1':['mean'],'nhe2':['mean'],'nhe3':['mean'], 'nhe4':['mean'],
                                       'nhe5':['mean'],'gm_score':['mean'],'ngm1':['mean'],'ngm2':['mean'],'ngm3':['mean']}).transpose()

def t_mindsets(df):
    return df[df.role=='Teacher/Specialist'].agg({'overall_score':['mean'],'rc_score':['mean'], 'rc1':['mean'],'rc2':['mean'],
                                       'rc3':['mean'],'rc4':['mean'],
                                        'he_score':['mean'],'he1':['mean'],'he2':['mean'],'he3':['mean'], 'he4':['mean'],
                                       'he5':['mean'],'gm_score':['mean'],'gm1':['mean'],'gm2':['mean'],'gm3':['mean']}).transpose()
