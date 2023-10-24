## retrieve and clean data ##
#reference the survey codes to be used in get_survey
survey_codes()
#use get_survey() to create df
df = get_survey('SV_8vrKtPDtqQFbiBM')
#use clean()to return a cleaned version of the df: drop import id, subset >=7/1/2023, include only Finished responses, reset index
df = pd.DataFrame(clean(df))
#call race_col() to create new column for summative race based on DA guidelines
df = race_col(df)
# call grade_col() to create new column for summative grade level 
df = grade_col(df)



def grade_dist(df):
    return [df.grade.value_counts().sum(),df.grade.value_counts(),round(df.grade.value_counts()*100/len(df),2)]

def exp_dist(df):
    return [df.teaching_experience.value_counts().sum(),df.teaching_experience.value_counts(),round(df.teaching_experience.value_counts()*100/df.teaching_experience.value_counts().sum(),2)]

def lablead_dist(df):
    return [df.lab_leader.value_counts().sum(),df.lab_leader.value_counts(),round(df.lab_leader.value_counts()*100/df.lab_leader.value_counts().sum(),2)]

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

