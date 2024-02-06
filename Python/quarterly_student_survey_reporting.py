import pandas as pd
import numpy as np
import math

### BASIC SURVEY RETRIEVAL AND CLEANING FUNCTIONS
#list of possible student surveys
# we exclude D13 from calculation (for now)
def student_codes():
    codes = {'D13_Student':'SV_9LisDEvQ3oUVCGq', 'SY23-24_Student TL':'SV_9z3haYioAgAvaAK',
            'EIC_Student':'SV_8f9l21n6ML58WFM', 'NM_Student':'SV_9uze2faHuIf3vP8'}
    return codes

# qualtrics credentials to import surveys
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

# clean data based on Finished = True surveys, and certain start and end dates (on Recorded Date)
def clean(data,start,end):
    #drop the import label and question rows
    data.drop(index=data.index[:2], inplace=True)
    #change the data type to date instead of string 
    data['RecordedDate']=pd.to_datetime(data['RecordedDate'])
    #subset the data to only include Finished surveys completed after July 1st (new school year)
    df = pd.DataFrame(data[(data['RecordedDate']>= start)&(data['RecordedDate']<= end)])
    df = pd.DataFrame(df[(df['Finished']=='True')|(df['Finished']==True)])
    df = df.reset_index()
    df = df.drop(columns=['index'])
    return df


### POSITIVE RESPONSE CALCULATION BY SURVEY###
# calculating the average % positive response on NM student survey

def pos_ind_nm():
    
    #initialize an np array to collect the %s from each q
    pos =[]
    
    #calculate the pos ind/nsize of each question
        #this code counts the rows that contain the desired string associated with a pos ind on each q,
        #then divides that count by the total non NaN values in that column
    q1 = round(100*nm.crse_1.str.contains('5|4').value_counts()[True]/nm.crse_1.value_counts().sum(),2)
    pos.append(q1)
    q2 = round(100*nm.crse_2.str.contains('5|4').value_counts()[True]/nm.crse_2.value_counts().sum(),2)
    pos.append(q2)
    q3 = round(100*nm.crse_3.str.contains('5|4').value_counts()[True]/nm.crse_3.value_counts().sum(),2)
    pos.append(q3)
    q4 = round(100*nm.crse_4.str.contains('5|4').value_counts()[True]/nm.crse_4.value_counts().sum(),2)
    pos.append(q4)
    q5 = round(100*nm.crse_5.str.contains('5|4').value_counts()[True]/nm.crse_5.value_counts().sum(),2)
    pos.append(q5)
    q6 = round(100*nm.crse_6.str.contains('5|4').value_counts()[True]/nm.crse_6.value_counts().sum(),2)
    pos.append(q6)
    q7 = round(100*nm.crse_7.str.contains('5|4').value_counts()[True]/nm.crse_7.value_counts().sum(),2)
    pos.append(q7)
    q8 = round(100*nm.crse_8.str.contains('5|4').value_counts()[True]/nm.crse_8.value_counts().sum(),2)
    pos.append(q8)
    q9 = round(100*nm.teacher_student_rel_1.str.contains('5|4').value_counts()[True]/nm.teacher_student_rel_1.value_counts().sum(),2)
    pos.append(q9)
    q10 = round(100*nm.teacher_student_rel_2.str.contains('5|4').value_counts()[True]/nm.teacher_student_rel_2.value_counts().sum(),2)
    pos.append(q10)
    q11 = round(100*nm.teacher_student_rel_3.str.contains('5|4').value_counts()[True]/nm.teacher_student_rel_3.value_counts().sum(),2)
    pos.append(q11)
    q12 = round(100*nm.self_efficacy_1.str.contains('5|4').value_counts()[True]/nm.self_efficacy_1.value_counts().sum(),2)
    pos.append(q12)
    q13 = round(100*nm.self_efficacy_2.str.contains('5|4').value_counts()[True]/nm.self_efficacy_2.value_counts().sum(),2)
    pos.append(q13)
    q14 = round(100*nm.self_efficacy_3.str.contains('5|4').value_counts()[True]/nm.self_efficacy_3.value_counts().sum(),2)
    pos.append(q14)
    q15 = round(100*nm.self_efficacy_4.str.contains('5|4').value_counts()[True]/nm.self_efficacy_4.value_counts().sum(),2)
    pos.append(q15)
    q16 = round(100*nm.self_efficacy_5.str.contains('5|4').value_counts()[True]/nm.self_efficacy_5.value_counts().sum(),2)
    pos.append(q16)
    q17 = round(100*nm.happiness_belonging_1.str.contains('5|4').value_counts()[True]/nm.happiness_belonging_1.value_counts().sum(),2)
    pos.append(q17)
    q18 = round(100*nm.happiness_belonging_2.str.contains('5|4').value_counts()[True]/nm.happiness_belonging_2.value_counts().sum(),2)
    pos.append(q18)
    q19 = round(100*nm.happiness_belonging_3.str.contains('1|2').value_counts()[True]/nm.happiness_belonging_3.value_counts().sum(),2)
    pos.append(q19)
    q20 = round(100*nm.happiness_belonging_4.str.contains('5|4').value_counts()[True]/nm.happiness_belonging_4.value_counts().sum(),2)
    pos.append(q20)
    q21 = round(100*nm.happiness_belonging_5.str.contains('5|4').value_counts()[True]/nm.happiness_belonging_5.value_counts().sum(),2)
    pos.append(q21)
    q22 = round(100*nm.happiness_belonging_6.str.contains('5|4').value_counts()[True]/nm.happiness_belonging_6.value_counts().sum(),2)
    pos.append(q22)
    q23 = round(100*nm.being_challenged_1.str.contains('5|4').value_counts()[True]/nm.being_challenged_1.value_counts().sum(),2)
    pos.append(q23)
    q24 = round(100*nm.being_challenged_2.str.contains('5|4').value_counts()[True]/nm.being_challenged_2.value_counts().sum(),2)
    pos.append(q24)
    q25 = round(100*nm.being_challenged_3.str.contains('5|4').value_counts()[True]/nm.being_challenged_3.value_counts().sum(),2)
    pos.append(q25)
    q26 = round(100*nm.being_challenged_4.str.contains('5|4').value_counts()[True]/nm.being_challenged_4.value_counts().sum(),2)
    pos.append(q26)
    q27 = round(100*nm.being_challenged_5.str.contains('5|4').value_counts()[True]/nm.being_challenged_5.value_counts().sum(),2)
    pos.append(q27)
    
    #take average of the % pos ind array
    avg = round(np.mean(pos),2)
    #return the pos ind avg as a percent rounded to two decimals
    return avg


# calculating the average % positive response on EIC student survey

def pos_ind_eic():
    
    #initialize an np array to collect the %s from each q
    pos =[]
    
    #calculate the pos ind/nsize of each question
        #this code counts the rows that contain the desired string associated with a pos ind on each q,
        #then divides that count by the total non NaN values in that column
    q1 = round(100*eic.growth_mindsets_a1_1.str.contains('1|2').value_counts()[True]/eic.growth_mindsets_a1_1.value_counts().sum(),2)
    pos.append(q1)
    q2 = round(100*eic.growth_mindsets_a1_2.str.contains('1|2').value_counts()[True]/eic.growth_mindsets_a1_2.value_counts().sum(),2)
    pos.append(q2)
    q3 = round(100*eic.growth_mindsets_a1_3.str.contains('1|2').value_counts()[True]/eic.growth_mindsets_a1_3.value_counts().sum(),2)
    pos.append(q3)
    q4 = round(100*eic.growth_mindsets_a1_4.str.contains('1|2').value_counts()[True]/eic.growth_mindsets_a1_4.value_counts().sum(),2)
    pos.append(q4)
    q5 = round(100*eic.growth_mindsets_a2.str.contains('Very|Extremely').value_counts()[True]/eic.growth_mindsets_a2.value_counts().sum(),2)
    pos.append(q5)
    q6 = round(100*eic.growth_mindsets_a3.str.contains('Strongly agree|Agree').value_counts()[True]/eic.growth_mindsets_a3.value_counts().sum(),2)
    pos.append(q6)
    q7 = round(100*eic.self_efficacy_a4_1.str.contains('Strongly agree|Agree').value_counts()[True]/eic.self_efficacy_a4_1.value_counts().sum(),2)
    pos.append(q7)
    q8 = round(100*eic.self_efficacy_a4_2.str.contains('Strongly agree|Agree').value_counts()[True]/eic.self_efficacy_a4_2.value_counts().sum(),2)
    pos.append(q8)
    q9 = round(100*eic.self_efficacy_a4_3.str.contains('Strongly agree|Agree').value_counts()[True]/eic.self_efficacy_a4_3.value_counts().sum(),2)
    pos.append(q9)
    q10 = round(100*eic.self_efficacy_a4_4.str.contains('Strongly agree|Agree').value_counts()[True]/eic.self_efficacy_a4_4.value_counts().sum(),2)
    pos.append(q10)
    q11 = round(100*eic.self_efficacy_a4_5.str.contains('Strongly agree|Agree').value_counts()[True]/eic.self_efficacy_a4_5.value_counts().sum(),2)
    pos.append(q11)
    q12 = round(100*eic.math_enjoyment_a5_1.str.contains('Agree').value_counts()[True]/eic.math_enjoyment_a5_1.value_counts().sum(),2)
    pos.append(q12)
    q13 = round(100*eic.math_enjoyment_a5_2.str.contains('Agree').value_counts()[True]/eic.math_enjoyment_a5_2.value_counts().sum(),2)
    pos.append(q13)
    q14 = round(100*eic.math_enjoyment_a5_3.str.contains('Agree').value_counts()[True]/eic.math_enjoyment_a5_3.value_counts().sum(),2)
    pos.append(q14)
    q15 = round(100*eic.math_enjoyment_a5_4.str.contains('Agree').value_counts()[True]/eic.math_enjoyment_a5_4.value_counts().sum(),2)
    pos.append(q15)
    q16 = round(100*eic.mses_a6_1.str.contains('1|2').value_counts()[True]/eic.mses_a6_1.value_counts().sum(),2)
    pos.append(q16)
    q17 = round(100*eic.mses_a6_2.str.contains('5|4').value_counts()[True]/eic.mses_a6_2.value_counts().sum(),2)
    pos.append(q17)
    q18 = round(100*eic.mses_a6_3.str.contains('1|2').value_counts()[True]/eic.mses_a6_3.value_counts().sum(),2)
    pos.append(q18)
    q19 = round(100*eic.mses_a6_4.str.contains('5|4').value_counts()[True]/eic.mses_a6_4.value_counts().sum(),2)
    pos.append(q19)
    q20 = round(100*eic.mses_a6_5.str.contains('5|4').value_counts()[True]/eic.mses_a6_5.value_counts().sum(),2)
    pos.append(q20)
    q21 = round(100*eic.mses_a6_6.str.contains('5|4').value_counts()[True]/eic.mses_a6_6.value_counts().sum(),2)
    pos.append(q21)
    q22 = round(100*eic.mses_a6_7.str.contains('5|4').value_counts()[True]/eic.mses_a6_7.value_counts().sum(),2)
    pos.append(q22)
    q23 = round(100*eic.mses_a6_8.str.contains('1|2').value_counts()[True]/eic.mses_a6_8.value_counts().sum(),2)
    pos.append(q23)
    q24 = round(100*eic.mses_a6_9.str.contains('1|2').value_counts()[True]/eic.mses_a6_9.value_counts().sum(),2)
    pos.append(q24)
    q25 = round(100*eic.mses_a6_10.str.contains('5|4').value_counts()[True]/eic.mses_a6_10.value_counts().sum(),2)
    pos.append(q25)
    q26 = round(100*eic.mses_a6_11.str.contains('5|4').value_counts()[True]/eic.mses_a6_11.value_counts().sum(),2)
    pos.append(q26)
    q27 = round(100*eic.mses_a6_12.str.contains('5|4').value_counts()[True]/eic.mses_a6_12.value_counts().sum(),2)
    pos.append(q27)
    q28 = round(100*eic.mses_a6_13.str.contains('5|4').value_counts()[True]/eic.mses_a6_13.value_counts().sum(),2)
    pos.append(q28)
    q29 = round(100*eic.math_a7_1.str.contains('3|4').value_counts()[True]/eic.math_a7_1.value_counts().sum(),2)
    pos.append(q29)
    q30 = round(100*eic.math_a7_2.str.contains('3|4').value_counts()[True]/eic.math_a7_2.value_counts().sum(),2)
    pos.append(q30)
    
    #take average of the % pos ind array
    avg = round(np.mean(pos),2)
    #return the pos ind avg as a percent rounded to two decimals
    return avg


# calculating the average % positive response on TL SY23-24 student survey

def pos_ind_tl():
    
    #initialize an np array to collect the %s from each q
    pos =[]
    
    #calculate the pos ind/nsize of each question
        #this code counts the rows that contain the desired string associated with a pos ind on each q,
        #then divides that count by the total non NaN values in that column
    q1 = round(100*tl.high_exp_one_1.str.contains('5|4').value_counts()[True]/tl.high_exp_one_1.value_counts().sum(),2)
    pos.append(q1)
    q2 = round(100*tl.high_exp_one_2.str.contains('5|4').value_counts()[True]/tl.high_exp_one_2.value_counts().sum(),2)
    pos.append(q2)
    q3 = round(100*tl.high_exp_one_3.str.contains('5|4').value_counts()[True]/tl.high_exp_one_3.value_counts().sum(),2)
    pos.append(q3)
    q4 = round(100*tl.high_exp_two.str.contains('Very|Extremely').value_counts()[True]/tl.high_exp_two.value_counts().sum(),2)
    pos.append(q4)
    q5 = round(100*tl.rig_learn_one_1.str.contains('5|4').value_counts()[True]/tl.rig_learn_one_1.value_counts().sum(),2)
    pos.append(q5)
    q6 = round(100*tl.rig_learn_one_2.str.contains('5|4').value_counts()[True]/tl.rig_learn_one_2.value_counts().sum(),2)
    pos.append(q6)
    q7 = round(100*tl.rig_learn_two.str.contains('Often|always').value_counts()[True]/tl.rig_learn_two.value_counts().sum(),2)
    pos.append(q7)
    q8 = round(100*tl.relevance_one.str.contains('Often|always').value_counts()[True]/tl.relevance_one.value_counts().sum(),2)
    pos.append(q8)
    q9 = round(100*tl.relevance_two_1.str.contains('5|4').value_counts()[True]/tl.relevance_two_1.value_counts().sum(),2)
    pos.append(q9)
    q10 = round(100*tl.relevance_two_2.str.contains('5|4').value_counts()[True]/tl.relevance_two_2.value_counts().sum(),2)
    pos.append(q10)
    q11 = round(100*tl.affirm_1.str.contains('5|4').value_counts()[True]/tl.affirm_1.value_counts().sum(),2)
    pos.append(q11)
    q12 = round(100*tl.affirm_2.str.contains('5|4').value_counts()[True]/tl.affirm_2.value_counts().sum(),2)
    pos.append(q12)
    q13 = round(100*tl.affirm_3.str.contains('5|4').value_counts()[True]/tl.affirm_3.value_counts().sum(),2)
    pos.append(q13)
    q14 = round(100*tl.connect_one_1.str.contains('5|4').value_counts()[True]/tl.connect_one_1.value_counts().sum(),2)
    pos.append(q14)
    q15 = round(100*tl.connect_one_2.str.contains('5|4').value_counts()[True]/tl.connect_one_2.value_counts().sum(),2)
    pos.append(q15)
    q16 = round(100*tl.connect_two.str.contains('Mostly|Completely').value_counts()[True]/tl.connect_two.value_counts().sum(),2)
    pos.append(q16)
    q17 = round(100*tl.custom_one.str.contains('Often|always').value_counts()[True]/tl.custom_one.value_counts().sum(),2)
    pos.append(q17)
    q18 = round(100*tl.custom_two_1.str.contains('5|4').value_counts()[True]/tl.custom_two_1.value_counts().sum(),2)
    pos.append(q18)
    q19 = round(100*tl.custom_two_2.str.contains('5|4').value_counts()[True]/tl.custom_two_2.value_counts().sum(),2)
    pos.append(q19)
    q20 = round(100*tl.asd_one.str.contains('Often|always').value_counts()[True]/tl.asd_one.value_counts().sum(),2)
    pos.append(q20)
    q21 = round(100*tl.asd_two_1.str.contains('5|4').value_counts()[True]/tl.asd_two_1.value_counts().sum(),2)
    pos.append(q21)
    q22 = round(100*tl.asd_two_2.str.contains('5|4').value_counts()[True]/tl.asd_two_2.value_counts().sum(),2)
    pos.append(q22)
    q23 = round(100*tl.asd_two_3.str.contains('5|4').value_counts()[True]/tl.asd_two_3.value_counts().sum(),2)
    pos.append(q23)
    q24 = round(100*tl.overall_experience_1.str.contains('5|4').value_counts()[True]/tl.overall_experience_1.value_counts().sum(),2)
    pos.append(q24)
    q25 = round(100*tl.overall_experience_2.str.contains('5|4').value_counts()[True]/tl.overall_experience_2.value_counts().sum(),2)
    pos.append(q25)
    
    #take average of the % pos ind array
    avg = round(np.mean(pos),2)
    #return the pos ind avg as a percent rounded to two decimals
    return avg


### QUARTERLY CALCULATION ###
#create new function that pulls all 3 surveys, calculates n sizes, calculates pos ind, returns weighted_average

def quarterly_student_pos(start,end):
    ## retrieve all surveys and clean 
    tl = clean(get_survey('SV_9z3haYioAgAvaAK'),start,end)
    eic = clean(get_survey('SV_8f9l21n6ML58WFM'),start,end)
    nm = clean(get_survey('SV_9uze2faHuIf3vP8'),start,end)

    ## n size calculator
    tl_n=len(tl)
    eic_n = len(eic)
    nm_n = len(nm)
    n_size = tl_n+eic_n+nm_n
    
    ## calculate the average pos indicators per survey
    ptl = pos_ind_tl()
    peic = pos_ind_eic()
    pnm = pos_ind_nm()
    
    ## calculate the weighted average
    weighted_average = ((ptl*tl_n)+(peic*eic_n)+(pnm*nm_n))/n_size
    
    #return the weighted average for reporting
    return weighted_average

# Number to Report:
# Q2 for example
quarterly_student_pos('07-01-2023','12-31-2023')
