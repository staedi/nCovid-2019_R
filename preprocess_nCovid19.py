#pylint: disable=unused-variable
#pylint: disable=anomalous-backslash-in-string

import pandas as pd
import numpy as np
from datetime import date, datetime, timedelta

src_path = {'wc':'time_series_covid19_confirmed_global.csv','wd':'time_series_covid19_deaths_global.csv','uc':'time_series_covid19_confirmed_US.csv','ud':'time_series_covid19_deaths_US.csv','kc':'time_series_covid19_confirmed_KR.csv','kd':'time_series_covid19_deaths_KR.csv','geo':'UID_ISO_FIPS_LookUp_Table.csv'}

target_path = {'summary':'time_series_covid19.csv'}

def get_startdate(data,gaps=5):
    # Get last available date to applicable form
    start_dt = datetime.strptime(max(data['Date']),'%m/%d/%y').date()
    end_dt = date.today()

    if start_dt < end_dt-timedelta(days=1):
        start_dt -= timedelta(days=gaps)

    # %- option only works for Unix-based systems
    return start_dt.strftime('%-m/%-d/%y')

def write_file(data,type,date,target_path=target_path):
    if type == 'new':
        filename = 'data/'+target_path['summary']
    else:
        filename = 'data/'+target_path['summary'][:-4]+'_'+date[:-2].replace('-','')+'_old.csv'
    print('{}: {}'.format(type,filename))
    data.to_csv(filename,index=False)

def reorder_columns(cols):
    # adm0_a3
    # Province/State
    # Country/Region
    # Lat
    # Long
    # Population
    lst = cols
    bases_idx = [cols.get_loc('adm0_a3'),cols.get_loc('Province/State'),cols.get_loc('Country/Region'),cols.get_loc('Lat'),cols.get_loc('Long'),cols.get_loc('Population')]
    bases = cols[bases_idx]

    return list(bases) + list(lst.drop(bases))

def read_appendfile():
    # Read current file
    file_list = target_path['summary']

    read_columns = ['Date','adm0_a3','Province/State','Country/Region','Lat','Long','Population','Confirmed','r_Confirmed','i_Confirmed','ri_Confirmed','Tot_Confirmed','iTot_Confirmed','rTot_Confirmed','riTot_Confirmed','Deaths','r_Deaths','i_Deaths','ri_Deaths','Tot_Deaths','iTot_Deaths','rTot_Deaths','riTot_Deaths']

    try:
        print("Reading previous summary data with the filename: "+file_list)
        last = pd.read_csv("data/"+target_path['summary'])
        new = last.copy()
        if len(read_columns) == len(last.columns):
            status = 'Replace'
        else:
            status = "New"

    except FileNotFoundError:
        print('Current summary file not found\nNo backup file(s) will be created')
        status = "New"

    # wc, wd, kc, kd, uc, ud, geo = read_source()

    ds = read_source()
    start_date = get_startdate(ds)
    new = ds

    # else:
    #     # ds = read_source(start_date)
    #     new.drop(new.loc[new['Date']>=start_date,:].index,inplace=True)

    write_date = datetime.strptime(max(ds['Date']),'%m/%d/%y').strftime('%m-%d-%Y')

    # Write to files
    print("Updating summary data: {}".format(status))
    if status == 'Replace':
        write_file(last,'last',write_date)
        write_file(new,'new',write_date)
    else:
        write_file(new,'new',write_date)
    print("Summary data successfully updated")

def read_source(start_date=None):
    # Read global data
    wc = pd.read_csv("data/"+src_path['wc'])
    wd = pd.read_csv("data/"+src_path['wd'])
    # Geographical information will be merged
    wc.drop(['Lat','Long'],axis=1,inplace=True)
    wd.drop(['Lat','Long'],axis=1,inplace=True)

    # Read KR data
    kc = pd.read_csv("data/"+src_path['kc'])
    kd = pd.read_csv("data/"+src_path['kd'])
    # Geographical information will be merged
    kc.drop(['Lat','Long'],axis=1,inplace=True)
    kd.drop(['Lat','Long'],axis=1,inplace=True)

    # Read US data
    uc = pd.read_csv("data/"+src_path['uc'])
    ud = pd.read_csv("data/"+src_path['ud'])
    uc.rename(columns={'iso3':'adm0_a3','Province_State':'Province/State','Country_Region':'Country/Region','Long_':'Long'},inplace=True)
    ud.rename(columns={'iso3':'adm0_a3','Province_State':'Province/State','Country_Region':'Country/Region','Long_':'Long'},inplace=True)

    # County-level data to be purged
    uc.drop(['UID','iso2','code3','Admin2','FIPS','Combined_Key'],axis=1,inplace=True)
    ud.drop(['UID','iso2','code3','Admin2','FIPS','Combined_Key'],axis=1,inplace=True)

    # Append Population column to infections data
    uc = pd.concat([uc.iloc[:,:5],ud['Population'],uc.iloc[:,5:]],axis=1)

    # Read geographical information
    geo = pd.read_csv("data/"+src_path['geo'])
    geo.rename(columns={'iso3':'adm0_a3','Province_State':'Province/State','Country_Region':'Country/Region','Long_':'Long'},inplace=True)
    geo.drop(['UID','iso2','code3','Admin2','FIPS','Combined_Key'],axis=1,inplace=True)

    # Clean and group datasets
    wc, wd, kc, kd, uc, ud = clean_data(wc, wd, kc, kd, uc, ud, geo)
    confirmed = group_by_type(wc,kc,uc,'Confirmed')
    deaths = group_by_type(wd,kd,ud,'Deaths')

    covid = confirmed.merge(deaths,on=['Date','adm0_a3','Province/State','Country/Region','Lat','Long','Population'])

    # print(covid)

    return covid

def clean_data(wc, wd, kc, kd, uc, ud, geo, cutoff=60):
    # Get least recent date (reporting time gaps)
    min_latest = datetime.strptime(min(wc.columns[-1],wd.columns[-1],kc.columns[-1],kd.columns[-1],uc.columns[-1],ud.columns[-1]),'%m/%d/%y')
    max_first = datetime.strptime(max(wc.columns[2],wd.columns[2],kc.columns[2],kd.columns[2],uc.columns[6],ud.columns[6]),'%m/%d/%y')
    min_date = max(min_latest-timedelta(days=cutoff),max_first)

    start_date = min_date.strftime('%-m/%-d/%y')
    end_date = min_latest.strftime('%-m/%-d/%y')

    col_list = {'wc':list(wc.columns[:2])+list(wc.columns[wc.columns.get_loc(start_date):wc.columns.get_loc(end_date)+1]),'wd':list(wd.columns[:2])+list(wd.columns[wd.columns.get_loc(start_date):wc.columns.get_loc(end_date)+1]),'kc':list(kc.columns[:2])+list(kc.columns[kc.columns.get_loc(start_date):kc.columns.get_loc(end_date)+1]),'kd':list(kd.columns[:2])+list(kd.columns[kd.columns.get_loc(start_date):kd.columns.get_loc(end_date)+1]),'uc':list(uc.columns[:6])+list(uc.columns[uc.columns.get_loc(start_date):uc.columns.get_loc(end_date)+1]),'ud':list(ud.columns[:6])+list(ud.columns[ud.columns.get_loc(start_date):ud.columns.get_loc(end_date)+1])
    }

    wc = wc[col_list['wc']].merge(geo,on=['Country/Region','Province/State'])
    wc.drop(wc[((wc['Lat']==0) & (wc['Long']==0) & (wc['Province/State'] != 'Unknown') & (wc['Province/State'].str.contains('Qurantine')!=True)) | (wc['adm0_a3'].isin(['USA','KOR'])) | (wc['adm0_a3'].isnull())].index,inplace=True)
    wc = wc[reorder_columns(wc.columns)]

    wd = wd[col_list['wd']].merge(geo,on=['Country/Region','Province/State'])
    wd.drop(wd[((wd['Lat']==0) & (wd['Long']==0) & (wd['Province/State'] != 'Unknown') & (wd['Province/State'].str.contains('Qurantine')!=True)) | (wd['adm0_a3'].isin(['USA','KOR'])) | (wd['adm0_a3'].isnull())].index,inplace=True)
    wd = wd[reorder_columns(wd.columns)]

    kc = kc[col_list['kc']].merge(geo,on=['Country/Region','Province/State'])
    kc.drop(kc[((kc['Lat']==0) & (kc['Long']==0) & (kc['Province/State'] != 'Unknown') & (kc['Province/State'].str.contains('Qurantine')!=True) & (kc['adm0_a3'] != 'KOR')) | (kc['adm0_a3'].isnull())].index,inplace=True)
    kc = kc[reorder_columns(kc.columns)]

    kd = kd[col_list['kd']].merge(geo,on=['Country/Region','Province/State'])
    kd.drop(kd[((kd['Lat']==0) & (kd['Long']==0) & (kd['Province/State'] != 'Unknown') & (kd['Province/State'].str.contains('Qurantine')!=True) & (kd['adm0_a3'] != 'KOR')) | (kd['adm0_a3'].isnull())].index,inplace=True)
    kd = kd[reorder_columns(kd.columns)]

    uc = uc.loc[((uc['Lat']!=0) | (uc['Long']!=0)) & ((uc['adm0_a3'].notnull())),col_list['uc']]
    us_geo = uc.groupby(['adm0_a3','Country/Region','Province/State'])[['Lat','Long']].mean()
    uc_core = uc.groupby(['adm0_a3','Country/Region','Province/State']).sum().drop(['Lat','Long'],axis=1)
    uc = pd.merge(uc_core,us_geo,on=['adm0_a3','Country/Region','Province/State']).reset_index()
    uc = uc[reorder_columns(uc.columns)]

    ud = ud.loc[((ud['Lat']!=0) | (ud['Long']!=0)) & ((ud['adm0_a3'].notnull())),col_list['ud']]
    # us_geo = uc.groupby(['adm0_a3','Country/Region','Province/State'])[['Lat','Long']].mean()
    ud_core = ud.groupby(['adm0_a3','Country/Region','Province/State']).sum().drop(['Lat','Long'],axis=1)
    ud = pd.merge(ud_core,us_geo,on=['adm0_a3','Country/Region','Province/State']).reset_index()
    ud = ud[reorder_columns(ud.columns)]

    return wc, wd, kc, kd, uc, ud


def group_by_type(data_1, data_2, data_3, type):
    # Confirmed and Deaths
    data = pd.concat([data_1,data_2,data_3])

    # Population manipulation (No country-level statistics for multi-states)
    base_pop = 1e5
    data['num_states'] = data.groupby(['adm0_a3','Country/Region']).adm0_a3.transform('size')
    data.loc[(data['Province/State'].isnull()) & (data['num_states']>1),'Population'] = 0
    data.drop('num_states',axis=1,inplace=True)

    # Wide to long form transformation
    data = data.melt(id_vars=['adm0_a3','Province/State','Country/Region','Lat','Long','Population'])
    data.rename(columns={'variable':'Date','value':type},inplace=True)
    # data['Date'] = pd.to_datetime(data['Date'],format='%m/%d/%y')
    data[type].fillna(0,inplace=True)
    data['Province/State'].replace(np.nan,'NA',inplace=True)
    # Reorder columns
    data = data[[data.columns[-2]]+list(data.columns[:-2])+[data.columns[-1]]]

    # per 100K (r_: Rate)
    data['r_'+type] = data[type]/data['Population']*base_pop

    # Daily changes (i_: Daily Raw, ri_: Daily Rate)
    data['i_'+type] = data[type]-data.groupby(['adm0_a3','Province/State','Country/Region'])[type].shift(1)
    data['i_'+type].fillna(data[type],inplace=True)
    # print(data[['Date','Country/Region','Province/State',type,'i_'+type]])
    # print(data)
    data['ri_'+type] = data['i_'+type]/data['Population']*base_pop
    # data['ri'+type].fillna(data['r'+type],inplace=True)

    # Country-level grouping
    data['Tot_'+type] = data.groupby(['Date','adm0_a3','Country/Region'])[type].transform('sum')

    data['iTot_'+type] = data.groupby(['Date','adm0_a3','Country/Region'])['i_'+type].transform('sum')
    data['rTot_'+type] = data['Tot_'+type]/data.groupby(['Date','adm0_a3','Country/Region'])['Population'].transform('sum')*base_pop

    data['riTot_'+type] = data['iTot_'+type]/data.groupby(['Date','adm0_a3','Country/Region'])['Population'].transform('sum')*base_pop

    # print(data[['Date','Country/Region','Province/State',type,'i_'+type]])

    data.drop(data[data['Date']==min(data['Date'])].index,inplace=True)
    data = data.round(4)
    data.reset_index(drop=True,inplace=True)
    print(data[['Date','Country/Region','Province/State',type,'i_'+type]])

    # print(data)

    return data

if __name__ == '__main__':
    read_appendfile()
