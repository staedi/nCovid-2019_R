#pylint: disable=unused-variable
#pylint: disable=anomalous-backslash-in-string

import pandas as pd
import sys
import os
import inspect
from datetime import date, timedelta
from urllib.error import HTTPError
from urllib.request import urlretrieve

src_path = {'wc':'time_series_covid19_confirmed_global.csv','wd':'time_series_covid19_deaths_global.csv','uc':'time_series_covid19_confirmed_US.csv','ud':'time_series_covid19_deaths_US.csv','geo':'UID_ISO_FIPS_LookUp_Table.csv'}
summary_path = 'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/'
daily_path = 'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/'

def set_colname(dates):
    # Make column name in accordance with previous format (e.g., 7/3/20)
    list_date = list(map(str,map(int,dates.split('-'))))
    list_date[2] = list_date[2][:2]
    list_date = '/'.join(list_date)
    return list_date

def get_datelist(last_wc):
    # Get last available date to applicable form
    last_date = last_wc.columns[-1].split('/')
    if len(last_date[2])==2:
        last_date[2] = '20'+last_date[2]
    last_date = list(map(int,last_date))

    # Check Level-1 Non-US data is available
    if len(last_wc.loc[last_wc['Country/Region']=='Japan','Country/Region'])==1:
        last_date = [5,13,2020]
    else:
        last_date

    start_dt = date(last_date[2],last_date[0],last_date[1])
    end_dt = date.today()
    appendlist = [(start_dt + timedelta(days=x)).strftime('%m-%d-%Y')+'.csv' for x in range(1,(end_dt-start_dt).days + 1)]

    return appendlist

def clean_data(time_series):
    # Non-US data
    bool_time_series = (time_series['Country_Region']!='US')

    # Generate US Summury Stat
    us_stat = time_series.loc[bool_time_series==False,].groupby(['Country_Region'])[['Confirmed','Deaths']].sum()

    # Remove US data
    time_series = time_series.loc[bool_time_series,]

    # Add US Summary Stat
    time_series = time_series.append(pd.DataFrame([['US',40,-100,int(us_stat['Confirmed']),int(us_stat['Deaths'])]],columns=['Country_Region','Lat','Long_','Confirmed','Deaths']))

    return time_series

# def conv_lastfile(src,geo):
#     # Convert currently available data to include all regions in geo
#     # bool_state = (geo['Province_State'].isin(src['Province/State'])==False)
#     # geo = geo.loc[bool_state]
#     conv = pd.merge(geo,src,how='left',left_on=['Province_State','Country_Region'],right_on=['Province/State','Country/Region'])
#     conv.drop(['Lat_y','Long','FIPS','Admin2','Country/Region','Province/State'],axis=1,inplace=True)
#     conv.rename(columns={'Lat_x':'Lat','Long_':'Long','Province_State':'Province/State','Country_Region':'Country/Region'},inplace=True)
#     return conv

def write_file(data,type,date,src_path=src_path):
    # first = ['Province/State','Country/Region']
    # print(type)
    # print(src_path[type.split('_')])
    if type.split('_')[0] == 'new':
        cols = list(data)
        idx1 = cols.index('Province/State')
        idx2 = cols.index('Lat')
        data = data[cols[idx1:idx1+2]+cols[idx2:idx2+2]+cols[:idx1]+cols[idx2+2:]]

    if type.split('_')[0] == 'new':
        filename = 'data/'+src_path[type.split('_')[1]]
    # elif type[:1]=='u':
    #     filename = 'data/'+src_path[type]
    else:
        filename = 'data/'+src_path[type.split('_')[1]]+'_'+date+'_old.csv'

    data.to_csv(filename,index=False)


def read_appendfile():
    # Read current file (-5/13)
    last_wc = pd.read_csv("data/"+src_path['wc'])
    last_wd = pd.read_csv("data/"+src_path['wd'])
    new_wc = last_wc.copy()
    new_wd = last_wd.copy()
    # geo = pd.read_csv("data/"+src_path['geo'])

    # bool_us = (geo['Combined_Key'].str.contains('US')==False)
    # geo = geo.loc[bool_us]   # Non-US data-only
    # geo_columns = ['FIPS','Admin2','Province_State','Country_Region','Lat','Long_']
    read_columns = ['FIPS','Admin2','Province_State','Country_Region','Lat','Long_','Confirmed','Deaths']

    # Get file list to be appended
    append_file = get_datelist(last_wc)
    # append_file = ['07-29-2020.csv','07-30-2020.csv','07-31-2020.csv','08-01-2020.csv']

    # last_wc = conv_lastfile(last_wc,geo[geo_columns])
    # last_wd = conv_lastfile(last_wd,geo[geo_columns])

    write_date = ''

    for iter,file_list in enumerate(append_file):
        # if iter>0:
        #     break
        # Generate file path on Github
        file_path = daily_path+file_list
        try:
            print("Reading Daily global data with the filename: "+file_list)
            ds = pd.read_csv(file_path,usecols=read_columns) # daily timeseries
            ds = clean_data(ds)

            write_date = file_list.split('.')[0]
            column_name = set_colname(file_list.split('.')[0])

            if column_name in new_wc.columns:
                new_wc = new_wc.drop(column_name,axis=1)
            if column_name in new_wd.columns:
                new_wd = new_wd.drop(column_name,axis=1)

            new_wc = pd.merge(new_wc,ds[['Province_State','Country_Region','Lat','Long_','Confirmed']],how='outer',left_on=['Province/State','Country/Region'],right_on=['Province_State','Country_Region'])
            new_wd = pd.merge(new_wd,ds[['Province_State','Country_Region','Lat','Long_','Deaths']],how='outer',left_on=['Province/State','Country/Region'],right_on=['Province_State','Country_Region'])

            new_wc.loc[new_wc['Province_State'].isnull(),'Province_State'] = new_wc.loc[new_wc['Province_State'].isnull(),'Province/State']
            new_wc.loc[new_wc['Country_Region'].isnull(),'Country_Region'] = new_wc.loc[new_wc['Country_Region'].isnull(),'Country/Region']
            new_wd.loc[new_wd['Province_State'].isnull(),'Province_State'] = new_wd.loc[new_wd['Province_State'].isnull(),'Province/State']
            new_wd.loc[new_wd['Country_Region'].isnull(),'Country_Region'] = new_wd.loc[new_wd['Country_Region'].isnull(),'Country/Region']

            # print(last_wc)
            # print(last_wc.columns)

            new_wc.loc[new_wc['Lat_y'].isnull(),'Lat_y'] = new_wc.loc[new_wc['Lat_y'].isnull(),'Lat_x']
            new_wc.loc[new_wc['Long_'].isnull(),'Long_'] = new_wc.loc[new_wc['Long_'].isnull(),'Long']
            new_wd.loc[new_wd['Lat_y'].isnull(),'Lat_y'] = new_wd.loc[new_wd['Lat_y'].isnull(),'Lat_x']
            new_wd.loc[new_wd['Long_'].isnull(),'Long_'] = new_wd.loc[new_wd['Long_'].isnull(),'Long']


            new_wc.drop(['Province/State','Country/Region','Lat_x','Long'],axis=1,inplace=True)
            new_wd.drop(['Province/State','Country/Region','Lat_x','Long'],axis=1,inplace=True)

            #'/'.join(map(str,list(map(int,file_list.split('.')[0].split('-')))))

            new_wc.rename(columns={'Province_State':'Province/State','Country_Region':'Country/Region','Lat_y':'Lat','Long_':'Long','Confirmed':column_name},inplace=True)
            new_wd.rename(columns={'Province_State':'Province/State','Country_Region':'Country/Region','Lat_y':'Lat','Long_':'Long','Deaths':column_name},inplace=True)

        except HTTPError:   # Data backlog or time differences
            print(file_list+" not found")
            break

    # Read US Summary data and save to local
    for iter,type in enumerate(['uc','ud']):
        file_path = summary_path+src_path[type]
        if type == 'uc':
            file_type = 'infections'
        else:
            file_type = 'deaths'
        try:
            print("Attempting to retrieve US summary data of "+file_type)
            filename = 'data/'+src_path[type]
            urlretrieve(file_path,filename)  # US summary data
            # print("Reading US summary data of "+file_type)
            # # ds = pd.read_csv(file_path) # US summary data
            print("US summary data of "+file_type+" successfully retreieved")
            # write_file(ds,type,write_date)

        except HTTPError:   # Data backlog or time differences
            print("One or more file(s) for US summary data not found")
            break

    # Write to files
    if write_date != '':
        print("Updating global data file(s)")
        write_file(last_wc,'last_wc',write_date)
        write_file(last_wd,'last_wd',write_date)
        write_file(new_wc,'new_wc',write_date)
        write_file(new_wd,'new_wd',write_date)
        print("Global data file(s) successfully updated")
    else:
        print("Global summary data update is not required!")

read_appendfile()
