#pylint: disable=unused-variable
#pylint: disable=anomalous-backslash-in-string

import pandas as pd
# import sys
# import os
from datetime import date, timedelta
from bs4 import BeautifulSoup
import requests
from urllib.error import HTTPError
from urllib.request import urlretrieve

src_path = {'wc':'time_series_covid19_confirmed_global.csv','wd':'time_series_covid19_deaths_global.csv','uc':'time_series_covid19_confirmed_US.csv','ud':'time_series_covid19_deaths_US.csv','kc':'time_series_covid19_confirmed_KR.csv','kd':'time_series_covid19_deaths_KR.csv','geo':'UID_ISO_FIPS_LookUp_Table.csv'}

remote_path = {'summary':'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/','daily':'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/','kr_snapshot':'http://ncov.mohw.go.kr/bdBoardList_Real.do?brdId=1&brdGubun=13&ncvContSeq=&contSeq=&board_id=&gubun='}

def set_colname(dates):
    # Make column name in accordance with previous format (e.g., 7/3/20)
    list_date = list(map(str,map(int,dates.split('-'))))
    list_date[2] = list_date[2][:2]
    list_date = '/'.join(list_date)
    return list_date

def get_datelist(last_wc,gaps=5):
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

    start_dt = date(last_date[2],last_date[0],last_date[1]) - timedelta(days=gaps)
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

def write_file(data,type,date,src_path=src_path):
    if type.split('_')[0] == 'new':
        cols = list(data)
        idx1 = cols.index('Province/State')
        idx2 = cols.index('Lat')
        data = data[cols[idx1:idx1+2]+cols[idx2:idx2+2]+cols[:idx1]+cols[idx2+2:]]

    if type.split('_')[0] == 'new':
        filename = 'data/'+src_path[type.split('_')[1]]
    else:
        filename = 'data/'+src_path[type.split('_')[1]]+'_'+date+'_old.csv'

    data.to_csv(filename,index=False)

def scrape_kcdc():
    # Scape KR data (csv not available)
    tag_lists = {'date':['p','class','info'],'region':['th','scope','row'],'infections':['td','headers','status_con s_type1'],'deaths':['td','headers','status_con s_type4']}

    # Lists to be added to DataFrame
    region = []
    infections = []
    deaths = []

    page = remote_path['kr_snapshot']
    src_soup = BeautifulSoup(requests.get(page).text,'lxml')

    # Get report_list
    report_list = src_soup.find(tag_lists['date'][0],attrs={tag_lists['date'][1],tag_lists['date'][2]}).text.strip().split('.')[:2]
    report_list = list(map(int,report_list))
    report_date = date(date.today().year,report_list[0],report_list[1])-timedelta(days=1)
    report_date = report_date.strftime('%m-%d-%Y')

    # Get Province/State info
    for idx in src_soup.find_all(tag_lists['region'][0],attrs={tag_lists['region'][1]:tag_lists['region'][2]}):
        region.append(idx.text)

    # Get confirmed metrics
    for idx in src_soup.find_all(tag_lists['infections'][0],attrs={tag_lists['infections'][1]:tag_lists['infections'][2]}):
        infections.append(float(idx.text.replace(',','')))

    # Get confirmed metrics
    for idx in src_soup.find_all(tag_lists['deaths'][0],attrs={tag_lists['deaths'][1]:tag_lists['deaths'][2]}):
        deaths.append(float(idx.text.replace(',','')))

    d = {'region':region,'infections':infections,'deaths':deaths}
    ds = pd.DataFrame(data=d)[1:].reset_index(drop=True)
    return report_date,ds

def read_appendfile():
    # Read current file (-5/13)
    last_wc = pd.read_csv("data/"+src_path['wc'])
    last_wd = pd.read_csv("data/"+src_path['wd'])
    new_wc = last_wc.copy()
    new_wd = last_wd.copy()

    # Read current KR file
    last_kc = pd.read_csv("data/"+src_path['kc'])
    last_kd = pd.read_csv("data/"+src_path['kd'])
    new_kc = last_kc.copy()
    new_kd = last_kd.copy()

    read_columns = ['FIPS','Admin2','Province_State','Country_Region','Lat','Long_','Confirmed','Deaths']

    # Get file list to be appended
    append_file_w = get_datelist(last_wc)
    append_file_kr = get_datelist(last_kc)

    # append_file = ['07-29-2020.csv','07-30-2020.csv','07-31-2020.csv','08-01-2020.csv']

    # last_wc = conv_lastfile(last_wc,geo[geo_columns])
    # last_wd = conv_lastfile(last_wd,geo[geo_columns])

    write_date_w = ''
    write_date_kr = ''

    for iter,file_list in enumerate(append_file_w):
        # if iter>0:
        #     break

        # Generate file path on Github
        file_path = remote_path['daily']+file_list
        try:
            print("Reading Daily global data with the filename: "+file_list)
            ds = pd.read_csv(file_path,usecols=read_columns) # daily timeseries
            ds = clean_data(ds)

            write_date_w = file_list.split('.')[0]
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
        file_path = remote_path['summary']+src_path[type]
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

    for iter,file_list in enumerate(append_file_kr):
        # if iter>0:
        #     break

        # Scrape KR Summary data from web and save to local
        print("Getting KR up-to-date info")
        report_date, ds = scrape_kcdc()
        print(file_list.split('.')[0])

        if report_date == file_list.split('.')[0]:
            write_date_kr = file_list.split('.')[0]
            column_name = set_colname(file_list.split('.')[0])

            for iter,type in enumerate(['kc','kd']):
                if type == 'kc':
                    file_type = 'infections'
                    new_kc[column_name] = ds[file_type]
                    print(new_kc)
                else:
                    file_type = 'deaths'
                    new_kd[column_name] = ds[file_type]
                    print(new_kd)


    # Write to files
    if write_date_w != '':
        print("Updating global data file(s)")
        write_file(last_wc,'last_wc',write_date_w)
        write_file(last_wd,'last_wd',write_date_w)
        write_file(new_wc,'new_wc',write_date_w)
        write_file(new_wd,'new_wd',write_date_w)
        print("Global data file(s) successfully updated")
    else:
        print("Global summary data update is not required!")

    if write_date_kr != '':
        print("Updating KR data file(s)")
        write_file(last_kc,'last_kc',write_date_kr)
        write_file(last_kd,'last_kd',write_date_kr)
        write_file(new_kc,'new_kc',write_date_kr)
        write_file(new_kd,'new_kd',write_date_kr)
        print("KR data file(s) successfully updated")
    else:
        print("KR summary data update is not required!")


read_appendfile()
