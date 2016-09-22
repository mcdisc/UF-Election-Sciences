import datetime

import pymysql.cursors

#Connect to MySQL Server

connection = pymysql.connect(host = 'localhost',
                             port = 3306,
                             user = 'root',
                             password = 'NEWPASSWORD',
                             db= 'NC_EARLYVOTE')

cur = connection.cursor()

#Create simple table name, will be used throughout

def create_filepath():
    date = datetime.date.today()
    path = 'absentee%s%s%s' % (date.month, date.day, date.year)
    return path

try:
    with connection.cursor() as cursor:

        #Create table with fields in absentee ballot file old files will NOT have NCID

        sql = 'CREATE TABLE %s (county_desc	text,' \
              'voter_reg_num text, ' \
              'ncid	text,' \
              'voter_last_name text,' \
              'voter_first_name	text, ' \
              'voter_middle_name text,' \
              'race text,' \
              'gender text,' \
              'age	text	,' \
              'voter_street_address	text	,' \
              'voter_city	text	,' \
              'voter_state	text	,' \
              'voter_zip	text	,' \
              'ballot_mail_street_address	text	,' \
              'ballot_mail_city	text	,' \
              'ballot_mail_state	text	,' \
              'ballot_mail_zip	text	,' \
              'other_mail_addr1	text	,' \
              'other_mail_addr2	text	,' \
              'other_city_state_zip	text	,' \
              'election_dt	text	,' \
              'voter_party_code	text	,' \
              'precinct_desc	text	,' \
              'cong_dist_desc	text	,' \
              'nc_house_desc	text	,' \
              'nc_senate_desc	text	,' \
              'ballot_req_delivery_type	text	,' \
              'ballot_req_type	text	,' \
              'ballot_request_party	text	,' \
              'ballot_req_dt	text	,' \
              'ballot_send_dt	text	,' \
              'ballot_rtn_dt	text	,' \
              'ballot_rtn_status text,' \
              'site_name text);' % create_filepath()
        cursor.execute(sql)
        connection.commit()

    with connection.cursor() as cursor:

        #Load data in- much faster than data import wizard in MySQL

        sql = "LOAD DATA INFILE '/Users/Potalora/Desktop/absentee11xx08xx2016-6.csv' " \
              "INTO TABLE {0} " \
              "FIELDS TERMINATED BY ',' " \
              "ENCLOSED BY '\"' " \
              "LINES TERMINATED BY '\\n' " \
              "IGNORE 1 Lines;".format(create_filepath())
        cursor.execute(sql)
        connection.commit()

    with connection.cursor() as cursor:

        #Make empty dates NULL and convert to DATETIME

        sql = "UPDATE {0} " \
              "SET `ballot_rtn_dt`= NULL WHERE `ballot_rtn_dt`= '';" \
              "UPDATE {0} " \
              "SET `ballot_req_dt`= NULL WHERE `ballot_req_dt`= '';" \
              "UPDATE {0} " \
              "SET `ballot_send_dt`= NULL WHERE `ballot_send_dt`= ''; " \
              "UPDATE {0} " \
              "SET `ballot_rtn_dt` = str_to_date(`ballot_rtn_dt`, '%m/%d/%Y') where 1=1; " \
              "UPDATE {0} " \
              "SET `ballot_req_dt` = str_to_date(`ballot_req_dt`, '%m/%d/%Y') where 1=1; " \
              "UPDATE {0} " \
              "SET `ballot_send_dt` = str_to_date(`ballot_send_dt`, '%m/%d/%Y') where 1=1;".format(create_filepath())
        cursor.execute(sql)
        connection.commit()
    with connection.cursor() as cursor:

        #Change date columns to actual DATETIME

        sql = "ALTER TABLE {0} " \
              "CHANGE COLUMN `ballot_req_dt` `ballot_req_dt` DATETIME NULL DEFAULT NULL , " \
              "CHANGE COLUMN `ballot_send_dt` `ballot_send_dt` DATETIME NULL DEFAULT NULL , " \
              "CHANGE COLUMN `ballot_rtn_dt` `ballot_rtn_dt` DATETIME NULL DEFAULT NULL ;".format(create_filepath())
        cursor.execute(sql)
        connection.commit()

    with connection.cursor() as cursor:

        #Create new column with FIPS codes matched to county_id

        sql = "ALTER TABLE {0} ADD COLUMN county_id TEXT; " \
              "UPDATE {0} SET county_id = 	'37001'	WHERE county_desc = 	'ALAMANCE'	; " \
              "UPDATE {0} SET county_id = 	'37003'	WHERE county_desc = 	'ALEXANDER'	; " \
              "UPDATE {0} SET county_id = 	'37005'	WHERE county_desc = 	'ALLEGHANY'	; " \
              "UPDATE {0} SET county_id = 	'37007'	WHERE county_desc = 	'ANSON'	; " \
              "UPDATE {0} SET county_id = 	'37009'	WHERE county_desc = 	'ASHE'	; " \
              "UPDATE {0} SET county_id = 	'37011'	WHERE county_desc = 	'AVERY'	; " \
              "UPDATE {0} SET county_id = 	'37013'	WHERE county_desc = 	'BEAUFORT'	; " \
              "UPDATE {0} SET county_id = 	'37015'	WHERE county_desc = 	'BERTIE'	; " \
              "UPDATE {0} SET county_id = 	'37017'	WHERE county_desc = 	'BLADEN'	; " \
              "UPDATE {0} SET county_id = 	'37019'	WHERE county_desc = 	'BRUNSWICK'	; " \
              "UPDATE {0} SET county_id = 	'37021'	WHERE county_desc = 	'BUOMBE'	; " \
              "UPDATE {0} SET county_id = 	'37023'	WHERE county_desc = 	'BURKE'	; " \
              "UPDATE {0} SET county_id = 	'37025'	WHERE county_desc = 	'CABARRUS'	; " \
              "UPDATE {0} SET county_id = 	'37027'	WHERE county_desc = 	'CALDWELL'	; " \
              "UPDATE {0} SET county_id = 	'37029'	WHERE county_desc = 	'CAMDEN'	; " \
              "UPDATE {0} SET county_id = 	'37031'	WHERE county_desc = 	'CARTERET'	; " \
              "UPDATE {0} SET county_id = 	'37033'	WHERE county_desc = 	'CASWELL'	; " \
              "UPDATE {0} SET county_id = 	'37035'	WHERE county_desc = 	'CATAWBA'	; " \
              "UPDATE {0} SET county_id = 	'37037'	WHERE county_desc = 	'CHATHAM'	; " \
              "UPDATE {0} SET county_id = 	'37039'	WHERE county_desc = 	'CHEROKEE'	; " \
              "UPDATE {0} SET county_id = 	'37041'	WHERE county_desc = 	'CHOWAN'	; " \
              "UPDATE {0} SET county_id = 	'37043'	WHERE county_desc = 	'CLAY'	; " \
              "UPDATE {0} SET county_id = 	'37045'	WHERE county_desc = 	'CLEVELAND'	; " \
              "UPDATE {0} SET county_id = 	'37047'	WHERE county_desc = 	'COLUMBUS'	; " \
              "UPDATE {0} SET county_id = 	'37049'	WHERE county_desc = 	'CRAVEN'	; " \
              "UPDATE {0} SET county_id = 	'37051'	WHERE county_desc = 	'CUMBERLAND'	; " \
              "UPDATE {0} SET county_id = 	'37053'	WHERE county_desc = 	'CURRITUCK'	; " \
              "UPDATE {0} SET county_id = 	'37055'	WHERE county_desc = 	'DARE'	; " \
              "UPDATE {0} SET county_id = 	'37057'	WHERE county_desc = 	'DAVIDSON'	; " \
              "UPDATE {0} SET county_id = 	'37059'	WHERE county_desc = 	'DAVIE'	; " \
              "UPDATE {0} SET county_id = 	'37061'	WHERE county_desc = 	'DUPLIN'	; " \
              "UPDATE {0} SET county_id = 	'37063'	WHERE county_desc = 	'DURHAM'	; " \
              "UPDATE {0} SET county_id = 	'37065'	WHERE county_desc = 	'EDGECOMBE'	; " \
              "UPDATE {0} SET county_id = 	'37067'	WHERE county_desc = 	'FORSYTH'	; " \
              "UPDATE {0} SET county_id = 	'37069'	WHERE county_desc = 	'FRANKLIN'	; " \
              "UPDATE {0} SET county_id = 	'37071'	WHERE county_desc = 	'GASTON'	; " \
              "UPDATE {0} SET county_id = 	'37073'	WHERE county_desc = 	'GATES'	; " \
              "UPDATE {0} SET county_id = 	'37075'	WHERE county_desc = 	'GRAHAM'	; " \
              "UPDATE {0} SET county_id = 	'37077'	WHERE county_desc = 	'GRANVILLE'	; " \
              "UPDATE {0} SET county_id = 	'37079'	WHERE county_desc = 	'GREENE'	; " \
              "UPDATE {0} SET county_id = 	'37081'	WHERE county_desc = 	'GUILFORD'	; " \
              "UPDATE {0} SET county_id = 	'37083'	WHERE county_desc = 	'HALIFAX'	; " \
              "UPDATE {0} SET county_id = 	'37085'	WHERE county_desc = 	'HARNETT'	; " \
              "UPDATE {0} SET county_id = 	'37087'	WHERE county_desc = 	'HAYWOOD'	; " \
              "UPDATE {0} SET county_id = 	'37089'	WHERE county_desc = 	'HENDERSON'	; " \
              "UPDATE {0} SET county_id = 	'37091'	WHERE county_desc = 	'HERTFORD'	; " \
              "UPDATE {0} SET county_id = 	'37093'	WHERE county_desc = 	'HOKE'	; " \
              "UPDATE {0} SET county_id = 	'37095'	WHERE county_desc = 	'HYDE'	; " \
              "UPDATE {0} SET county_id = 	'37097'	WHERE county_desc = 	'IREDELL'	; " \
              "UPDATE {0} SET county_id = 	'37099'	WHERE county_desc = 	'JACKSON'	; " \
              "UPDATE {0} SET county_id = 	'37101'	WHERE county_desc = 	'JOHNSTON'	; " \
              "UPDATE {0} SET county_id = 	'37103'	WHERE county_desc = 	'JONES'	; " \
              "UPDATE {0} SET county_id = 	'37105'	WHERE county_desc = 	'LEE'	; " \
              "UPDATE {0} SET county_id = 	'37107'	WHERE county_desc = 	'LENOIR'	; " \
              "UPDATE {0} SET county_id = 	'37109'	WHERE county_desc = 	'LIOLN'	; " \
              "UPDATE {0} SET county_id = 	'37111'	WHERE county_desc = 	'MCDOWELL'	; " \
              "UPDATE {0} SET county_id = 	'37113'	WHERE county_desc = 	'MACON'	; " \
              "UPDATE {0} SET county_id = 	'37115'	WHERE county_desc = 	'MADISON'	; " \
              "UPDATE {0} SET county_id = 	'37117'	WHERE county_desc = 	'MARTIN'	; " \
              "UPDATE {0} SET county_id = 	'37119'	WHERE county_desc = 	'MECKLENBURG'	; " \
              "UPDATE {0} SET county_id = 	'37121'	WHERE county_desc = 	'MITCHELL'	; " \
              "UPDATE {0} SET county_id = 	'37123'	WHERE county_desc = 	'MONTGOMERY'	; " \
              "UPDATE {0} SET county_id = 	'37125'	WHERE county_desc = 	'MOORE'	; " \
              "UPDATE {0} SET county_id = 	'37127'	WHERE county_desc = 	'NASH'	; " \
              "UPDATE {0} SET county_id = 	'37129'	WHERE county_desc = 	'NEW'	; " \
              "UPDATE {0} SET county_id = 	'37131'	WHERE county_desc = 	'NORTHAMPTON'	; " \
              "UPDATE {0} SET county_id = 	'37133'	WHERE county_desc = 	'ONSLOW'	; " \
              "UPDATE {0} SET county_id = 	'37135'	WHERE county_desc = 	'ORANGE'	; " \
              "UPDATE {0} SET county_id = 	'37137'	WHERE county_desc = 	'PAMLICO'	; " \
              "UPDATE {0} SET county_id = 	'37139'	WHERE county_desc = 	'PASQUOTANK'	; " \
              "UPDATE {0} SET county_id = 	'37141'	WHERE county_desc = 	'PENDER'	; " \
              "UPDATE {0} SET county_id = 	'37143'	WHERE county_desc = 	'PERQUIMANS'	; " \
              "UPDATE {0} SET county_id = 	'37145'	WHERE county_desc = 	'PERSON'	; " \
              "UPDATE {0} SET county_id = 	'37147'	WHERE county_desc = 	'PITT'	; " \
              "UPDATE {0} SET county_id = 	'37149'	WHERE county_desc = 	'POLK'	; " \
              "UPDATE {0} SET county_id = 	'37151'	WHERE county_desc = 	'RANDOLPH'	; " \
              "UPDATE {0} SET county_id = 	'37153'	WHERE county_desc = 	'RICHMOND'	; " \
              "UPDATE {0} SET county_id = 	'37155'	WHERE county_desc = 	'ROBESON'	; " \
              "UPDATE {0} SET county_id = 	'37157'	WHERE county_desc = 	'ROCKINGHAM'	; " \
              "UPDATE {0} SET county_id = 	'37159'	WHERE county_desc = 	'ROWAN'	; " \
              "UPDATE {0} SET county_id = 	'37161'	WHERE county_desc = 	'RUTHERFORD'	; " \
              "UPDATE {0} SET county_id = 	'37163'	WHERE county_desc = 	'SAMPSON'	; " \
              "UPDATE {0} SET county_id = 	'37165'	WHERE county_desc = 	'SCOTLAND'	; " \
              "UPDATE {0} SET county_id = 	'37167'	WHERE county_desc = 	'STANLY'	; " \
              "UPDATE {0} SET county_id = 	'37169'	WHERE county_desc = 	'STOKES'	; " \
              "UPDATE {0} SET county_id = 	'37171'	WHERE county_desc = 	'SURRY'	; " \
              "UPDATE {0} SET county_id = 	'37173'	WHERE county_desc = 	'SWAIN'	; " \
              "UPDATE {0} SET county_id = 	'37175'	WHERE county_desc = 	'TRANSYLVANIA'	; " \
              "UPDATE {0} SET county_id = 	'37177'	WHERE county_desc = 	'TYRRELL'	; " \
              "UPDATE {0} SET county_id = 	'37179'	WHERE county_desc = 	'UNION'	; " \
              "UPDATE {0} SET county_id = 	'37181'	WHERE county_desc = 	'VAE'	; " \
              "UPDATE {0} SET county_id = 	'37183'	WHERE county_desc = 	'WAKE'	; " \
              "UPDATE {0} SET county_id = 	'37185'	WHERE county_desc = 	'WARREN'	; " \
              "UPDATE {0} SET county_id = 	'37187'	WHERE county_desc = 	'WASHINGTON'	; " \
              "UPDATE {0} SET county_id = 	'37189'	WHERE county_desc = 	'WATAUGA'	; " \
              "UPDATE {0} SET county_id = 	'37191'	WHERE county_desc = 	'WAYNE'	; " \
              "UPDATE {0} SET county_id = 	'37193'	WHERE county_desc = 	'WILKES'	; " \
              "UPDATE {0} SET county_id = 	'37195'	WHERE county_desc = 	'WILSON'	; " \
              "UPDATE {0} SET county_id = 	'37197'	WHERE county_desc = 	'YADKIN'	; " \
              "UPDATE {0} SET county_id = 	'37199'	WHERE county_desc = 	'YAEY'	;".format(create_filepath())
        cursor.execute(sql)
        connection.commit()
finally:
    connection.close()