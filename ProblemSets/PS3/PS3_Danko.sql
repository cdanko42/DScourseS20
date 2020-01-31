
--create table for insurance
CREATE TABLE insurance(
policyID INTEGER,
statecode CHAR,
county CHAR,
eq_site_limit REAL,
hu_site_limit REAL,
fl_site_limit REAL,
fr_site_limit REAL,
tiv_2011 REAL,
tiv_2012 REAL,
eq_site_deductible REAL,
hu_site_deductible REAL,
fl_site_deductible REAL,
fr_site_deductible REAL,
point_latitude REAL,
point_longitude REAL,
line CHAR,
construction CHAR,
point_granularity INTEGER);
--import csv into the created table
.mode csv
.import FL_insurance_sample.csv insurance

--print 10 observations
SELECT * FROM insurance LIMIT 10;

--List distinct counties
SELECT DISTINCT(county) FROM insurance

--Take average of property appreciation across years
SELECT AVG(tiv_2012 - tiv_2011) FROM insurance;

 --Create frequency table for the construction variable
SELECT construction, COUNT(*) FROM insurance GROUP BY construction;
