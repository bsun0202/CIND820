--############Checking for Common Cities

select distinct
 a.City, b.city WCity
from CIND820..Pollution_Raw a
full join 
CIND820..WCityGPS b
on a.City = b.City

--############Replace Name of Cities in Pollution data to match weather data

update CIND820..Pollution_Raw
set City = 'Saint Louis'
where City = 'St. Louis'

update CIND820..Pollution_Raw
set City = 'Indianapolis'
where City like '%indiana%'

--##############filter by common dates and cities into new table

select * 
into CIND820..PRawFiltered
from CIND820..Pollution_Raw 
where [Date Local] >= '2012-10-01'
and [Date Local] <= '2016-05-31'
and City in 
	(select distinct 
	City
	from CIND820..WCityGPS)

--##########UnPivot weather data tables

	--#####Humidity
select  
[datetime] LocalDate
, City
, HumidityValue
into CIND820..WUnpivHumidity
from CIND820..Whumidity b
unpivot
(
	HumidityValue
	for City in ([Vancouver]
      ,[Portland]
      ,[San Francisco]
      ,[Seattle]
      ,[Los Angeles]
      ,[San Diego]
      ,[Las Vegas]
      ,[Phoenix]
      ,[Albuquerque]
      ,[Denver]
      ,[San Antonio]
      ,[Dallas]
      ,[Houston]
      ,[Kansas City]
      ,[Minneapolis]
      ,[Saint Louis]
      ,[Chicago]
      ,[Nashville]
      ,[Indianapolis]
      ,[Atlanta]
      ,[Detroit]
      ,[Jacksonville]
      ,[Charlotte]
      ,[Miami]
      ,[Pittsburgh]
      ,[Toronto]
      ,[Philadelphia]
      ,[New York]
      ,[Montreal]
      ,[Boston]
      ,[Beersheba]
      ,[Tel Aviv District]
      ,[Eilat]
      ,[Haifa]
      ,[Nahariyya]
      ,[Jerusalem]
))as unpiv;


	--###Pressure
select  
[datetime] LocalDate
, City
, PressureValue
into CIND820..WUnpivPressure
FROM [CIND820].[dbo].[Wpressure]
unpivot
(
	PressureValue
	for City in ([Vancouver]
      ,[Portland]
      ,[San Francisco]
      ,[Seattle]
      ,[Los Angeles]
      ,[San Diego]
      ,[Las Vegas]
      ,[Phoenix]
      ,[Albuquerque]
      ,[Denver]
      ,[San Antonio]
      ,[Dallas]
      ,[Houston]
      ,[Kansas City]
      ,[Minneapolis]
      ,[Saint Louis]
      ,[Chicago]
      ,[Nashville]
      ,[Indianapolis]
      ,[Atlanta]
      ,[Detroit]
      ,[Jacksonville]
      ,[Charlotte]
      ,[Miami]
      ,[Pittsburgh]
      ,[Toronto]
      ,[Philadelphia]
      ,[New York]
      ,[Montreal]
      ,[Boston]
      ,[Beersheba]
      ,[Tel Aviv District]
      ,[Eilat]
      ,[Haifa]
      ,[Nahariyya]
      ,[Jerusalem]
))as unpiv;

	--###Temperature
select  
[datetime] LocalDate
, City
, TemperatureValue
into CIND820..WUnpivTemperature
FROM [CIND820].[dbo].[Wtemperature]
unpivot
(
	TemperatureValue
	for City in ([Vancouver]
      ,[Portland]
      ,[San Francisco]
      ,[Seattle]
      ,[Los Angeles]
      ,[San Diego]
      ,[Las Vegas]
      ,[Phoenix]
      ,[Albuquerque]
      ,[Denver]
      ,[San Antonio]
      ,[Dallas]
      ,[Houston]
      ,[Kansas City]
      ,[Minneapolis]
      ,[Saint Louis]
      ,[Chicago]
      ,[Nashville]
      ,[Indianapolis]
      ,[Atlanta]
      ,[Detroit]
      ,[Jacksonville]
      ,[Charlotte]
      ,[Miami]
      ,[Pittsburgh]
      ,[Toronto]
      ,[Philadelphia]
      ,[New York]
      ,[Montreal]
      ,[Boston]
      ,[Beersheba]
      ,[Tel Aviv District]
      ,[Eilat]
      ,[Haifa]
      ,[Nahariyya]
      ,[Jerusalem]
))as unpiv;
	--###WindDirection
select  
[datetime] LocalDate
, City
, WindDirectionValue
into CIND820..WUnpivWindDirection
FROM [CIND820].[dbo].[Wwind_direction]
unpivot
(
	WindDirectionValue
	for City in ([Vancouver]
      ,[Portland]
      ,[San Francisco]
      ,[Seattle]
      ,[Los Angeles]
      ,[San Diego]
      ,[Las Vegas]
      ,[Phoenix]
      ,[Albuquerque]
      ,[Denver]
      ,[San Antonio]
      ,[Dallas]
      ,[Houston]
      ,[Kansas City]
      ,[Minneapolis]
      ,[Saint Louis]
      ,[Chicago]
      ,[Nashville]
      ,[Indianapolis]
      ,[Atlanta]
      ,[Detroit]
      ,[Jacksonville]
      ,[Charlotte]
      ,[Miami]
      ,[Pittsburgh]
      ,[Toronto]
      ,[Philadelphia]
      ,[New York]
      ,[Montreal]
      ,[Boston]
      ,[Beersheba]
      ,[Tel Aviv District]
      ,[Eilat]
      ,[Haifa]
      ,[Nahariyya]
      ,[Jerusalem]
))as unpiv;
	--###WindSpeed
select  
[datetime] LocalDate
, City
, WindSpeedValue
into CIND820..WUnpivWindSpeed
FROM [CIND820].[dbo].[Wwind_speed]
unpivot
(
	WindSpeedValue
	for City in ([Vancouver]
      ,[Portland]
      ,[San Francisco]
      ,[Seattle]
      ,[Los Angeles]
      ,[San Diego]
      ,[Las Vegas]
      ,[Phoenix]
      ,[Albuquerque]
      ,[Denver]
      ,[San Antonio]
      ,[Dallas]
      ,[Houston]
      ,[Kansas City]
      ,[Minneapolis]
      ,[Saint Louis]
      ,[Chicago]
      ,[Nashville]
      ,[Indianapolis]
      ,[Atlanta]
      ,[Detroit]
      ,[Jacksonville]
      ,[Charlotte]
      ,[Miami]
      ,[Pittsburgh]
      ,[Toronto]
      ,[Philadelphia]
      ,[New York]
      ,[Montreal]
      ,[Boston]
      ,[Beersheba]
      ,[Tel Aviv District]
      ,[Eilat]
      ,[Haifa]
      ,[Nahariyya]
      ,[Jerusalem]
))as unpiv;
	--###Description
select  
[datetime] LocalDate
, City
, WeatherDescription
into CIND820..WUnpivWeatherDescription
FROM [CIND820].[dbo].[Wdescription]
unpivot
(
	WeatherDescription
	for City in ([Vancouver]
      ,[Portland]
      ,[San Francisco]
      ,[Seattle]
      ,[Los Angeles]
      ,[San Diego]
      ,[Las Vegas]
      ,[Phoenix]
      ,[Albuquerque]
      ,[Denver]
      ,[San Antonio]
      ,[Dallas]
      ,[Houston]
      ,[Kansas City]
      ,[Minneapolis]
      ,[Saint Louis]
      ,[Chicago]
      ,[Nashville]
      ,[Indianapolis]
      ,[Atlanta]
      ,[Detroit]
      ,[Jacksonville]
      ,[Charlotte]
      ,[Miami]
      ,[Pittsburgh]
      ,[Toronto]
      ,[Philadelphia]
      ,[New York]
      ,[Montreal]
      ,[Boston]
      ,[Beersheba]
      ,[Tel Aviv District]
      ,[Eilat]
      ,[Haifa]
      ,[Nahariyya]
      ,[Jerusalem]
))as unpiv;

--###fix values stored as string

update WUnpivHumidity
set LocalDate = CONVERT(date, LocalDate, 120) from WUnpivHumidity

update WUnpivPressure
set LocalDate = CONVERT(date, LocalDate, 120) from WUnpivPressure

update WUnpivTemperature
set LocalDate = CONVERT(date, LocalDate, 120) from WUnpivTemperature

update WUnpivWeatherDescription
set LocalDate = CONVERT(date, LocalDate, 120) from WUnpivWeatherDescription

update WUnpivWindDirection
set LocalDate = CONVERT(date, LocalDate, 120) from WUnpivWindDirection

update WUnpivWindSpeed
set LocalDate = CONVERT(date, LocalDate, 120) from WUnpivWindSpeed


--####average values by date and join
select a.LocalDate
, a.City
, a.HumidityValue
, b.PressureValue
, c.TemperatureValue
, d.WindDirectionValue
, e.WindSpeedValue

into WJoinedTable

from 

(select [LocalDate]
, City
, avg(CONVERT(float, HumidityValue) ) HumidityValue 
 from
 WUnpivHumidity a
 group by [LocalDate]
 , City
) a

 left join 
 (select [LocalDate]
, City
, avg(CONVERT(float, PressureValue) ) PressureValue 
 from
 WUnpivPressure a
 group by [LocalDate]
 , City
) b

 on a.City = b.City
 and a.LocalDate = b.LocalDate

  left join 
 (select [LocalDate]
, City
, avg(CONVERT(float, TemperatureValue) ) TemperatureValue 
 from
 WUnpivTemperature 
 group by [LocalDate]
 , City
) c

 on a.City = c.City
 and a.LocalDate = c.LocalDate

   left join 
 (select [LocalDate]
, City
, avg(CONVERT(float, WindDirectionValue) ) WindDirectionValue 
 from
 WUnpivWindDirection 
 group by [LocalDate]
 , City
) d

 on a.City = d.City
 and a.LocalDate = d.LocalDate
 
    left join 
 (select [LocalDate]
, City
, avg(CONVERT(float, WindSpeedValue) ) WindSpeedValue 
 from
 WUnpivWindSpeed
 group by [LocalDate]
 , City
) e

 on a.City = e.City
 and a.LocalDate = e.LocalDate
 
--#### Drop Unnecessary columns in Pollution Data
alter table PRawFiltered
drop column 
		[Column 0]
	  ,[State Code]
      ,[County Code]
      ,[Site Num]
      ,[Address]
      ,[State]
      ,[County]
	  ,[NO2 Units]
      ,[NO2 1st Max Value]
      ,[NO2 1st Max Hour]
      ,[NO2 AQI]
      ,[O3 Units]
      ,[O3 1st Max Value]
      ,[O3 1st Max Hour]
      ,[O3 AQI]
      ,[SO2 Units]
      ,[SO2 1st Max Value]
      ,[SO2 1st Max Hour]
      ,[SO2 AQI]
      ,[CO Units]
      ,[CO 1st Max Value]
      ,[CO 1st Max Hour]
      ,[CO AQI]
;

select distinct * from PRawFiltered

--Join Weather and Pollution Dataset total 17069 records
select  a.*, b.NO2,b.O3, b.SO2, b.CO
into CleanedAndJoinedData
from WJoinedTable a
inner join 
(select City, [Date Local], AVG(CONVERT(float, [NO2 Mean])) NO2, AVG(CONVERT(float, [O3 Mean])) O3, AVG(CONVERT(float, [SO2 Mean])) SO2, AVG(CONVERT(float, [CO Mean])) CO
from
PRawFiltered 
group by City, [Date Local]
)b
on a.LocalDate = b.[Date Local]
and a.City = b.City