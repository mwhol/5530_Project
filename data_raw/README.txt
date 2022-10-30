Downloaded from: https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents
Download the zip file from the above link, extract the csv to the raw folder then begin running the scripts

#	Attribute	Nullable	Description
1	ID	No	This is a unique identifier of the accident record.
2	Severity	No	Shows the severity of the accident, a number between 1 and 4, where 1 indicates the least impact on traffic (i.e., short delay as a result of the accident) and 4 indicates a significant impact on traffic (i.e., long delay).
3	Start_Time	No	Shows start time of the accident in local time zone.
4	End_Time	No	Shows end time of the accident in local time zone. End time here refers to when the impact of accident on traffic flow was dismissed.
5	Start_Lat	No	Shows latitude in GPS coordinate of the start point.
6	Start_Lng	No	Shows longitude in GPS coordinate of the start point.
7	End_Lat	Yes	Shows latitude in GPS coordinate of the end point.
8	End_Lng	Yes	Shows longitude in GPS coordinate of the end point.
9	Distance(mi)	No	The length of the road extent affected by the accident.
10	Description	No	Shows natural language description of the accident.
11	Number	Yes	Shows the street number in address field.
12	Street	Yes	Shows the street name in address field.
13	Side	Yes	Shows the relative side of the street (Right/Left) in address field.
14	City	Yes	Shows the city in address field.
15	County	Yes	Shows the county in address field.
16	State	Yes	Shows the state in address field.
17	Zipcode	Yes	Shows the zipcode in address field.
18	Country	Yes	Shows the country in address field.
19	Timezone	Yes	Shows timezone based on the location of the accident (eastern, central, etc.).
20	Airport_Code	Yes	Denotes an airport-based weather station which is the closest one to location of the accident.
21	Weather_Timestamp	Yes	Shows the time-stamp of weather observation record (in local time).
22	Temperature(F)	Yes	Shows the temperature (in Fahrenheit).
23	Wind_Chill(F)	Yes	Shows the wind chill (in Fahrenheit).
24	Humidity(%)	Yes	Shows the humidity (in percentage).
25	Pressure(in)	Yes	Shows the air pressure (in inches).
26	Visibility(mi)	Yes	Shows visibility (in miles).
27	Wind_Direction	Yes	Shows wind direction.
28	Wind_Speed(mph)	Yes	Shows wind speed (in miles per hour).
29	Precipitation(in)	Yes	Shows precipitation amount in inches, if there is any.
30	Weather_Condition	Yes	Shows the weather condition (rain, snow, thunderstorm, fog, etc.)
31	Amenity	No	A POI annotation which indicates presence of amenity in a nearby location.
32	Bump	No	A POI annotation which indicates presence of speed bump or hump in a nearby location.
33	Crossing	No	A POI annotation which indicates presence of crossing in a nearby location.
34	Give_Way	No	A POI annotation which indicates presence of give_way in a nearby location.
35	Junction	No	A POI annotation which indicates presence of junction in a nearby location.
36	No_Exit	No	A POI annotation which indicates presence of no_exit in a nearby location.
37	Railway	No	A POI annotation which indicates presence of railway in a nearby location.
38	Roundabout	No	A POI annotation which indicates presence of roundabout in a nearby location.
39	Station	No	A POI annotation which indicates presence of station in a nearby location.
40	Stop	No	A POI annotation which indicates presence of stop in a nearby location.
41	Traffic_Calming	No	A POI annotation which indicates presence of traffic_calming in a nearby location.
42	Traffic_Signal	No	A POI annotation which indicates presence of traffic_signal in a nearby loction.
43	Turning_Loop	No	A POI annotation which indicates presence of turning_loop in a nearby location.
44	Sunrise_Sunset	Yes	Shows the period of day (i.e. day or night) based on sunrise/sunset.
45	Civil_Twilight	Yes	Shows the period of day (i.e. day or night) based on civil twilight.
46	Nautical_Twilight	Yes	Shows the period of day (i.e. day or night) based on nautical twilight.
47	Astronomical_Twilight	Yes	Shows the period of day (i.e. day or night) based on astronomical twilight.

From: https://smoosavi.org/datasets/us_accidents