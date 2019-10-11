# Xride_Bangalore
One of the leading ride-hailing companies, XRides in Bangalore has approached we help us to use its location data to make better decisions. They have shared a dataset of ~40,000 trips with their time, starting and ending locations along with some other attribute data. Here is the data: https://docs.google.com/spreadsheets/d/1a2lv-O9V8kjnHVdE4CEMqRLYFreE0fBAMHEfX81aQ2g/edit?usp=sharing

Data Description:
●	id - booking ID
●	user_id - the ID of the customer (based on mobile number)
●	vehicle_model_id - vehicle model type.
●	package_id - type of package (1=4hrs & 40kms, 2=8hrs & 80kms, 3=6hrs & 60kms, 4= 10hrs & 100kms, 5=5hrs & 50kms, 6=3hrs & 30kms, 7=12hrs & 120kms)
●	travel_type_id - type of travel (1=long distance, 2= point to point, 3= hourly rental).
●	from_area_id - unique identifier of area. Applicable only for point-to-point travel and packages
●	to_area_id - unique identifier of area. Applicable only for point-to-point travel
●	from_city_id - unique identifier of city
●	to_city_id - unique identifier of the city (only for intercity)
●	from_date - timestamp of the requested trip start
●	to_date - timestamp of the trip end
●	online_booking - if the booking was done on the desktop website
●	mobile_site_booking - if the booking was done on mobile website
●	booking_created - time stamp of booking
●	from_lat - latitude of from area
●	from_long -  longitude of from  area
●	to_lat - latitude of to area
●	to_long - longitude of to area
●	car_cancellation - whether the booking was canceled (1) or not (0) due to unavailability of a car.

Front-end Task
The goal for you in this task is to create a front-end application where the user can upload a CSV, select the Latitude and Longitude column of the CSV and then view all the points on the map.

Fairly, straight-forward right?

Now the tricky part, after creating this basic visualization, you need to create some charts and graphs that depict a story and are useful for the end-user. For example, you could plot the medium of booking, a visualization of distances or a plain-old time-series histogram.

Remember, Design Matters!!

Our ideal front-end framework is Vue/React. But choose a framework that you’re most comfortable with.
Use Mapbox-GL-JS or Leaflet-JS for the map visualization and your favorite charting library.

Back-end Task
Problem Statement:
SQL is an extremely powerful language that allows engineers to perform multiple complex operations on data. However, it’s still hard for a non-engineer to pass through the hoops of multiple joins and group-by. As engineers, you know that technology only works when everyone has access to it. Your task is to create a modular Postgres query builder which accepts an input of a set of nodes where each node will select one attribute and the user can combine multiple nodes to get the metrics that he wants easily.


Data Science Task
Problem Statement:
Use any preferred language to build a surge pricing model to recognize areas in the city with high demand and measure how does this demand compares to the rest of the city. The dataset provided is from XRides where you select the type of travel and car, and then go from point A to point B. It contains 40,000 trips. 
Deliverable:
The model should be able to process the data in close to real-time and give a surge multiplier of every area. Identify what areas and at what times get most bookings and how would you increase the price in those areas in order to meet the demand.
Note: 
Get into the shoes of a data scientist working in XRides, such that you have to give recommendations to your business team or strategy backed by data that helps them reduce cancellation, increase revenue or reduce costs. Keep the recommendations very brief and crisp!
