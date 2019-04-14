### 1. Exploring the Olympics Dataset

-- Query the sport and distinct number of athletes
SELECT 
    sport, 
    COUNT(DISTINCT athlete_id) AS athletes
FROM summer_games
GROUP BY sport
-- Only include the 3 sports with the most athletes
ORDER BY athletes
LIMIT 3;

-- Query sport, events, and athletes from summer_games
SELECT 
    sport, 
    COUNT(DISTINCT event) AS events, 
    COUNT(DISTINCT athlete_id) AS athletes
FROM summer_games
GROUP BY sport;

-- Select the age of the oldest athlete for each region
SELECT 
    region, 
    MAX(age) AS age_of_oldest_athlete
FROM athletes a
-- First JOIN statement
JOIN summer_games s
ON a.id = s.athlete_id
-- Second JOIN statement
JOIN countries c
ON s.country_id = c.id
GROUP BY region;

-- Select sport and events for summer sports
SELECT 
    sport, 
    COUNT(DISTINCT event) AS events
FROM summer_games
GROUP BY sport
UNION
-- Select sport and events for winter sports
SELECT 
    sport, 
    COUNT(DISTINCT event) AS events
FROM winter_games
GROUP BY sport
-- Show the most athletes at the top of the report
ORDER BY events DESC;

-- Add the rows column to your query
SELECT 
    bronze, 
    COUNT(*) AS rows
FROM summer_games
GROUP BY bronze;

/* Pull total_bronze_medals below
SELECT SUM(bronze) AS total_bronze_medals
FROM summer_games; 
>> OUTPUT = 141 total_bronze_medals */

-- Select the total bronze_medals from your query
SELECT SUM(bronze_medals)
FROM 
-- Previous query is shown below.  Alias this AS subquery
  (SELECT 
      country, 
      SUM(bronze) AS bronze_medals
  FROM summer_games AS s
  JOIN countries AS c
  ON s.country_id = c.id
  GROUP BY country) AS subquery
;

-- Pull athlete_name and gold_medals for summer games
SELECT 
    a.name AS athlete_name, 
    SUM(gold) AS gold_medals
FROM summer_games s
JOIN athletes a
ON s.athlete_id = a.id
GROUP BY a.name
-- Filter for only athletes with 3 gold medals or more (not alias)
HAVING SUM(gold) >= 3
-- Sort to show the most gold medals at the top
ORDER BY gold_medals DESC;

#######################

### 2. Creating Reports

-- Query season, country, and events for all summer events
SELECT 
  'summer' AS season, 
    country, 
    COUNT(DISTINCT event) AS events
FROM countries c
JOIN summer_games s
ON c.id = s.country_id
GROUP BY country
-- Combine the queries
UNION ALL
-- Query season, country, and events for all winter events
SELECT 
  'winter' AS season, 
    country, 
    COUNT(DISTINCT event) AS events
FROM countries c
JOIN winter_games w
ON c.id = w.country_id
GROUP BY country
-- Sort the results to show most events at the top
ORDER BY events DESC;

-- Add outer layer to pull season, country and unique events
SELECT 
  season, 
    country, 
    COUNT(DISTINCT event) AS events
FROM
    -- Pull season, country_id, and event for both seasons
    (SELECT 
      'summer' AS season, 
      country_id, 
      event
    FROM summer_games
    UNION ALL
    SELECT 
      'winter' AS season, 
      country_id, 
      event
    FROM winter_games) AS subquery
JOIN countries AS c
ON subquery.country_id = c.id
-- Group by any unaggregated fields
GROUP BY season, country
-- Order to show most events at the top
ORDER BY events DESC;

SELECT 
  name,
    -- Output 'Tall Female', 'Tall Male', or 'Other'
  CASE WHEN (gender = 'F' AND height >= 175) THEN 'Tall Female'
    WHEN (gender = 'M' AND height >= 190) THEN 'Tall Male'
    ELSE 'Other' END AS segment
FROM athletes;

-- Pull in sport, bmi_bucket, and athletes
SELECT 
  sport,
    -- Bucket BMI in three groups: <.25, .25-.30, and >.30  
    CASE WHEN weight/height^2*100 <.25 THEN '<.25'
    WHEN weight/height^2*100 <=.30 THEN '.25-.30'
    WHEN weight/height^2*100 >.30 THEN '>.30' END AS bmi_bucket,
    COUNT(DISTINCT athlete_id) AS athletes
FROM summer_games AS sg
JOIN athletes AS a
ON sg.athlete_id = a.id
-- GROUP BY non-aggregated fields
GROUP BY sport, bmi_bucket
-- Sort by sport and then by athletes in descending order
ORDER BY sport, athletes DESC;

-- Uncomment the original query
SELECT 
  sport,
    CASE WHEN weight/height^2*100 <.25 THEN '<.25'
    WHEN weight/height^2*100 <=.30 THEN '.25-.30'
    WHEN weight/height^2*100 >.30 THEN '>.30'
    -- Add ELSE statement to output 'no weight recorded'
    ELSE 'no weight recorded' END AS bmi_bucket,
    COUNT(DISTINCT athlete_id) AS athletes
FROM summer_games AS sg
JOIN athletes AS a
ON sg.athlete_id = a.id
GROUP BY sport, bmi_bucket
ORDER BY sport, athletes DESC;

-- Comment out the troubleshooting query
/*SELECT 
  height, 
    weight, 
    weight/height^2*100 AS bmi
FROM athletes
WHERE weight/height^2*100 IS NULL;*/

#############################

### 3. Cleaning & Validation

-- Pull column_name & data_type from the columns table
SELECT 
	column_name,
    data_type
FROM information_schema.columns
-- Filter for the table 'country_stats'
WHERE table_name = 'country_stats';

-- Run the query, then convert a data type to fix the error
/*SELECT AVG(CAST (pop_in_millions AS float)) AS avg_population
FROM country_stats;*/

SELECT s.country_id, 
    COUNT(DISTINCT s.athlete_id) AS summer_athletes, 
    COUNT(DISTINCT w.athlete_id) AS winter_athletes
FROM summer_games AS s
JOIN winter_games_str AS w
ON s.country_id = CAST (w.country_id AS float)
GROUP BY s.country_id;

-- Comment out the previous query
/*SELECT AVG(CAST(pop_in_millions AS float)) AS avg_population
FROM country_stats;*/

-- Uncomment the following block & run the query
SELECT 
	s.country_id, 
    COUNT(DISTINCT s.athlete_id) AS summer_athletes, 
    COUNT(DISTINCT w.athlete_id) AS winter_athletes
FROM summer_games AS s
JOIN winter_games_str AS w
-- Fix the error by making both columns integers
ON s.country_id = CAST (w.country_id AS int)
GROUP BY s.country_id;

SELECT year,
    -- Pull decade, decade_truncate, and the world's gdp
    DATE_PART('decade',CAST(year AS date)) AS decade,
    DATE_TRUNC('decade',CAST(year AS date)) AS decade_truncated,
    SUM(gdp) AS world_gdp
FROM country_stats
-- Group and order by year in descending order
GROUP BY year
ORDER BY year DESC;

-- Convert country to lower case
SELECT country, 
    LOWER(country) AS country_altered
FROM countries
GROUP BY country;
			     
-- Convert country to proper case
SELECT country, 
    INITCAP(country) AS country_altered
FROM countries
GROUP BY country;

-- Output the left 3 characters of country
SELECT country, 
    LEFT(country, 3) AS country_altered
FROM countries
GROUP BY country;

-- Output all characters starting with position 7
SELECT country, 
    SUBSTRING(country FROM 7) AS country_altered
FROM countries
GROUP BY country;

SELECT 
	region, 
    -- Replace all '&' characters with the string 'and'
    REPLACE(region, '&', 'and') AS character_swap,
    -- Remove all periods
    REPLACE(region, '.', '') AS character_remove
FROM countries
WHERE region = 'LATIN AMER. & CARIB'
GROUP BY region;

SELECT 
	region, 
    -- Replace all '&' characters with the string 'and'
    REPLACE(region,'&','and') AS character_swap,
    -- Remove all periods
    REPLACE(region,'.','') AS character_remove,
    -- Combine the functions to run both changes at once
    ____ AS character_swap_and_remove
FROM countries
WHERE region = 'LATIN AMER. & CARIB'
GROUP BY region;

-- Pull event and unique athletes from summer_games_messy 
SELECT
    -- Remove trailing spaces and alias as event_fixed
	event, 
    COUNT(DISTINCT athlete_id) AS athletes
FROM summer_games_messy
-- Update the group by accordingly
GROUP BY event;

-- Pull event and unique athletes from summer_games_messy 
SELECT 
    -- Remove dashes from all event values
    TRIM(event) AS event_fixed, 
    COUNT(DISTINCT athlete_id) AS athletes
FROM summer_games_messy
-- Update the group by accordingly
GROUP BY event_fixed;

-- Pull event and unique athletes from summer_games_messy 
SELECT 
    -- Remove dashes from all event values
    REPLACE(TRIM(event),'-','') AS event_fixed, 
    COUNT(DISTINCT athlete_id) AS athletes
FROM summer_games_messy
-- Update the group by accordingly
GROUP BY event_fixed;
			     
-- Show total gold_medals by country
SELECT country,
    SUM(gold) AS gold_medals
FROM winter_games AS w
JOIN countries AS c
ON w.country_id = c.id
GROUP BY country
-- Order by gold_medals in descending order
ORDER BY gold_medals DESC;
			     
-- Show total gold_medals by country
SELECT country, 
    SUM(gold) AS gold_medals
FROM winter_games AS w
JOIN countries AS c
ON w.country_id = c.id
-- Removes any row with no gold medals
WHERE gold IS NOT NULL
GROUP BY country
-- Order by gold_medals in descending order
ORDER BY gold_medals DESC;
			     
-- Show total gold_medals by country
SELECT 
	country, 
    SUM(gold) AS gold_medals
FROM winter_games AS w
JOIN countries AS c
ON w.country_id = c.id
-- Comment out the WHERE statement
WHERE gold IS NOT NULL
GROUP BY country
-- Replace WHERE statement with equivalent HAVING statement
HAVING SUM(gold) IS NOT NULL
-- Order by gold_medals in descending order
ORDER BY gold_medals DESC;

-- Pull events and golds by athlete_id for summer events
SELECT 
    athlete_id, 
    -- Add a field that averages the existing gold field
    AVG(gold) AS avg_golds,
    COUNT(event) AS total_events, 
    SUM(gold) AS gold_medals
FROM summer_games
GROUP BY athlete_id
-- Order by total_events descending and athlete_id ascending
ORDER BY total_events DESC, athlete_id;

-- Pull events and golds by athlete_id for summer events
SELECT 
    athlete_id, 
    -- Replace all null gold values with 0
    AVG(COALESCE(gold,0)) AS avg_golds,
    COUNT(event) AS total_events, 
    SUM(gold) AS gold_medals
FROM summer_games
GROUP BY athlete_id
-- Order by total_events descending and athlete_id ascending
ORDER BY total_events DESC, athlete_id;
		 
-- Comment out the query after noting the gold medal count
/*SELECT SUM(gold) AS gold_medals
FROM winter_games;*/
-- TOTAL GOLD MEDALS: 47 

-- Show gold_medals and avg_gdp by country_id
SELECT w.country_id, 
    SUM(gold) AS gold_medals, 
    AVG(gdp) AS avg_gdp
FROM winter_games AS w
JOIN country_stats AS c
-- Only join on the country_id fields
ON c.country_id = w.country_id
GROUP BY w.country_id;
		 
-- Comment out the query after noting the gold medal count
/*SELECT SUM(gold) AS gold_medals
FROM winter_games;*/
-- TOTAL GOLD MEDALS: 47 

-- Calculate the total gold_medals in your query
SELECT SUM(gold_medals)
FROM
	(SELECT 
        w.country_id, 
     	SUM(gold) AS gold_medals, 
        AVG(gdp) AS avg_gdp
    FROM winter_games AS w
    JOIN country_stats AS c
    ON c.country_id = w.country_id
    -- Alias your query as subquery
    GROUP BY w.country_id) AS subquery;
		 
SELECT SUM(gold_medals) AS gold_medals
FROM (SELECT 
      w.country_id, 
      SUM(gold) AS gold_medals, 
      AVG(gdp) AS avg_gdp
    FROM winter_games AS w
    JOIN country_stats AS c
    -- Update the subquery to join on a second field
    ON c.country_id = w.country_id AND c.year::date = w.year::date
    GROUP BY w.country_id) AS subquery;
		 
SELECT 
	country,
    -- Add the three medal fields using one sum function
	SUM(COALESCE(gold,0)+COALESCE(silver,0)+COALESCE(bronze,0)) AS medals
FROM summer_games AS s
JOIN countries AS c
ON c.id = s.country_id
GROUP BY country
ORDER BY medals DESC;
							 
SELECT 
	c.country,
    -- Pull in pop_in_millions and medals_per_million 
    pop_in_millions,
    -- Add the three medal fields using one sum function
	SUM(COALESCE(bronze,0) + COALESCE(silver,0) + COALESCE(gold,0)) AS medals,
    SUM(COALESCE(bronze,0) + COALESCE(silver,0) + COALESCE(gold,0)) / pop_in_millions::float AS medals_per_million
FROM summer_games AS s
JOIN countries AS c
ON s.country_id = c.id
-- Add a join
JOIN country_stats AS cs
ON cs.country_id = c.id
GROUP BY c.country, pop_in_millions
ORDER BY medals DESC;
							   
SELECT 
	c.country,
    -- Pull in pop_in_millions and medals_per_million 
	pop_in_millions,
    -- Add the three medal fields using one sum function
	SUM(COALESCE(bronze,0) + COALESCE(silver,0) + COALESCE(gold,0)) AS medals,
	SUM(COALESCE(bronze,0) + COALESCE(silver,0) + COALESCE(gold,0)) / CAST(cs.pop_in_millions AS float) AS medals_per_million
FROM summer_games AS s
JOIN countries AS c 
ON s.country_id = c.id
-- Update the newest join statement to remove duplication
JOIN country_stats AS cs 
ON s.country_id = cs.country_id AND cs.year::date = s.year::date
GROUP BY c.country, pop_in_millions
ORDER BY medals DESC;
							       
SELECT 
	-- Clean the country field to only show country_code
    LEFT(REPLACE(UPPER(TRIM(c.country)), '.', ''),3) AS country_code,
    -- Pull in pop_in_millions and medals_per_million 
	pop_in_millions,
    -- Add the three medal fields using one sum function
	SUM(COALESCE(bronze,0) + COALESCE(silver,0) + COALESCE(gold,0)) AS medals,
	SUM(COALESCE(bronze,0) + COALESCE(silver,0) + COALESCE(gold,0)) / CAST(cs.pop_in_millions AS float) AS medals_per_million
FROM summer_games AS s
JOIN countries AS c 
ON s.country_id = c.id
-- Update the newest join statement to remove duplication
JOIN country_stats AS cs 
ON s.country_id = cs.country_id AND s.year = CAST(cs.year AS date)
-- Filter out null populations
WHERE pop_in_millions IS NOT NULL
GROUP BY c.country, pop_in_millions
-- Keep only the top 25 medals_per_million rows
ORDER BY medals_per_million DESC
LIMIT 25;
							       
###############################
							       
### 4. Complex Calculations
							       
SELECT country_id,
    year,
    gdp,
    -- Show total gdp per country and alias accordingly
	AVG(gdp) OVER (PARTITION BY country_id) AS country_avg_gdp
FROM country_stats;
							       
SELECT country_id,
    year,
    gdp,
    -- Show total gdp per country and alias accordingly
	SUM(gdp) OVER (PARTITION BY country_id) AS country_sum_gdp
FROM country_stats;
							       
SELECT 
	country_id,
    year,
    gdp,
    -- Show max gdp per country and alias accordingly
	MAX(gdp) OVER (PARTITION BY country_id) AS country_max_gdp
FROM country_stats;
							       
SELECT country_id,
    year,
    gdp,
    -- Show max gdp for the table and alias accordingly
	MAX(gdp) OVER() AS global_max_gdp
FROM country_stats;
							       
-- Query total_golds by region and country_id
SELECT region, 
    country_id, 
    SUM(gold) AS total_golds
FROM summer_games_clean AS s
JOIN countries AS c
ON c.id = s.country_id
GROUP BY region, country_id;
			
-- Pull in avg_total_golds by region
SELECT 
	region,
    AVG(total_golds) AS avg_total_golds
FROM
  (SELECT 
      region, 
      country_id, 
      SUM(gold) AS total_golds
  FROM summer_games_clean AS s
  JOIN countries AS c
  ON s.country_id = c.id
  -- Alias the subquery
  GROUP BY region, country_id) AS subquery
GROUP BY region
-- Order by avg_total_golds in descending order
ORDER BY avg_total_golds DESC;
							       
SELECT 
	-- Query region, athlete_name, and total gold medals
    region, 
    name AS athlete_name, 
    SUM(gold) AS total_golds,
    -- Assign a regional rank to each athlete
    ROW_NUMBER() OVER (PARTITION BY region ORDER BY SUM(gold) DESC) AS row_num
FROM summer_games_clean AS s
JOIN athletes AS a
ON a.id = s.athlete_id
JOIN countries AS c
ON s.country_id = c.id
GROUP BY region, athlete_name;
							       
-- Query region, athlete name, and total_golds
SELECT 
	region, athlete_name, total_golds
FROM
    (SELECT 
		-- Query region, athlete_name, and total gold medals
        region, 
        name AS athlete_name, 
        SUM(gold) AS total_golds,
        -- Assign a regional rank to each athlete
        ROW_NUMBER() OVER (PARTITION BY region ORDER BY SUM(gold) DESC) AS row_num
    FROM summer_games_clean AS s
    JOIN athletes AS a
    ON a.id = s.athlete_id
    JOIN countries AS c
    ON s.country_id = c.id
    -- Alias as subquery
    GROUP BY region, athlete_name) AS subquery
-- Filter for only the top athlete per region
WHERE row_num = 1;
							       
-- Pull country_gdp by region and country
SELECT 
	region,
    country,
	SUM(gdp) AS country_gdp,
    -- Calculate the global gdp
    SUM(SUM(gdp)) OVER () AS global_gdp,
    -- Calculate percent of global gdp
    SUM(gdp) / SUM(SUM(gdp)) OVER () AS perc_global_gdp,
    -- Calculate percent of gdp relative to its region
    SUM(gdp) / SUM(SUM(gdp)) OVER (PARTITION BY region) AS perc_region_gdp
FROM country_stats AS cs
JOIN countries AS c
ON cs.country_id = c.id
-- Filter out null gdp values
WHERE gdp IS NOT NULL
GROUP BY region, country
-- Show the highest country_gdp at the top
ORDER BY country_gdp DESC;
		       
-- Bring in region, country, and gdp_per_million
SELECT 
    region,
    country,
    SUM(gdp) / SUM(pop_in_millions) AS gdp_per_million,
    -- Output the worlds gdp_per_million
    SUM(SUM(gdp)) OVER () / SUM(SUM(pop_in_millions)) OVER () AS gdp_per_million_total,
    -- Build the performance_index in the 3 lines below
    (SUM(gdp) / SUM(pop_in_millions))
    /
    (SUM(SUM(gdp)) OVER () / SUM(SUM(pop_in_millions)) OVER ()) AS performance_index
-- Pull from country_stats_clean
FROM country_stats_clean AS cs
JOIN countries AS c 
ON cs.country_id = c.id
-- Filter for 2016 and remove null gdp values
WHERE year = '2016-01-01' AND gdp IS NOT NULL
GROUP BY region, country
-- Show highest gdp_per_million at the top
ORDER BY gdp_per_million DESC;
				     
SELECT
	-- Pull month and country_id
	DATE_PART('month', date) AS month,
	country_id,
    -- Pull in current month views
    SUM(views) AS month_views,
    -- Pull in last month views
    LAG(SUM(views)) OVER (PARTITION BY country_id ORDER BY DATE_PART('month', date)) AS previous_month_views,
    -- Calculate the percent change
    SUM(views) / LAG(SUM(views)) OVER (PARTITION BY country_id ORDER BY DATE_PART('month', date)) - 1 AS perc_change
FROM web_data
WHERE date <= '2018-05-31'
GROUP BY month, country_id;
										  
SELECT
	-- Pull in date and daily_views
	date,
	SUM(views) AS daily_views,
    -- Calculate the rolling 7 day average
	AVG(SUM(views)) OVER (ORDER BY date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS weekly_avg
FROM web_data
GROUP BY date;
		
SELECT 
	-- Pull in date and weekly_avg
	date,
    weekly_avg,
    -- Output the value of weekly_avg from 7 days prior
    LAG(weekly_avg,7) OVER (ORDER BY date) AS weekly_avg_previous
FROM
  (SELECT
      -- Pull in date and daily_views
      date,
      SUM(views) AS daily_views,
      -- Calculate the rolling 7 day average
      AVG(SUM(views)) OVER (ORDER BY date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS weekly_avg
  FROM web_data
  -- Alias as subquery
  GROUP BY date) AS subquery
-- Order by date in descending order
ORDER BY date DESC;
	
SELECT 
	-- Pull in date and weekly_avg
	date,
    weekly_avg,
    -- Output the value of weekly_avg from 7 days prior
    LAG(weekly_avg,7) OVER (ORDER BY date) AS weekly_avg_previous,
    -- Calculate percent change vs previous period
    weekly_avg / LAG(weekly_avg,7) OVER (ORDER BY date) - 1 as perc_change
FROM
  (SELECT
      -- Pull in date and daily_views
      date,
      SUM(views) AS daily_views,
      -- Calculate the rolling 7 day average
      AVG(SUM(views)) OVER (ORDER BY date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS weekly_avg
  FROM web_data
  -- Alias as subquery
  GROUP BY date) AS subquery
-- Order by date in descending order
ORDER BY date DESC;
	  
SELECT 
	-- Pull in country_id and height
    country_id, 
    height, 
    -- Number the height of each country's athletes
    ROW_NUMBER() OVER (PARTITION BY country_id ORDER BY height DESC) AS row_num
FROM winter_games AS w 
JOIN athletes AS a 
ON w.athlete_id = a.id
GROUP BY country_id, height
-- Order by country_id and then height in descending order
ORDER BY country_id, height DESC;
	  
SELECT
	-- Pull in region and calculate avg tallest height
    region,
    AVG(height) AS avg_tallest
FROM countries AS c
JOIN
    (SELECT 
     	-- Pull in country_id and height
        country_id, 
        height, 
        -- Number the height of each country's athletes
        ROW_NUMBER() OVER (PARTITION BY country_id ORDER BY height DESC) AS row_num
    FROM winter_games AS w 
    JOIN athletes AS a 
    ON w.athlete_id = a.id
    GROUP BY country_id, height
    -- Alias as subquery
    ORDER BY country_id, height DESC) AS subquery
ON c.id = subquery.country_id
-- Only include the tallest height for each country
WHERE row_num = 1
GROUP BY region;
	  
SELECT
	-- Pull in region and calculate avg tallest height
    region,
    AVG(height) AS avg_tallest,
    -- Calculate region's percent of world gdp
    SUM(gdp)/SUM(SUM(gdp)) OVER () AS perc_world_gdp    
FROM countries AS c
JOIN
    (SELECT 
     	-- Pull in country_id and height
        country_id, 
        height, 
        -- Number the height of each country's athletes
        ROW_NUMBER() OVER (PARTITION BY country_id ORDER BY height DESC) AS row_num
    FROM winter_games AS w 
    JOIN athletes AS a ON w.athlete_id = a.id
    GROUP BY country_id, height
    -- Alias as subquery
    ORDER BY country_id, height DESC) AS subquery
ON c.id = subquery.country_id
-- Join to country_stats
JOIN country_stats AS cs 
ON cs.country_id = c.id
-- Only include the tallest height for each country
WHERE row_num = 1
GROUP BY region;
