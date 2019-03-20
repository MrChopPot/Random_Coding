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

