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

