-- 1. Temporal EDA, Variables & Date Manipulation

-- Writing Functions and Stored Procedures in SQL Server

SELECT
  -- Select the date portion of StartDate
  CONVERT(date, StartDate) as StartDate,
  -- Measure how many records exist for each StartDate
  COUNT(StartDate) as CountOfRows 
FROM CapitalBikeShare 
-- Group by the date portion of StartDate
GROUP BY CONVERT(date, StartDate)
-- Sort the results by the date portion of StartDate
ORDER BY CONVERT(date, StartDate);

SELECT
    -- Count the number of IDs
    COUNT(ID) AS Count,
    -- Use DATEPART() to evaluate the SECOND part of StartDate
    "StartDate" = CASE WHEN DATEPART(SECOND, StartDate) = 0 THEN 'SECONDS = 0'
                       WHEN DATEPART(SECOND, StartDate) > 0 THEN 'SECONDS > 0' END
FROM CapitalBikeShare
GROUP BY
    -- Complete the CASE statement
    CASE WHEN DATEPART(SECOND, StartDate) = 0 THEN 'SECONDS = 0'
         WHEN DATEPART(SECOND, StartDate) > 0 THEN 'SECONDS > 0' END

SELECT
    -- Select the day of week value for StartDate
    DATENAME(weekday, StartDate) as DayOfWeek,
    -- Calculate TotalTripHours
    SUM(DATEDIFF(second, StartDate, EndDate))/ 3600 as TotalTripHours 
FROM CapitalBikeShare 
-- Group by the day of week
GROUP BY DATENAME(weekday, StartDate)
-- Order TotalTripHours in descending order
ORDER BY TotalTripHours DESC

SELECT
  -- Calculate TotalRideHours using SUM() and DATEDIFF()
    SUM(DATEDIFF(SECOND, StartDate, EndDate))/ 3600 AS TotalRideHours,
    -- Select the DATE portion of StartDate
    CONVERT(DATE, StartDate) AS DateOnly,
    -- Select the WEEKDAY
    DATENAME(WEEKDAY, CONVERT(DATE, StartDate)) AS DayOfWeek 
FROM CapitalBikeShare
-- Only include Saturday
WHERE DATENAME(WEEKDAY, StartDate) = 'Saturday' 
GROUP BY CONVERT(DATE, StartDate);

-- Create @ShiftStartTime
DECLARE @ShiftStartTime AS time = '08:00 AM'

-- Create @StartDate
DECLARE @StartDate AS date

-- Set StartDate to the first StartDate from CapitalBikeShare
SET 
  @StartDate = (
      SELECT TOP 1 StartDate 
      FROM CapitalBikeShare 
      ORDER BY StartDate ASC
    )

-- Create ShiftStartDateTime
DECLARE @ShiftStartDateTime AS datetime

-- Cast StartDate and ShiftStartTime to datetime data types
SET @ShiftStartDateTime = CAST(@StartDate AS datetime) + CAST(@ShiftStartTime AS datetime) 

SELECT @ShiftStartDateTime

-- Create @Shifts
DECLARE @Shifts TABLE(
    -- Create StartDateTime column
  StartDateTime DATETIME,
    -- Create EndDateTime column
  EndDateTime DATETIME)
-- Populate @Shifts
INSERT INTO @Shifts (StartDateTime, EndDateTime)
  SELECT '3/1/2018 8:00 AM', '3/1/2018 4:00 PM'
SELECT * 
FROM @Shifts

-- Create @RideDates
DECLARE @RideDates TABLE(
    -- Create RideStart
  RideStart DATE,
    -- Create RideEnd
  RideEnd DATE)
-- Populate @RideDates
INSERT INTO @RideDates(RideStart, RideEnd)
-- Select the unique date values of StartDate and EndDate
SELECT DISTINCT
    -- Cast StartDate as date
  CAST(StartDate AS date),
    -- Cast EndDate as date
  CAST(EndDate AS date) 
FROM CapitalBikeShare 
SELECT * 
FROM @RideDates;

-- Find the first day of the current month
SELECT DATEADD(month, DATEDIFF(month, 0, GETDATE()), 0)

-----------------------------
-- 2. User Defined Functions

-- Create GetYesterday()
CREATE FUNCTION GetYesterday()
-- Specify return data type
RETURNS date
AS
BEGIN
-- Calculate yesterday's date value
RETURN (SELECT DATEADD(day, -1, GETDATE()))
END 

-- Create SumRideHrsSingleDay
CREATE FUNCTION SumRideHrsSingleDay (@DateParm date)
-- Specify return data type
RETURNS numeric
AS
-- Begin
BEGIN
RETURN
-- Add the difference between StartDate and EndDate
(SELECT SUM(DATEDIFF(second, StartDate, EndDate))/3600
FROM CapitalBikeShare
 -- Only include transactions where StartDate = @DateParm
WHERE CAST(StartDate AS date) = @DateParm)
-- End
END

-- Create the function
CREATE FUNCTION SumRideHrsDateRange (@StartDateParm datetime, @EndDateParm datetime)
-- Specify return data type
RETURNS numeric
AS
BEGIN
RETURN
-- Sum the difference between StartDate and EndDate
(SELECT SUM(DATEDIFF(second, StartDate, EndDate))/3600
FROM CapitalBikeShare
-- Include only the relevant transactions
WHERE StartDate > @StartDateParm and StartDate < @EndDateParm)
END

-- Create the function
CREATE FUNCTION SumStationStats(@StartDate AS datetime)
-- Specify return data type
RETURNS TABLE
AS
RETURN
SELECT
  StartStation,
    -- Use COUNT() to select RideCount
  COUNT(ID) AS RideCount,
    -- Use SUM() to calculate TotalDuration
    SUM(DURATION) AS TotalDuration
FROM CapitalBikeShare
WHERE CAST(StartDate as Date) = @StartDate
-- Group by StartStation
GROUP BY StartStation;

-- Create the function
CREATE FUNCTION CountTripAvgDuration (@Month CHAR(2), @Year CHAR(4))
-- Specify return variable
RETURNS @DailyTripStats TABLE(
  TripDate  date,
  TripCount int,
  AvgDuration numeric)
AS
BEGIN
-- Insert query results into @DailyTripStats
INSERT @DailyTripStats
SELECT
    -- Cast StartDate as a date
  CAST(StartDate AS date),
    COUNT(ID),
    AVG(Duration)
FROM CapitalBikeShare
WHERE
  DATEPART(month, StartDate) = @Month AND
    DATEPART(year, StartDate) = @Year
-- Group by StartDate as a date
GROUP BY CAST(StartDate AS date)
-- Return
RETURN
END

-- Create @BeginDate
DECLARE @BeginDate AS date = '3/1/2018'
-- Create @EndDate
DECLARE @EndDate AS date = '3/10/2018' 
SELECT
  -- Select @BeginDate
  @BeginDate AS BeginDate,
  -- Select @EndDate
  @EndDate AS EndDate,
  -- Execute SumRideHrsDateRange()
  dbo.SumRideHrsDateRange(@BeginDate, @EndDate) AS TotalRideHrs

-- Create @RideHrs
DECLARE @RideHrs AS numeric
-- Execute SumRideHrsSingleDay() and store the result in @RideHrs
EXEC @RideHrs = dbo.SumRideHrsSingleDay @DateParm = '3/5/2018' 
SELECT 
  'Total Ride Hours for 3/5/2018:', 
  @RideHrs

-- Create @StationStats
DECLARE @StationStats TABLE(
  StartStation nvarchar(100), 
  RideCount int, 
  TotalDuration numeric)
-- Populate @StationStats with the results of the function
INSERT INTO @StationStats 
SELECT TOP 10 *
-- Execute SumStationStats with 3/15/2018
FROM dbo.SumStationStats('3/15/2018') 
ORDER BY RideCount DESC
-- Select all the records from @StationStats
SELECT * 
FROM @StationStats

-- Update SumStationStats
CREATE OR ALTER FUNCTION dbo.SumStationStats(@EndDate AS date)
-- Enable SCHEMABINDING
RETURNS TABLE WITH SCHEMABINDING
AS
RETURN
SELECT
  StartStation,
    COUNT(ID) AS RideCount,
    SUM(DURATION) AS TotalDuration
FROM dbo.CapitalBikeShare
-- Cast EndDate as date and compare to @EndDate
WHERE CAST(EndDate AS Date) = @EndDate
GROUP BY StartStation;

-----------------------------
-- 3. Stored Procedures

-- Create the stored procedure
CREATE PROCEDURE dbo.cuspSumRideHrsSingleDay
    -- Declare the input parameter
  @DateParm date,
    -- Declare the output parameter
  @RideHrsOut numeric OUTPUT
AS
-- Don't send the row count 
SET NOCOUNT ON
BEGIN
-- Assign the query result to @RideHrsOut
SELECT
  @RideHrsOut = SUM(DATEDIFF(second, StartDate, EndDate))/3600
FROM CapitalBikeShare
-- Cast StartDate as date and compare with @DateParm
WHERE CAST(StartDate AS date) = @DateParm
RETURN
END

-- Create the stored procedure
CREATE PROCEDURE dbo.cusp_RideSummaryCreate (@DateParm date)
AS
BEGIN
SET NOCOUNT ON
-- Insert into the Date and RideHours columns
INSERT INTO dbo.RideSummary(Date, RideHours)
-- Insert @DateParm and the result of SumRideHrsSingleDay
VALUES(@DateParm, dbo.SumRideHrsSingleDay(@DateParm)) 

-- Select the record that was just inserted
SELECT
    -- Select Date column
  Date,
    -- Select RideHours column
    RideHours
FROM dbo.RideSummary
-- Check whether Date equals @DateParm
WHERE Date = @DateParm
END;

-- Create the stored procedure
CREATE PROCEDURE dbo.cuspRideSummaryUpdate
  -- Specify @Date input parameter
  (@Date date,
     -- Specify @RideHrs input parameter
     @RideHrs numeric(18,0))
AS
BEGIN
SET NOCOUNT ON
-- Update RideSummary
UPDATE RideSummary
-- Set
SET
  Date = @Date,
    RideHours = @RideHrs
-- Include records where Date equals @Date
WHERE Date = @Date
END;

-- Create the stored procedure
CREATE PROCEDURE dbo.cuspRideSummaryDelete
  -- Specify @DateParm input parameter
  (@DateParm date,
     -- Specify @RowCountOut output parameter
     @RowCountOut int OUTPUT)
AS
BEGIN
-- Delete record(s) where Date equals @DateParm
DELETE FROM dbo.RideSummary
WHERE Date = @DateParm
-- Set @RowCountOut to @@ROWCOUNT
SET @RowCountOut = @@ROWCOUNT
END;

-- Create @RideHrs
Declare @RideHrs AS numeric(18,0)
-- Execute the stored procedure
EXEC dbo.cuspSumRideHrsSingleDay
    -- Pass the input parameter
  @DateParm = '3/1/2018',
    -- Store the output in RideHrsOut
  @RideHrsOut = @RideHrs OUTPUT
-- Select RideHrs
SELECT @RideHrs AS RideHours

-- Create @ReturnStatus
DECLARE @ReturnStatus AS int
-- Execute the SP, storing the result in @ReturnStatus
EXEC @ReturnStatus = dbo.cuspRideSummaryUpdate
    -- Specify @DateParm
  @DateParm = '3/1/2018',
    -- Specify @RideHrs
  @RideHrs = 300

-- Select the columns of interest
SELECT
  @ReturnStatus AS ReturnStatus,
    Date,
    RideHours
FROM dbo.RideSummary
WHERE Date = '3/1/2018';

-- Create @ReturnStatus
DECLARE @ReturnStatus AS INT
-- Create @RowCount
DECLARE @RowCount AS INT

-- Execute the SP, storing the result in @ReturnStatus
EXEC @ReturnStatus = dbo.cuspRideSummaryDelete
    -- Specify @DateParm
  @DateParm = '3/1/2018',
    -- Specify RowCountOut
  @RowCountOut = @RowCount OUTPUT

-- Select the columns of interest
SELECT
  @ReturnStatus AS ReturnStatus,
    @RowCount AS 'RowCount';

-- Alter the stored procedure
CREATE OR ALTER PROCEDURE dbo.cuspRideSummaryDelete
  -- (Incorrectly) specify @DateParm
  @DateParm nvarchar(30),
    -- Specify @Error
  @Error nvarchar(max) = NULL OUTPUT
AS
SET NOCOUNT ON
BEGIN
  -- Start of the TRY block
  BEGIN TRY
      -- Delete
      DELETE FROM RideSummary
      WHERE Date = @DateParm
  -- End of the TRY block
  END TRY
  -- Start of the CATCH block
  BEGIN CATCH
    SET @Error = 
    'Error_Number: '+ CAST(ERROR_NUMBER() AS VARCHAR) +
    'Error_Severity: '+ CAST(ERROR_SEVERITY() AS VARCHAR) +
    'Error_State: ' + CAST(ERROR_STATE() AS VARCHAR) + 
    'Error_Message: ' + ERROR_MESSAGE() + 
    'Error_Line: ' + CAST(ERROR_LINE() AS VARCHAR)
  -- End of the CATCH block
  END CATCH
END;

-- Create @ReturnCode
DECLARE @ReturnCode INT
-- Create @ErrorOut
DECLARE @ErrorOut nvarchar(max)
-- Execute the SP, storing the result in @ReturnCode
EXEC @ReturnCode = dbo.cuspRideSummaryDelete
    -- Specify @DateParm
  @DateParm = '1/32/2018',
    -- Assign @ErrorOut to @Error
    @Error = @ErrorOut OUTPUT
-- Select @ReturnCode and @ErrorOut
SELECT
  @ReturnCode AS ReturnCode,
    @ErrorOut AS ErrorMessage;

--------------------------
-- 4. NYC Taxi Ride Case Study

SELECT
  -- PickupDate is after today
  COUNT (CASE WHEN PickupDate > DropOffDate THEN 1 END) AS 'FuturePickup',
    -- DropOffDate is after today
  COUNT (CASE WHEN DropOffDate > GETDATE() THEN 1 END) AS 'FutureDropOff',
    -- PickupDate is after DropOffDate
  COUNT (CASE WHEN PickupDate > GETDATE() THEN 1 END) AS 'PickupBeforeDropoff',
    -- TripDistance is 0
  COUNT (CASE WHEN TripDistance = 0 THEN 1 END) AS 'ZeroTripDistance'  
FROM YellowTripData;

-- Create the stored procedure
CREATE PROCEDURE dbo.cuspImputeTripDistanceMean
AS
BEGIN
-- Specify @AvgTripDistance variable
DECLARE @AvgTripDistance AS numeric (18,4)

-- Calculate the average trip distance
SELECT @AvgTripDistance = AVG(TripDistance) 
FROM YellowTripData
-- Only include trip distances greater than 0
WHERE TripDistance > 0

-- Update the records where trip distance is 0
UPDATE YellowTripData
SET TripDistance =  @AvgTripDistance
WHERE TripDistance = 0
END;

-- Create the function
CREATE FUNCTION dbo.GetTripDistanceHotDeck()
-- Specify return data type
RETURNS numeric(18,4)
AS 
BEGIN
RETURN
  -- Select the first TripDistance value
  (SELECT TOP 1 TripDistance
  FROM YellowTripData
    -- Sample 1000 records
  TABLESAMPLE(1000 rows)
    -- Only include records where TripDistance is > 0
  WHERE TripDistance > 0)
END;

-- Create the function
CREATE FUNCTION dbo.ConvertMileToKm (@Miles numeric(18,2))
-- Specify return data type
RETURNS numeric(18,2)
AS
BEGIN
RETURN
  -- Convert Miles to Kilometers
  (SELECT @Miles * 1.609)
END;

-- Create the function
CREATE FUNCTION dbo.ConvertDollar
  -- Specify @DollarAmt parameter
  (@DollarAmt numeric(18,2),
     -- Specify ExchangeRate parameter
     @ExchangeRate numeric(18,2))
-- Specify return data type
RETURNS numeric(18,2)
AS
BEGIN
RETURN
  -- Multiply @ExchangeRate and @DollarAmt
  (SELECT @ExchangeRate * @DollarAmt)
END;

-- Create the function
CREATE FUNCTION dbo.GetShiftNumber (@Hour integer)
-- Specify return data type
RETURNS int
AS
BEGIN
RETURN
  -- 12am (0) to 9am (9) shift
  (CASE WHEN @Hour >= 0 AND @Hour < 9 THEN 1
        -- 9am (9) to 5pm (17) shift
     WHEN @Hour >= 9 AND @Hour < 17 THEN 2
          -- 5pm (17) to 12am (24) shift
       WHEN @Hour >= 17 AND @Hour < 24 THEN 3 END)
END;

SELECT
  -- Select the first 100 records of PickupDate
  TOP 100 PickupDate,
    -- Determine the shift value of PickupDate
  dbo.GetShiftNumber(DATEPART(hour, PickupDate)) AS 'Shift',
    -- Select FareAmount
  FareAmount,
    -- Convert FareAmount to Euro
  dbo.ConvertDollar(FareAmount, 0.87) AS 'FareinEuro',
    -- Select TripDistance
  TripDistance,
    -- Convert TripDistance to kilometers
  dbo.ConvertMileToKm(TripDistance) AS 'TripDistanceinKM'
FROM YellowTripData
-- Only include records for the 2nd shift
WHERE dbo.GetShiftNumber(DATEPART(hour, PickupDate)) = 2;

SELECT
    -- Select the pickup day of week
  DATENAME(weekday, PickupDate) as DayofWeek,
    -- Calculate TotalAmount per TripDistance
  CAST(AVG(TotalAmount/
            -- Select TripDistance if it's more than 0
      CASE WHEN TripDistance > 0 THEN TripDistance
                 -- Use GetTripDistanceHotDeck()
           ELSE dbo.GetTripDistanceHotDeck() END) as decimal(10,2)) as 'AvgFare'
FROM YellowTripData
GROUP BY DATENAME(weekday, PickupDate)
-- Order by the PickupDate day of week
ORDER BY
     CASE WHEN DATENAME(weekday, PickupDate) = 'Monday' THEN 1
          WHEN DATENAME(weekday, PickupDate) = 'Tuesday' THEN 2
          WHEN DATENAME(weekday, PickupDate) = 'Wednesday' THEN 3
          WHEN DATENAME(weekday, PickupDate) = 'Thursday' THEN 4
          WHEN DATENAME(weekday, PickupDate) = 'Friday' THEN 5
          WHEN DATENAME(weekday, PickupDate) = 'Saturday' THEN 6
          WHEN DATENAME(weekday, PickupDate) = 'Sunday' THEN 7
END ASC;

CREATE OR ALTER PROCEDURE dbo.cuspBoroughRideStats
AS
BEGIN
SELECT
    -- Calculate the pickup weekday
  DATENAME(weekday, PickupDate) AS 'Weekday',
    -- Select the Borough
  Zone.Borough AS 'PickupBorough',
    -- Display AvgFarePerKM as German currency
  FORMAT(AVG(dbo.ConvertDollar(TotalAmount, .88)/dbo.ConvertMiletoKM(TripDistance)), 'c', 'de-de') AS 'AvgFarePerKM',
    -- Display RideCount in the German format
  FORMAT(COUNT(ID), 'n', 'de-de') AS 'RideCount',
    -- Display TotalRideMin in the German format
  FORMAT(SUM(DATEDIFF(SECOND, PickupDate, DropOffDate))/60, 'n', 'de-de') AS 'TotalRideMin'
FROM YellowTripData
INNER JOIN TaxiZoneLookup AS Zone 
ON PULocationID = Zone.LocationID
-- Only include records where TripDistance is greater than 0
WHERE  TripDistance > 0
-- Group by pickup weekday and Borough
GROUP BY DATENAME(WEEKDAY, PickupDate), Zone.Borough
ORDER BY CASE WHEN DATENAME(WEEKDAY, PickupDate) = 'Monday' THEN 1
          WHEN DATENAME(WEEKDAY, PickupDate) = 'Tuesday' THEN 2
              WHEN DATENAME(WEEKDAY, PickupDate) = 'Wednesday' THEN 3
              WHEN DATENAME(WEEKDAY, PickupDate) = 'Thursday' THEN 4
              WHEN DATENAME(WEEKDAY, PickupDate) = 'Friday' THEN 5
              WHEN DATENAME(WEEKDAY, PickupDate) = 'Saturday' THEN 6
              WHEN DATENAME(WEEKDAY, PickupDate) = 'Sunday' THEN 7 END,  
     SUM(DATEDIFF(SECOND, PickupDate, DropOffDate))/60
DESC
END;

-- Create SPResults
DECLARE @SPResults TABLE(
    -- Create Weekday
  Weekday nvarchar(30),
    -- Create Borough
  Borough nvarchar(30),
    -- Create AvgFarePerKM
  AvgFarePerKM nvarchar(30),
    -- Create RideCount
  RideCount nvarchar(30),
    -- Create TotalRideMin
  TotalRideMin nvarchar(30))

-- Insert the results into @SPResults
INSERT INTO @SPResults
-- Execute the SP
EXEC dbo.cuspBoroughRideStats

-- Select all the records from @SPresults 
SELECT *
FROM @SPresults;

-- Create the stored procedure
CREATE PROCEDURE dbo.cuspPickupZoneShiftStats
  -- Specify @Borough parameter
  @Borough nvarchar(30)
AS
BEGIN
SELECT
  DATENAME(WEEKDAY, PickupDate) as 'Weekday',
    -- Calculate the shift number
  dbo.GetShiftNumber(DATEPART(hour, PickupDate)) as 'Shift',
  Zone.Zone as 'Zone',
  FORMAT(AVG(dbo.ConvertDollar(TotalAmount, .77)/dbo.ConvertMiletoKM(TripDistance)), 'c', 'de-de') AS 'AvgFarePerKM',
  FORMAT(COUNT (ID),'n', 'de-de') as 'RideCount',
  FORMAT(SUM(DATEDIFF(SECOND, PickupDate, DropOffDate))/60, 'n', 'de-de') as 'TotalRideMin'
FROM YellowTripData
INNER JOIN TaxiZoneLookup as Zone on PULocationID = Zone.LocationID 
WHERE
  dbo.ConvertMiletoKM(TripDistance) > 0 AND
  Zone.Borough = @Borough
GROUP BY
  DATENAME(WEEKDAY, PickupDate),
    -- Group by shift
  dbo.GetShiftNumber(DATEPART(hour, PickupDate)),  
  Zone.Zone
ORDER BY CASE WHEN DATENAME(WEEKDAY, PickupDate) = 'Monday' THEN 1
              WHEN DATENAME(WEEKDAY, PickupDate) = 'Tuesday' THEN 2
              WHEN DATENAME(WEEKDAY, PickupDate) = 'Wednesday' THEN 3
              WHEN DATENAME(WEEKDAY, PickupDate) = 'Thursday' THEN 4
              WHEN DATENAME(WEEKDAY, PickupDate) = 'Friday' THEN 5
              WHEN DATENAME(WEEKDAY, PickupDate) = 'Saturday' THEN 6
              WHEN DATENAME(WEEKDAY, PickupDate) = 'Sunday' THEN 7 END,
         -- Order by shift
         dbo.GetShiftNumber(DATEPART(hour, PickupDate)),
         SUM(DATEDIFF(SECOND, PickupDate, DropOffDate))/60 DESC
END;

-- Create @Borough
DECLARE @Borough AS nvarchar(30) = 'Manhattan'
-- Execute the SP
EXEC dbo.cuspPickupZoneShiftStats
    -- Pass @Borough
  @Borough = @Borough;
