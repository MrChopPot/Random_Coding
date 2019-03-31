## Window Function

Credit to Youtube vedio: [Window Function Tutorial](https://www.youtube.com/watch?v=H6OTMoXjNiM&list=PLgR-BOYibnN0QqIPFbMlS01bw8x9g07Ll)

1. ... OVER(PARTITION BY XXX [ORDER BY YYY ASC/DESC]) AS ...

   ~~~SQL
   SUM(Line_Total) OVER(PARTITION BY Sales_id) AS Sales_Total
   ~~~


2. ROW_NUMBER(), RANK(), DENSE_RANK(), NTILE()

   ~~~SQL
   ROW_NUMBER() OVER (ORDER BY Sales_Total DESC) AS rownum
   NTILE(3) OVER (ORDER BY Sales_Total DESC) AS ntile
   RANK() OVER (PARTITION BY Sales_Cust_id ORDER BY Sales_Total DESC) AS rank
   ~~~

   * (1,2,3,4,…) | (1,1,3,4,…) | (1,1,2,3,…) | (1,1,2,3)



3. Additional "GROUP BY” in the query also helps

   ~~~SQL
   RANK() OVER (ORDER BY SUM(Sales_Total) DESC) AS rank
   ...
   GROUP BY Sales_Cust_id
   ~~~



4. RANK() VS DENSE_RANK()



5. Running SUM/AVG

   ~~~SQL
   -- Running Sum
   SUM(Sales_Total) OVER (ORDER BY Sales_Date ROWS UNBOUNDED PRECEDING) AS [Running Total]
   /* SUM(Sales_Total) OVER (ORDER BY Sales_Date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS [Running Total] */
   
   -- Just consider 1 line before
   SUM(Sales_Total) OVER (ORDER BY Sales_Date ROWS BETWEEN 1 PRECEDING AND CURRENT ROW) AS [Running Total]
   
   --Running Avg
   CAST (AVG(Sales_Total) OVER (ORDER BY Sales_Date ROWS UNBOUNDED PRECEDING) AS DICIMAL(8, 2)) AS [Running Avg]
   
   --Create windows
   SUM(Sales_Total) OVER (PARTITION BY Sales_Cust_id ORDER BY Sales_Date ROWS UNBOUNDED PRECEDING) AS [Running Total]
   
   CAST (AVG(Sales_Total) OVER (PARTITION BY Sales_Cust_id ORDER BY Sales_Date ROWS UNBOUNDED PRECEDING) AS DICIMAL(8, 2)) AS [Running Avg]
   ~~~


6. LAG() & LEAD()

   ~~~SQL
   --Previous one, 0 for Null
   LAG(Sales_Amount, 1, 0) OVER (PARTITION BY Sales_Cust_id ORDER BY Sales_Date) AS PrevValue
   --Second following one
   LEAD(Sales_Amount, 2) OVER (PARTITION BY Sales_Cust_id ORDER BY Sales_Date) AS NextValue
   ~~~

   * Get the previous one | following one



7. Create Samples of Data

   ~~~SQL
   WITH Number AS
   (SELECT NTILE(100) ... AS N),
   TopCust AS
   (SELECT ... FROM Number GROUP BY N)
   SELECT * FROM TopCust
   ~~~



8. Running Diff

   ~~~SQL
   Sales_Amount - LAG(Sales_Amount) ...
   ~~~



9. Showing grand total with group by

   ~~~SQL
   SUM(SUM(Sales_Amount)) OVER (ORDER BY (SELECT NULL)) AS Grand_Total
   ~~~



10. Rolling Sum

    ~~~SQL
    --The same as before
    SELECT SUM(Sales_Amount) OVER (ORDER BY Date) AS Total
    ...
    ORDER BY Date
    
    --But
    SELECT SUM(Sales_Amount) OVER (ORDER BY Date ROWS BETWEEN 9 PRECEDING AND CURRENT ROW) AS Total
    ...
    ORDER BY Date
    ~~~
