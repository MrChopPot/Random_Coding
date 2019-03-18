# coding = utf-8

from bs4 import BeautifulSoup
from selenium import webdriver
import time
from selenium.webdriver.common.by import By

driver = webdriver.Chrome()

driver.get('http://stats.nba.com/lineups/advanced/#!?Season=2016-17&SeasonType=Regular%20Season')

datasets = []

data = []

for i in range(9):
    soup = BeautifulSoup(driver.page_source, 'html.parser')

    for div in soup.find_all('td', {'class': 'ng-binding'}):
        content = div.text.strip()
        datasets.append(content)

    for a in soup.find_all('a', {'class': 'ng-binding'}):
        content1 = a.text.strip()
        data.append(content1)

    driver.find_element(By.XPATH, "/html/body/main/div[2]/div/div[2]/div/div/nba-stat-table/div[3]/div/div/a[2]").click()
    time.sleep(1)

file = open("temp.csv", "w")
file.write("Team, Man1, Man2, Man3, Man4, Man5, GP, MIN, OFFRTG, DEFRTG, NETRTG, AST%, AST/TO, AST RATIO, OREB%, DREB%, REB%, TO RATIO, EFG%, TS%, PACE, PIE\r\n")

len = len(datasets)
row = 0
j = 94

while (row <= len):
    file.write(data[j] + ',')
    j += 1

    for i in range(17):
        file.write(datasets[i + row] + ',')
    file.write('\r\n')
    row += 17
    i = 1
