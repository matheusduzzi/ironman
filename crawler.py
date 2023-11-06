from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from time import sleep
import pandas as pd

# Chrome diver
driver = webdriver.Chrome()

# maximizar janela
driver.maximize_window()

# acessar pagina do ironman
driver.get('https://www.coachcox.co.uk/imstats/race/73/results/')
sleep(3)

# Obtain the number of rows in body

table_id = driver.find_element(By.ID, 'imraceresultstable')
rows = table_id.find_elements(By.TAG_NAME, "tr") # get all of the rows in the table

col1 = []
col2 = []
col3 = []
col4 = []
col5 = []
col6 = []
col7 = []
col8 = []
col9 = []
col10 = []
col11= []
col12 = []
col13 = []
col14 = []
col15 = []

for x in range(1, len(rows)):
    col = rows[x].find_elements(By.TAG_NAME, "td")[0]
    s = col.text
    col1 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[1]
    s = col.text
    col2 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[2]
    s = col.text
    col3 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[3]
    s = col.text
    col4 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[4]
    s = col.text
    col5 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[5]
    s = col.text
    col6 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[6]
    s = col.text
    col7 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[7]
    s = col.text
    col8 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[8]
    s = col.text
    col9 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[9]
    s = col.text
    col10 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[10]
    s = col.text
    col11 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[11]
    s = col.text
    col12 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[12]
    s = col.text
    col13 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[13]
    s = col.text
    col14 += [s] 
    
    col = rows[x].find_elements(By.TAG_NAME, "td")[14]
    s = col.text
    col15 += [s] 
    
    
df = pd.DataFrame([col1,col2,col3,col4,col5,col6,col7,col8,col9,col10,col11,col12,col13,col14,col15]).T

df.to_csv("dados_ironman_2011.csv", encoding='utf-8', index=False)